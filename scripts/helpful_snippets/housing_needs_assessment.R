pacman::p_load(tidyverse, tidycensus, sf, mapview, data.table, tigris, purrr, srvyr, ipumsr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

options(stringsAsFactors = F, #String Variables are brought in as charactors
        scipen = 999, #Remove Scientific Notation
        digits = 4, #Set data precision for readability
        tigris_class = "sf", #Call in TIGER spatial objects as simple features
        tigris_use_cache = T,
        dplyr.width = Inf,
        survey.replicates.mse = T,
        datatable.fread.datatable = F,
        survey.lonely.psu="certainty" # Adjust lonely primary sampling units
)

#Clear environment and remove objects
rm(list=ls())
#Garbage Collector: Return memory to your computer
gc()


#### Helper files
puma_county_xwalk <- "covid-rental-assistance/data/geocorr_puma2010_county2010.csv" %>% 
  read_csv(col_names = c("state", "puma", "county", "afact"), col_types = "cdd____d", skip = 2) %>% 
  mutate(county = as.integer(str_sub(county, -3)), # county_geoid = ifelse(length(as.character(county))<3, paste0(state,"0",str_sub(county, -3)), paste0(state,str_sub(county, -3))),
         state = as.integer(state)) %>%
  arrange(state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE) %>%
  mutate(pumayear = 2012)

puma_county_xwalk00 <- "covid-rental-assistance/data/puma00_to_county_geocorr2014.csv" %>% 
  read_csv(col_names = c("state", "puma", "county", "afact"), col_types = "cdd____d", skip = 2) %>% 
  mutate(county = as.integer(str_sub(county, -3)), # county_geoid = ifelse(length(as.character(county))<3, paste0(state,"0",str_sub(county, -3)), paste0(state,str_sub(county, -3))),
         state = as.integer(state)) %>%
  arrange(state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE) %>%
  mutate(pumayear = 2000)

puma_county_xwalk <- rbind(puma_county_xwalk, puma_county_xwalk00) %>% 
  arrange(desc(pumayear), state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE)
# 
# hud_ami <- read_csv("covid-rental-assistance/data/hud-2018-ami.csv", col_types = "ddddd") %>% 
#   filter(persons == 4) %>% 
#   select(-hud_inc_lim80, -persons) %>% 
#   rename(hud_med_inc4 = hud_med_inc)

hud_ami <- read_csv("limits/limits_2000t2020.csv", col_types = "cddcccdd") %>% 
  janitor::clean_names() %>%
  select(stfips, cntyfips, hamfi = mfi, year) %>%
  distinct(stfips, cntyfips, hamfi, year, .keep_all = T)


raw_pums_vacancy <- read_ipums_micro(read_ipums_ddi("usa_00110.xml")) %>% janitor::clean_names()

# raw_pums_vacancy <- read.delim(file = "usa_00110.dat")

vacant_units <- raw_pums_vacancy %>%
  select(year:cbserial, hhwt, cluster:gq, rent, valueh:bedrooms) %>% 
  mutate_at(vars(met2013, gq, vacancy, kitchen, sink, stove, plumbing), haven::as_factor) %>%
  mutate_at(vars(year, hhwt, statefip, puma, rent, valueh, rooms:bedrooms), as.numeric) %>%
  left_join(., puma_county_xwalk, by = c("statefip" = "state", "puma")) %>%
  left_join(., hud_ami, by = c("statefip" = "stfips", "county" = "cntyfips", "year")) %>%
  
  mutate(valueh = as.numeric(valueh) %>% na_if(9999999),
         max_persons = ceiling(rooms * 1.5),
         mfi_coefficient = case_when(max_persons == 1 ~ 0.7,
                                     max_persons == 2 ~ 0.8,
                                     max_persons == 3 ~ 0.9,
                                     max_persons == 4 ~ 1,
                                     max_persons > 4 ~ (max_persons-4)*0.08+1),
         adj_hamfi = mfi_coefficient * hamfi,
         is_rental = rent > 0,
         rent_hamfi_grp = case_when((rent * 12) / (adj_hamfi * .3) <= .3 & is_rental ~ "0-30% MFI",
                                    (rent * 12) / (adj_hamfi * .5) <= .3 & is_rental ~ "30-50% MFI",
                                    (rent * 12) / (adj_hamfi * .8) <= .3 & is_rental ~ "50-80% MFI",
                                    (rent * 12) / (adj_hamfi * 1) <= .3 & is_rental ~ "80-100% MFI",
                                    (rent * 12) / (adj_hamfi * 1.2) <= .3 & is_rental ~ "100-120% MFI",
                                    (rent * 12) / (adj_hamfi * 1.4) <= .3 & is_rental ~ "120-140% MFI",
                                    TRUE ~ ">140% MFI"),
         
         # rent_grp = if_else(is_rental, cut(
         #   rent, c(-Inf, 1, 400, 800, 1200, 1600, 2000, 2400, 3000, 4000, Inf),
         #   c("No cash rent", "< $400", "$400 - $800", "$800 - $1,200", "$1,200 - $1,600", "$1,600 - $2,000",
         #     "$2,000 - $2,400", "$2,400 - $3,000", "$3,000 - $4,000", ">= $4,000")), as.factor(NA_complex_))
  )



test <- vacant_units %>%
  select(year, statefip, puma, rent_hamfi_grp, hhwt, cluster, strata, is_rental) %>%
  filter(is_rental) %>%
  mutate_at(vars(year:rent_hamfi_grp), as.factor) %>%
  srvyr::as_survey_design(
    ids = cluster,
    strata = strata,
    weights = hhwt
  ) %>%
  group_by(year, statefip, puma, rent_hamfi_grp) %>%
  summarize(count = unweighted(n()),
            units = survey_total(na.rm = T))



raw_pums_hna <- read_ipums_micro(read_ipums_ddi("usa_00108.xml")) %>% janitor::clean_names()

pums_hna <- raw_pums_hna %>%
  left_join(., puma_county_xwalk, by = c("statefip" = "state", "puma")) %>%
  left_join(., hud_ami, by = c("statefip" = "stfips", "county" = "cntyfips", "year")) %>%
  mutate_at(vars(met2013, gq:ownershpd, lingisol:unitsstr, sex, marst:citizen, language:ind, diffrem:diffhear), haven::as_factor) #%>%
  # mutate_at(vars(year, hhwt, statefip, puma, rent, valueh, rooms:bedrooms), as.numeric) %>%
pums_hna <- pums_hna %>% 
  mutate(mfi_coefficient = case_when(numprec == 1 ~ 0.7,
                                     numprec == 2 ~ 0.8,
                                     numprec == 3 ~ 0.9,
                                     numprec == 4 ~ 1,
                                     numprec > 4 ~ (numprec-4)*0.08+1),
         adj_hamfi = mfi_coefficient * hamfi,
         inc = hhincome,
         hud_limit = hhincome / adj_hamfi,
         is_low_income = hud_limit < 0.8,
         rachisp = if_else(hispan == "Not Hispanic", "No", "Yes"),
         racwhtnh = if_else(rachisp == "No" & race == "White", "Yes", "No"),
         racbipoc = if_else(racwhtnh == "No", "Yes", "No"),
         race1_rec = case_when(rachisp == "Yes" ~ "Hispanic",
                               race == "White" ~ "White",
                               race == "Black/African American/Negro" ~ "Black",
                               race == "American Indian or Alaska Native" ~ "Native American",
                               race %in% c("Chinese", "Japanese", "Other Asian or Pacific Islander") & racasian == "Yes" ~ "Asian",
                               race == "Other Asian or Pacific Islander" & racpacis == "Yes" ~ "Pacific Islander",
                               race == "Other race, nec" ~ "Another race",
                               race %in% c("Two major races", "Three or more major races") ~ "Multi-racial",
                               T ~ NA_character_))

pums_hna %>%
  filter(puma %in% c(1301, 1302, 1303, 1304, 1314, 1305, 1306),
         pernum == 1) %>%
  group_by(year, racbipoc, is_low_income, ownershp, unitsstr) %>%
  summarize(hh = sum(hhwt)) %>% clipr::write_clip()


  
  