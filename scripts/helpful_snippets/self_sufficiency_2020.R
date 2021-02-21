##### Load libraries, set working directory #####
pacman::p_load(tidyverse, data.table, srvyr, ipumsr)

options(
  scipen = 999, # remove scientific notation
  digits = 4, # set data precision for readability
  stringsAsFactors = F, # string variables are brought in as charactors
  dplyr.width = Inf,
  survey.replicates.mse = T,
  datatable.fread.datatable = F # <- is this necessary?
)

rm(list=ls())
gc()

## Set working directory
# setwd("N:/work/district_planning/Economic Development/NICK/PUMS/IPUMS/sss_2020/")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Import FIPS codes
stfips <- rio::import("fips.xlsx", which = "state")
cntyfips <- rio::import("fips.xlsx", which = "county")

## Import CPI (Census Research Series)
cpi <- rio::import("cpi-u-rs_1950-current.xlsx") %>%
  select(year, inflation_factor_2018)

## Import CPI (BLS)
# cpi <- rio::import("N:/work/district_planning/Economic Development/NICK/Resources/CPI-U-West_BLS.xlsx", which = "annual") %>%
#   select(year, inflation_factor_2018)

### Import the 2017 self-sufficiency standard 
sssor <- rio::import("OR2017_all_families.xlsx", which = "By Family") %>% select(`Family Type`, State, Year, County, `Annual Self-Sufficiency Wage`)
ssswa <- rio::import("WA2017_all_families.xlsx", which = "By Family") %>% select(`Family Type`, State, Year, County, `Annual Self-Sufficiency Wage`)
sss <- rbind(sssor, ssswa) %>%
  rename(family_type = `Family Type`, sss_wage = `Annual Self-Sufficiency Wage`); rm(sssor, ssswa)
names(sss) <- tolower(names(sss))
# Filter for Portland MSA and join FIPS codes and adjust for inflation
sss <- sss %>%
  filter((county %in% c("Clark County", "Skamania County") & state == "WA") | 
           (county %in% c("Clackamas County", "Columbia County", "Multnomah County", "Washington County", "Yamhill County") & state == "OR")) %>%
  left_join(., stfips, by = "state") %>%
  left_join(., cntyfips, by = "county") %>%
  left_join(., cpi, by = "year") %>%
  mutate(sss_wage_adj = sss_wage * inflation_factor_2018) %>%
  select(-inflation_factor_2018)


## Import poverty thressholds and adjust for inflation
poverty <- rio::import("poverty_thresholds.xlsx", which = "all_years") %>%
  left_join(., cpi, by = "year") %>%
  mutate(poverty_threshold_adj = threshold * inflation_factor_2018)

## Variables to assist in coding
pdxpumas <- c(1314, 1301, 1305, 1303, 1302) # east county: 1316

# Which Group Quarters to include
gq_filter <- c("Households under 1970 definition", "Additional households under 1990 definition",
               "Additional households under 2000 definition")

# Educational attainment
edu_lths <- c("Nursery school to grade 4", "Nursery school, preschool", "Kindergarten", "Grade 1, 2, 3, or 4", "Grade 1", "Grade 2", "Grade 3", 
              "Grade 4", "Grade 5, 6, 7, or 8", "Grade 5 or 6", "Grade 5", "Grade 6", "Grade 7 or 8", "Grade 7", "Grade 8", "Grade 9", "Grade 10", 
              "Grade 11", "Grade 12", "12th grade, no diploma", "No schooling completed")

edu_hs <- c("High school graduate or GED", "Regular high school diploma", "GED or alternative credential")

edu_sc <- c("Some college, but less than 1 year", "1 year of college", "1 or more years of college credit, no degree", "2 years of college", 
            "Associate's degree, type not specified", "Associate's degree, occupational program", "Associate's degree, academic program", 
            "3 years of college", "4 years of college")

edu_ba <- c("Bachelor's degree", "5+ years of college", "6 years of college (6+ in 1960-1970)", "7 years of college", "8+ years of college", 
            "Master's degree", "Professional degree beyond a bachelor's degree", "Doctoral degree")



#LANGUAGE and LANGUAGED
ee_slavic_lang = c('Rumanian', 'Albanian', 'Russian', 'Russian, Great Russian', 'Bielo-, White Russian', 'Ukrainian, Ruthenian, Little Russian', 'Ruthenian', 
                   'Little Russian', 'Ukrainian', 'Czech', 'Bohemian', 'Moravian', 'Polish', 'Kashubian, Slovincian', 'Slovak', 'Serbo-Croatian, Yugoslavian, Slavonian', 
                   'Croatian', 'Serbian', 'Dalmatian', 'Montenegrin', 'Slovene', 'Lithuanian', 'Lettish, Latvian', 'Other Balto-Slavic', 'Bulgarian', 
                   'Lusatian, Sorbian, Wendish', 'Wendish', 'Macedonian', 'Slavic unknown', 'Armenian', 'Romany, Gypsy', 'Gypsy', 'Finnish', 'Magyar, Hungarian', 
                   'Magyar', 'Hungarian', 'Uralic', 'Estonian, Ingrian, Livonian, Vepsian, Votic', 'Lapp, Inari, Kola, Lule, Pite, Ruija, Skolt, Ume', 'Other Uralic', 
                   'Other Altaic', 'Chuvash', 'Karakalpak', 'Kazakh', 'Kirghiz', 'Karachay, Tatar, Balkar, Bashkir, Kumyk', 'Uzbek, Uighur', 'Azerbaijani', 'Turkmen', 
                   'Yakut', 'Caucasian, Georgian, Avar', 'Georgian', 'Lappish', 'Estonian', 'Dalmatian, Montenegrin', 'Great Russian', 'Bosnian',
                   'Rumanian', 'Albanian', 'Russian', 'Ukrainian, Ruthenian, Little Russian', 'Czech', 'Polish', 'Slovak', 'Serbo-Croatian, Yugoslavian, Slavonian', 
                   'Slovene', 'Lithuanian', 'Other Balto-Slavic', 'Slavic unknown', 'Armenian', 'Romany, Gypsy', 'Finnish', 'Magyar, Hungarian')

ee_slavic_bpl = c('Turkmenistan', 'Tadzhik', 'Kirghizia', 'Kazakhstan', 'Republic of Georgia', 'Azerbaijan', 'Armenia', 'Ukraine', 'Bessarabia', 
                  'Moldavia', 'Byelorussia', 'Other USSR/Russia', 'Baltic States, ns', 'Lithuania', 'Latvia', 'Estonia', 'Eastern Europe, ns', 
                  'Central Europe, ns', 'Kosovo', 'Slovenia', 'Carniola', 'Slovonia', 'Dalmatia', 'Bosnia', 'Serbia', 'Montenegro', 'Croatia', 
                  'Yugoslavia', 'Transylvania', 'Romania', 'Russian Poland', 'West Prussia', 'Silesia', 'Prussian Poland', 'Posen', 'Pomerania', 
                  'East Prussia', 'German Poland', 'Galicia', 'Austrian Poland', 'Poland', 'Hungary', 'Finland', 'Lapland, ns', 'Svalbard and Jan Meyen', 
                  'Svalbard', 'Albania', 'Bulgaria', 'Czechoslovakia', 'Bohemia', 'Bohemia-Moravia', 'Slovakia', 'Czech Republic', 'East Berlin', 
                  'East Germany', 'USSR, ns', 'Siberia', 'Uzbekistan')

ee_slavic_ancest = c('Armenian', 'Eastern European, nec', 'Central European, nec', 'Slavonian', 'Slav', 'Yugoslavian', 'Windish', 'Husel', 'Bioko', 
                     'Lemko', 'Ruthenian (1990-2000)', 'Ruthenian (1980)', 'Ukrainian (1990-2000, ACS, PRCS)', 'Ukrainian (1980)', 'Uzbek', 
                     'Tadzhik (1980, 2000)', 'Soviet Central Asia (1990-2000)', 'Tuvinian (1990-2000)', 'Crimean (1980)', 'Tartar (1980)', 
                     'Tatar (1990-2000)', 'Soviet Union, nec', 'Yakut', 'Mesknetian (1990-2000)', 'Gagauz (1990-2000)', 'Chevash', 'Bashkir', 
                     'Soviet Turkic (1990-2000)', 'Sorb/Wend', 'Slovene', 'Slovak', 'Montenegrin (1990-2000, 2012 ACS)', 
                     'Bosnian (1990) Herzegovinian (2000, ACS, PRCS)', 'Serbian (1990-2000, ACS, PRCS)', 'Serbian (1980)', 'Muscovite', 'Russian', 
                     'Wallachian', 'Moldavian', 'Bucovina', 'Bessarabian (1990-2000)', 'Bessarabian (1980)', 'Transylvanian', 'Rumanian (1980)', 
                     'Romanian (1990-2000, ACS, PRCS)', 'Kashubian', 'Polish', 'Ossetian', 'North Caucasian Turkic (1990-2000)', 'North Caucasian', 
                     'Macedonian', 'Lithuanian', 'Latvian', 'Magyar', 'Hungarian', 'Rom', 'Gruziia (1990-2000)', 
                     'German from Russia (1990-2000); German Russian (ACS, PRCS)', 'Volga', 'Germans from Russia', 'Georgian', 'Voytak', 'Mordovian', 
                     'Udmert', 'Finno Ugrian (1990-2000)', 'Livonian', 'Estonian', 'Moravian (1990-2000)', 'Bohemian (1990-2000, ACS, PRCS)', 'Bohemian', 
                     'Czech', 'Czechoslovakian', 'Croatian', 'Turcoman (1980)', 'Kirghiz (1980)', 'Turkestani (1990-2000, 2012 ACS)', 'Cossack (1980)', 
                     'Cossack (1990-2000)', 'Rusyn', 'Carpatho Rusyn', 'Carpathian', 'Bulgarian', 'Belorussian', 'Azerbaijani', 'Albanian', 'Silesian (1990-2000)', 
                     'East German (1990-2000)', 'Finnish')


## Import the household and person replicate weights for variance estimation (person isn't actually needed)
repwt <- rbind(
  read_ipums_micro(read_ipums_ddi("usa_00089.xml")), ## 2013-2018
  read_ipums_micro(read_ipums_ddi("usa_00117.xml")) ## 2019
) %>% select(-c(YEAR, CBSERIAL, STRATA, HHWT, CLUSTER, STATEFIP, GQ, PERWT)) %>%
  mutate_at(vars(SAMPLE, SERIAL, PERNUM), as.numeric)


## Import the PUMS microdata
read_pums <- function(filename){
  read_ipums_micro(read_ipums_ddi(filename)) %>%
    # filter(MET2013 == 38900) %>%
    mutate_at(vars(GQ:OCC2010, MIGRATE1:MOVEDIN), haven::as_factor) %>%
    mutate_at(vars(YEAR:STRATA, INCTOT:INCOTHER, PERWT, PERNUM, AGE, OWNCOST, RENTGRS), as.numeric) %>% ## TODO double check these
    mutate_at(vars(OCCSOC, INDNAICS), as.character)
}

pums <- rbind(read_pums("usa_00092.xml"), read_pums("usa_00116.xml")) ## 2013-18 and 2019

pums_cleaned <- pums %>%
  left_join(., cpi, by = c("YEAR" = "year")) %>%
  
  ## Corrections before recoding
  mutate(AGE = AGE - 1,
         INCTOT = if_else(INCTOT == 9999999, NA_real_, INCTOT * inflation_factor_2018),
         INCWAGE = if_else(INCWAGE == 999999, NA_real_, INCWAGE * inflation_factor_2018),
         INCBUS00 = if_else(INCBUS00 == 999999, NA_real_, INCBUS00 * inflation_factor_2018),
         INCSS = if_else(INCSS == 99999, NA_real_, INCSS * inflation_factor_2018),
         INCINVST = if_else(INCINVST == 999999, NA_real_, INCINVST * inflation_factor_2018),
         INCRETIR = if_else(INCRETIR == 999999, NA_real_, INCRETIR * inflation_factor_2018),
         INCSUPP = if_else(INCSUPP == 99999, NA_real_, INCSUPP * inflation_factor_2018),
         INCWELFR = if_else(INCWELFR == 99999, NA_real_, INCWELFR * inflation_factor_2018),
         INCOTHER = if_else(INCOTHER == 99999, NA_real_, INCOTHER * inflation_factor_2018),) %>%
  
  ## Pre-recoding for calculating household-level characteristics
  mutate(ssi_disability_flag = if_else(INCSS > 10000 & AGE < 62 & INCWAGE < 5000, TRUE, FALSE), # Flag disabled if they are under 62, receive SS and have minimal wages
         qualified_person_flag = if_else(AGE >= 19 & AGE <= 65 & ssi_disability_flag == FALSE, 1, 0), # Flag disabled, youth and retired folks to '0'
         lep_helper1 = case_when(AGE < 14 | SPEAKENG == "N/A (Blank)" ~ 0L,
                                 SPEAKENG %in% c("Does not speak English", "Yes, speaks well", "Yes, but not well") ~ 0L,
                                 T ~ 1L),
         per_type = case_when(AGE <= 2 ~ 1,
                              AGE > 2 & AGE <= 5 ~ 10,
                              AGE > 5 & AGE <= 12 ~ 100,
                              AGE > 12 & AGE <= 18 ~ 1000,
                              AGE > 18 ~ 10000),
         adult = if_else(AGE > 18, 1, 0),
         infant = if_else(AGE <= 2, 1, 0),
         preschooler = if_else(AGE > 2 & AGE <= 5, 1, 0),
         schoolager = if_else(AGE > 5 & AGE <= 12, 1, 0),
         teenager = if_else(AGE > 12 & AGE <= 18, 1, 0)) %>% 
  group_by(SAMPLE, SERIAL) %>%
  mutate(qualified_in_hh = sum(qualified_person_flag),
         lep_helper2 = sum(lep_helper1),
         lep_hh = if_else(lep_helper2 == 0, "LEP household", "Non-LEP household"),
         hh_size = max(PERNUM),
         hh_income = sum(INCTOT, na.rm = T),
         adults = sum(adult),
         infants = sum(infant),
         preschoolers = sum(preschooler),
         schoolagers = sum(schoolager),
         teenagers = sum(teenager),
         children = infants + preschoolers + schoolagers + teenagers) %>% 
  ungroup() %>% 
  
  ## Recoding begins
  mutate(unqualified_hh_flag = if_else(qualified_in_hh == 0 | !(GQ %in% gq_filter), TRUE, FALSE), # Flag unqualified households or those in GQs
         family_type = case_when(adults >= 4 & children == 0 ~ paste0("a",adults,"i0p0s0t0"), #,infants,"p",preschoolers,"s",schoolagers,"t",teenagers),
                                 adults >= 4 ~ paste0("a",adults,"c",children),
                                 T ~ paste0("a",adults,"i",infants,"p",preschoolers,"s",schoolagers,"t",teenagers)),
         unadj_housing_costs = case_when(OWNERSHP == "Owned or being bought (loan)" ~ OWNCOST,
                                         OWNERSHP == "Rented" ~ RENTGRS,
                                         T ~ 0),
         housing_costs_adj = unadj_housing_costs * inflation_factor_2018,
         RACHISP = case_when(HISPAN != "Not Hispanic" ~ "Yes", T ~ "No"),
         race_rec = case_when(RACHISP == "Yes" ~ "Hispanic",
                              RACE == "White" ~ "White",
                              RACE == "Black/African American/Negro" ~ "Black",
                              RACE == "American Indian or Alaska Native" ~ "Native American",
                              RACE %in% c("Chinese", "Japanese", "Other Asian or Pacific Islander") & RACASIAN == "Yes" ~ "Asian",
                              RACE == "Other Asian or Pacific Islander" & RACPACIS == "Yes" ~ "Pacific Islander",
                              RACE == "Other race, nec" ~ "Another race",
                              RACE %in% c("Two major races", "Three or more major races") ~ "Multi-racial",
                              T ~ NA_character_),
         # Factors above 58 are outside U.S. posessions (foreign countries)
         birthplace_rec = case_when(as.numeric(BPL) > 58 & CITIZEN != "Born abroad of American parents" ~ "Foreign-born", T ~ "Not foreign-born"),
         # Factors here are people born in Eastern Europe
         RACSLAVIC = case_when(BPLD %in% ee_slavic_bpl | BPL %in% ee_slavic_bpl ~ "Yes",
                               LANGUAGED %in% ee_slavic_lang | LANGUAGE %in% ee_slavic_lang ~ "Yes",
                               ANCESTR1D %in% ee_slavic_ancest & ANCESTR2D == "Not Reported" ~ "Yes",
                               T ~ "No"),
         
         educ_rec = case_when(EDUCD %in% edu_lths ~ "Less than high school",
                              EDUCD %in% edu_hs ~ "High school graduate or equivalent",
                              EDUCD %in% edu_sc ~ "Some college or Associate's",
                              EDUCD %in% edu_ba ~ "Bachelor's degree or higher",
                              T ~ NA_character_)) %>%
  group_by(SAMPLE, SERIAL) %>%
  mutate(qualified_hh_income = sum(INCTOT[qualified_person_flag == 1], na.rm = T)) %>% ## Sum total income only for qualified persons in the household
  ungroup() %>%
  left_join(., sss, by = c("STATEFIP", "COUNTYFIP", "family_type")) %>%
  mutate(self_sufficient_flag = if_else(hh_income >= sss_wage_adj, "Self-sufficient", "Not self-sufficient")) %>% # Use qualified_hh_income if you must exclude folks
  mutate_at(vars(YEAR, PUMA, RACHISP, race_rec, birthplace_rec, lep_hh, RACSLAVIC, educ_rec, self_sufficient_flag), as.factor) %>%
  left_join(., repwt, by = c("SAMPLE", "SERIAL", "PERNUM")) %>%
  mutate(HHWT_3 = HHWT / 3,
         PERWT_3 = PERWT / 3)

### Three-year replicate weight sample design for calculating standard errors
h_repd <- pums_cleaned %>%
  select(-c(REPWTP:REPWTP80)) %>% # Remove person weights !! IMPORTANT !!
  filter(PERNUM == 1, # Select householder
         GQ %in% gq_filter, # Remove group quarters
         # YEAR %in% c(2016, 2017, 2018), # Subset 3 years
         MET2013 == 38900, # Keep only Portland MSA
         unqualified_hh_flag == FALSE, # Remove unqualified households
         !(is.na(self_sufficient_flag))) %>% # Remove NA values for whether they are self-sufficient
  mutate_at(vars(HHWT, REPWT:REPWT80), ~./3) %>% # Divide replicate weights and household weights by 3 since using 3-year sample
  as_survey_rep(weights = "HHWT",
                repweights = starts_with("REPWT"),
                combined_weights = T,
                type = "JK1",
                scale = 4/80,
                rscales = ncol('REPWT[0-9]+'))

## Quickly demonstrate self-sufficiency standard on race alone
h_repd %>%
  filter(COUNTYFIP == 51 & STATEFIP == 41) %>% # Filter for Multnomah County
  group_by(self_sufficient_flag, race_rec) %>%
  summarize(hh = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = hh_se / hh,
         moe = hh_se * 1.645) %>%
  arrange(race_rec)





#### Define a few helper functions to analyze some data
## (Sorry this is so ugly!)

## Filter for year and for a geography wrapped in quotes; use 3-year trailing average from `analysis_year`
geo_year_filter <- function(design, analysis_year, geofilter_statement = "COUNTYFIP == 51 & STATEFIP == 41") {
  # Helpful: https://stackoverflow.com/questions/61692367/how-to-pass-a-filter-statement-as-a-function-parameter-in-dplyr-using-quosure
  if(analysis_year == 2016) {
    design %>%
      filter(
        YEAR %in% c(2014, 2015, 2016),
        # COUNTYFIP == 51 & STATEFIP == 41
        eval(rlang::parse_expr(geofilter_statement)) ## Washington County would be `"COUNTYFIP == 67 & STATEFIP == 41"`
      ) %>%
      mutate(analysis_year = 2016)
  }
  if(analysis_year == 2018) {
    design %>%
      filter(
        YEAR %in% c(2016, 2017, 2018),
        # COUNTYFIP == 51 & STATEFIP == 41
        eval(rlang::parse_expr(geofilter_statement))
      ) %>%
      mutate(analysis_year = 2018)
  }
}

## Summarize the design by households and add moe and cv
sss_summarize <- function(design) {
  design %>%
    summarize(hh = survey_total(na.rm = T),
              count = unweighted(n())) %>%
    mutate(hh_moe = hh_se * 1.645,
           hh_cv = hh_se / hh,
           hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
                                      hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
                                      hh_cv < 0.2 ~ "1. Use"))
}

## Determine the self-sufficiency standard by passing in varuables, statements to 
## test for, a group name for the summary and an analysis year
get_sss <- function(svydesign, var, truevar, group_name, ayear, geofilter) {
  # var is the variable containing the demographic of interest
  # truevar is the string to test var against
  ayear <- ayear
  svydesign %>%
    geo_year_filter(., analysis_year = ayear, geofilter_statement = geofilter) %>% # Filter for Multnomah County
    mutate(group = group_name) %>% # Add the group name
    group_by(group, !!var, analysis_year, self_sufficient_flag) %>% # Group by the key variables
    sss_summarize(.) %>%
    filter(!!var == !!truevar) %>% # Return only records for the tested variable, i.e., racblk == "Yes"
    select(-!!var)
}

## Group 1 (g1) for TOTAL HOUSEHOLDS. 
## This returns the same results as all groupings below; this is just an explicit
## example of what is happening under the hood.
g1_18 <- h_repd %>%
  filter(
    COUNTYFIP == 51, STATEFIP == 41,
    # YEAR %in% c(2014, 2015, 2016),
    YEAR %in% c(2016, 2017, 2018),
  ) %>% 
  group_by(self_sufficient_flag) %>%
  summarize(hh = survey_total(na.rm = T),
            count = unweighted(n())) %>%
  mutate(hh_moe = hh_se * 1.645,
         hh_cv = hh_se / hh,
         hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
                                    hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
                                    hh_cv < 0.2 ~ "1. Use"),
         group = "Total households",
         analysis_year = 2018)


## The difference for these race categories is that we are using race alone or in combination
## which double-counts some households but increases sample size
g2_18 <- get_sss(h_repd, quo(RACBLK), "Yes", "Black households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g3_18 <- get_sss(h_repd, quo(RACHISP), "Yes", "Hispanic households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g4_18 <- get_sss(h_repd, quo(RACAMIND), "Yes", "Native American households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g5_18 <- get_sss(h_repd, quo(RACASIAN), "Yes", "Asian households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g6_18 <- get_sss(h_repd, quo(RACPACIS), "Yes", "Pacific Islander households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g7_18 <- get_sss(h_repd, quo(RACOTHER), "Yes", "Another race households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g8_18 <- get_sss(h_repd, quo(RACSLAVIC), "Yes", "Slavic households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g9_18 <- get_sss(h_repd, quo(race_rec), "White", "White alone households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g10_18 <- get_sss(h_repd, quo(race_rec), "Multi-racial", "Multi-racial households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g11_18 <- get_sss(h_repd, quo(lep_hh), "LEP household", "LEP households", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g12_18 <- get_sss(h_repd, quo(educ_rec), "Less than high school", "Less than HS", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g13_18 <- get_sss(h_repd, quo(educ_rec), "High school graduate or equivalent", "HS diploma", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g14_18 <- get_sss(h_repd, quo(educ_rec), "Some college or Associate's", "Some college", 2018, "COUNTYFIP == 51 & STATEFIP == 41")
g15_18 <- get_sss(h_repd, quo(educ_rec), "Bachelor's degree or higher", "Four-year degree", 2018, "COUNTYFIP == 51 & STATEFIP == 41")

# Bind all the groupings together
sss18 <- rbind(g1_18, g2_18, g3_18, g4_18, g5_18, g6_18, g7_18, g8_18, g10_18, g9_18, g11_18, g12_18, g13_18, g14_18, g15_18) ; sss18

# Display in tabular format
sss18 %>%
  select(group, self_sufficient_flag, hh) %>%
  pivot_wider(names_from = self_sufficient_flag, values_from = hh) %>%
  mutate(total_hh = (`Self-sufficient` + `Not self-sufficient`),
         share_self_sufficient = `Self-sufficient` / total_hh)






############ FOR LIZA ############
## For Liza - all family types by race
pums_cleaned %>%
  filter(
    PERNUM == 1,
    GQ %in% gq_filter,
    YEAR == 2019,
    # YEAR %in% c(2017, 2018, 2019),
    MET2013 == 38900,
  ) %>%
  # mutate(HHWT = HHWT/3) %>%
  group_by(family_type, race_rec) %>%
  # summarize(hh = round(sum(HHWT))) %>% ungroup() %>%
  summarize(hh = n()) %>% ungroup() %>%
  pivot_wider(id_cols = family_type, values_from = hh, names_from = race_rec) %>%
  mutate(total = select(., c(everything(), -family_type)) %>% rowSums(na.rm = T)) %>%
  arrange(desc(total)) %>% clipr::write_clip()

## For Liza - top 10 family types by race
pums_cleaned %>%
  filter(
    PERNUM == 1,
    GQ %in% gq_filter,
    YEAR == 2019,
    # YEAR %in% c(2017, 2018, 2019),
    MET2013 == 38900,
  ) %>%
  # mutate(HHWT = HHWT/3) %>%
  group_by(family_type, race_rec) %>%
  summarize(hh = round(sum(HHWT))) %>% ungroup() %>% 
  group_by(race_rec) %>% top_n(10, wt = hh) %>%
  arrange(race_rec, desc(hh)) %>% clipr::write_clip()

h_repd %>%
  filter(YEAR %in% c(2016, 2017, 2018)) %>% # TODO Change to 2019 when repwt file is ready to download
  group_by(family_type, race_rec) %>%
  summarize(hh = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = hh_se / hh,
         moe = hh_se * 1.645) %>%
  group_by(race_rec) %>% top_n(10, wt = hh) %>%
  arrange(race_rec, desc(hh)) %>% clipr::write_clip()





############ ARCHIVE - might be useful later ############

# 
# 
# h %>%
#   mutate(PUMA = as.factor(PUMA)) %>%
#   # filter(MET2013 == 38900) %>%
#   filter(
#     # COUNTYFIP == 51, STATEFIP == 41,
#     GQ %in% gq_filter,
#     PUMA %in% pdxpumas | PUMA == 1316,
#     # RACSLAVIC == "No",
#     unqualified_hh_flag == FALSE,
#     is.na(self_sufficient_flag) == FALSE,
#     # YEAR %in% c(2014, 2015, 2016),
#     YEAR %in% c(2016, 2017, 2018),
#   ) %>% 
#   group_by(self_sufficient_flag, race_rec) %>%
#   summarize(hh = survey_total(na.rm = T),
#             count = unweighted(n())) %>%
#   mutate(hh_moe = hh_se * 1.645,
#          hh_cv = hh_se / hh,
#          hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
#                                     hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
#                                     hh_cv < 0.2 ~ "1. Use")) %>%
#   select(race_rec, self_sufficient_flag, hh) %>%
#   pivot_wider(names_from = self_sufficient_flag, values_from = hh) %>%
#   mutate(total_hh = (`Self-sufficient` + `Not self-sufficient`),
#          share_self_sufficient = `Self-sufficient` / total_hh)
# 
# 
# 
# 
# 
# 
# 
# 
# g1_16 <- h %>%
#   mutate(PUMA = as.factor(PUMA)) %>%
#   filter(
#     COUNTYFIP == 51, STATEFIP == 41,
#     GQ %in% gq_filter,
#     unqualified_hh_flag == FALSE,
#     is.na(self_sufficient_flag) == FALSE,
#     YEAR %in% c(2014, 2015, 2016),
#     # YEAR %in% c(2016, 2017, 2018),
#   ) %>% 
#   group_by(self_sufficient_flag) %>%
#   summarize(hh = survey_total(na.rm = T),
#             count = unweighted(n())) %>%
#   mutate(hh_moe = hh_se * 1.645,
#          hh_cv = hh_se / hh,
#          hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
#                                     hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
#                                     hh_cv < 0.2 ~ "1. Use"),
#          group = "Total households",
#          analysis_year = 2016)
# 
# 
# g2_16 <-  get_sss(h, quo(RACBLK), "Black households", 2016)
# 
# g3_16 <- get_sss(h, quo(RACHISP), "Hispanic households", 2016)
# 
# g4_16 <- get_sss(h, quo(RACAMIND), "Native American households", 2016)
# 
# g5_16 <- get_sss(h, quo(RACASIAN), "Asian households", 2016)
# 
# g6_16 <- get_sss(h, quo(RACPACIS), "Pacific Islander households", 2016)
# 
# g7_16 <- get_sss(h, quo(RACOTHER), "Another race households", 2016)
# 
# g8_16 <- get_sss(h, quo(RACSLAVIC), "Slavic households", 2016)
# 
# 
# g9_16 <- h %>%
#   mutate(PUMA = as.factor(PUMA)) %>%
#   filter(
#     COUNTYFIP == 51, STATEFIP == 41,
#     GQ %in% gq_filter,
#     unqualified_hh_flag == FALSE,
#     is.na(self_sufficient_flag) == FALSE,
#     YEAR %in% c(2014, 2015, 2016),
#     # YEAR %in% c(2016, 2017, 2018),
#     race_rec == "White"
#   ) %>% 
#   group_by(self_sufficient_flag) %>%
#   summarize(hh = survey_total(na.rm = T),
#             count = unweighted(n())) %>%
#   mutate(hh_moe = hh_se * 1.645,
#          hh_cv = hh_se / hh,
#          hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
#                                     hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
#                                     hh_cv < 0.2 ~ "1. Use"),
#          group = "White alone households",
#          analysis_year = 2016)
# 
# g10_16 <- h %>%
#   mutate(PUMA = as.factor(PUMA)) %>%
#   filter(
#     COUNTYFIP == 51, STATEFIP == 41,
#     GQ %in% gq_filter,
#     unqualified_hh_flag == FALSE,
#     is.na(self_sufficient_flag) == FALSE,
#     YEAR %in% c(2014, 2015, 2016),
#     # YEAR %in% c(2016, 2017, 2018),
#     race_rec == "Multi-racial"
#   ) %>% 
#   group_by(self_sufficient_flag) %>%
#   summarize(hh = survey_total(na.rm = T),
#             count = unweighted(n())) %>%
#   mutate(hh_moe = hh_se * 1.645,
#          hh_cv = hh_se / hh,
#          hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
#                                     hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
#                                     hh_cv < 0.2 ~ "1. Use"),
#          group = "Multi-racial households",
#          analysis_year = 2016)
# 
# 
# sss16 <- rbind(g1_16, g2_16, g3_16, g4_16, g5_16, g6_16, g7_16, g8_16, g10_16, g9_16) ; sss16
# 
# sss16 %>%
#   select(group, analysis_year, self_sufficient_flag, hh) %>%
#   pivot_wider(names_from = self_sufficient_flag, values_from = hh) %>%
#   mutate(total_hh = (`Self-sufficient` + `Not self-sufficient`),
#          share_self_sufficient = `Self-sufficient` / total_hh)
# 
# 
# s <- rbind(sss18, sss16) %>%
#   select(analysis_year, group, self_sufficient_flag, hh) %>%
#   pivot_wider(names_from = self_sufficient_flag, values_from = hh) %>%
#   mutate(total_hh = (`Self-sufficient` + `Not self-sufficient`),
#          share_self_sufficient = `Self-sufficient` / total_hh) %>%
#   arrange(group)
# 
# s %>%
#   ggplot(aes(x = analysis_year, y = share_self_sufficient, color = group)) +
#   geom_line(size = 2)
# 
# 
# 
# 
# 
# ## https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones
# # repwt %>%
# #   mutate_at(vars(REPWT1:REPWT80), funs(./3))
# 
# 
# # repwtsub <- repwt %>% 
# #   filter(YEAR %in% c(2013, 2014, 2015, 2016, 2017, 2018)) %>%
# #   # mutate_at(vars(REPWT1:REPWT80), funs(./3)) %>%
# #   # mutate_at(vars(REPWTP1:REPWTP80, REPWTP), funs(./3)) %>%
# #   select(-c(YEAR, CBSERIAL, STRATA, HHWT, CLUSTER, STATEFIP, GQ, PERWT))
# 
# # sssraw <- rio::import("usa_00088.csv")
# 
# # pums_sub <- pums %>%
# #   filter(YEAR %in% c(2013, 2014, 2015, 2016, 2017, 2018),
# #          MET2013 == 38900)  %>%
# #   mutate_at(vars(HHWT, PERWT), funs(./3))
# 
# 
# # lep <- pums %>%
# #   select(SAMPLE, SERIAL, PERNUM, AGE, LANGUAGE, SPEAKENG) %>%
# #   mutate(hh_id = paste0(SAMPLE, SERIAL),
# #          lep_helper1 = case_when(AGE < 14 | SPEAKENG == "N/A (Blank)" ~ 0L,
# #                                  SPEAKENG %in% c("Does not speak English", "Yes, speaks well", "Yes, but not well") ~ 0L,
# #                                  T ~ 1L)) %>%
# #   group_by(SAMPLE, SERIAL) %>%
# #   mutate(lep_helper2 = sum(lep_helper1),
# #          lep_hh = if_else(lep_helper2 == 0, "LEP household", "Non-LEP household"))
# 
# 
# 
# # s_design2 <- s %>%
# #   as_survey_rep(weights = HHWT,
# #                 repweights = starts_with("REPWT"),
# #                 combined_weights = T,
# #                 type = "JK1",
# #                 scale = 4/80,
# #                 rscales = ncol('REPWT[0-9]+'))
# 
# 
# # test <- s_design %>%
# #   filter(COUNTYFIP == 51, STATEFIP == 41) %>%
# #   group_by(YEAR) %>%
# #   summarize(hh = survey_total(na.rm = T)) %>%
# #   mutate(hh_moe = hh_se * 1.645,
# #          hh_cv = hh_se / hh,
# #          hh_reliability = case_when(hh_cv > 0.4 ~ "3. Unreliabile",
# #                                      hh_cv <= 0.4 & hh_cv > 0.2 ~ "2. Use with caution",
# #                                      hh_cv < 0.2 ~ "1. Use"))