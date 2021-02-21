##### Load libraries, set working directory #####
pacman::p_load(tidyverse, data.table, srvyr, ipumsr, rio, janitor)

options(
  scipen = 999, # remove scientific notation
  digits = 4, # set data precision for readability
  stringsAsFactors = F, # string variables are brought in as charactors
  dplyr.width = Inf,
  survey.replicates.mse = T,
  datatable.fread.datatable = F # <- is this necessary?
)

## Clear global environment and return space to memory
rm(list=ls())
gc()

## Import CPI (Census Research Series). Downloaded and compiled from https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm
cpi <- rio::import("data/resources/cpi-u-rs_1950-current.xlsx") %>%
  select(year, inflation_factor_2019)

## Import Census poverty thresholds. Downloaded and compiled from https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html
poverty <- rio::import("data/resources/poverty_thresholds.xlsx", which = "all_years") %>%
  left_join(., cpi, by = "year") %>%
  mutate(poverty_threshold_adj = threshold * inflation_factor_2019) %>%
  select(pov_code, year, poverty_threshold = threshold, poverty_threshold_adj, hh_size, num_children, age)

## Read HUD AMI Section 8 income limits. Unsure of source of compiled dataset...
hud_ami <- read_csv("data/resources/limits_2000t2020.csv", col_types = "cddcccdd") %>% 
  janitor::clean_names() %>% 
  select(stfips, cntyfips, msafips, msa_name, hamfi = mfi, year) #%>% distinct(stfips, cntyfips, msafips, msa_name, hamfi, year, .keep_all = T)

puma_county_xwalk <- "data/resources/geocorr_puma2010_county2010.csv" %>% 
  read_csv(col_names = c("state", "puma", "county", "afact"), col_types = "cdd____d", skip = 2) %>% 
  mutate(county = as.integer(str_sub(county, -3)), # county_geoid = ifelse(length(as.character(county))<3, paste0(state,"0",str_sub(county, -3)), paste0(state,str_sub(county, -3))),
         state = as.integer(state)) %>%
  arrange(state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE) %>%
  mutate(pumayear = 2012)

puma_county_xwalk00 <- "data/resources/puma00_to_county_geocorr2014.csv" %>% 
  read_csv(col_names = c("state", "puma", "county", "afact"), col_types = "cdd____d", skip = 2) %>% 
  mutate(county = as.integer(str_sub(county, -3)), # county_geoid = ifelse(length(as.character(county))<3, paste0(state,"0",str_sub(county, -3)), paste0(state,str_sub(county, -3))),
         state = as.integer(state)) %>%
  arrange(state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE) %>%
  mutate(pumayear = 2000)

puma_county_xwalk <- rbind(puma_county_xwalk, puma_county_xwalk00) %>% 
  arrange(desc(pumayear), state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE)

## Import recoding definitions (i.e., educational attainment and Slavic definition helpers)
source("data/resources/ipums_recoding_definitions.R")

## Import the household and person replicate weights for variance estimation
repwt_raw <- rbind(
  read_ipums_micro(read_ipums_ddi("data/raw/usa_00121.xml")), ## 2015-2019
  read_ipums_micro(read_ipums_ddi("data/raw/usa_00122.xml")), ## 2010-2014
  read_ipums_micro(read_ipums_ddi("data/raw/usa_00123.xml")) ## 2006-2009
) %>% select(-c(YEAR, CBSERIAL, STRATA, HHWT, CLUSTER, STATEFIP, GQ, PERWT)) %>%
  mutate_at(vars(SAMPLE, SERIAL, PERNUM), as.numeric) %>%
  janitor::clean_names()

## Import the PUMS microdata 2006-2019 income demographic estimates for OR and WA
pums_raw <- read_ipums_micro(read_ipums_ddi("data/raw/usa_00120.xml")) %>%
  janitor::clean_names() %>%
  # filter(MET2013 == 38900) %>%
  mutate_at(vars(gq:ownershpd, lingisol:kitchen, plumbing:unitsstr, sex, marst:citizen, language:indnaics, diffrem:diffhear), haven::as_factor) %>%
  mutate_at(vars(year:cluster, strata, owncost:hhincome, rooms, pernum, age, yrnatur, yrimmig, inctot:incearn), as.numeric) %>% ## TODO double check these
  mutate_at(vars(statefip, puma, occ:indnaics), as.character) 







