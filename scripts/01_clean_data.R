## This analysis attempts to replicate the PUMS-derived data from Seattle's Displacement Risk Indicators 
## See here: https://population-and-demographics-seattlecitygis.hub.arcgis.com/pages/displacement-risk
## Their analysis produces citywide income estimates, similar to CHAS data, including the following variables:
## - Cost burden and severe cost burden by age, race, tenure and income category
## - Renter households by income category
## - Rental units by affordability category
## - Affordable and available rental units per 100 households

## This analysis attempts to reproduce their figures for Portland MSA and the City of Portland

## Source the import data script to load libraries, clear memory and import files used for use in this analysis
source("scripts/00_import_data.R")

pums_clean <- pums_raw %>%
  left_join(., puma_county_xwalk, by = c("statefip" = "state", "puma")) %>%
  left_join(., cpi, by = "year") %>%
  left_join(., hud_ami, by = c("stcnty_fips", "year")) %>%
  mutate_at(vars(incss, incsupp, incwelfr, incother), .funs = ~if_else(. == 99999, NA_real_, .)) %>% # Convert income jam value to NA_real_ types
  mutate_at(vars(incwage, incbus00, incinvst, incretir), .funs = ~if_else(. == 999999, NA_real_, .)) %>% # Convert income jam value to NA_real_ types
  mutate_at(vars(inctot, hhincome), .funs = ~if_else(. == 9999999, NA_real_, .)) %>% # Convert income jam value to NA_real_ types
  mutate_at(vars(inctot:incother, hhincome), .funs = list(adj = ~ . * inflation_factor_2019)) %>% # Create new variables and adjust them for inflation
  mutate(mfi_coefficient = case_when(numprec == 1 ~ 0.7,
                                     numprec == 2 ~ 0.8,
                                     numprec == 3 ~ 0.9,
                                     numprec == 4 ~ 1,
                                     numprec > 4 ~ (numprec-4)*0.08+1), # Create MFI coefficients based on household size
         adj_hamfi = mfi_coefficient * hamfi, # Calculate adjusted regional MFI benchmark based off household size
         hud_limit = hhincome / adj_hamfi, # Calculate their income ratio (i.e., "55% MFI") based off household income and adjusted HAMFI benchmark
         is_low_income = hud_limit < 0.8, # Determine if they are low-income if their income ratio is less than 80% of HAMFI
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
                               T ~ NA_character_)) %>%
  mutate_at(vars(year, puma, rachisp:race1_rec), as.factor) %>% # TODO Convert all relevant variables to factors for ease of use with survey object 
  left_join(., repwt_raw, by = c("sample", "serial", "pernum"))  %>% # Join the replicate weights to the end of the data
  rename_at(.vars = vars(repwt1:repwt80), .funs = ~ paste("WGTP", seq(1:80), sep = "")) %>% # Rename columns to be consistent with Census PUMS to use tidycensus features
  rename_at(.vars = vars(repwtp1:repwtp80), .funs = ~ paste0("PWGTP", seq(1:80), sep = "")) %>% # Rename columns to be consistent with Census PUMS to use tidycensus features
  rename(PWGTP = perwt, WGTP = hhwt, SERIALNO = cbserial, PUMA = puma) %>%  # Rename columns to be consistent with Census PUMS to use tidycensus features
  select(-c(repwt, repwtp)) # Unnecessary and causes me confusion ... 

## Create replicate design (repd) survey object of all persons from cleaned data
## Use tidycensus::to_survey to make a survey object. Requires appropriate variable names
p_repd <- pums_clean %>% 
  select(-c(WGTP1:WGTP80)) %>% 
  tidycensus::to_survey(type = "person", 
                        class = "srvyr", 
                        design = "rep_weights")

## TODO Consider joining vacant dataset prior to creating survey design object
## Create replicate design (repd) survey object of households from cleaned data
## NOTE: Will produce duplicate values warning, but it's because duplicate SERIALNO exist across years. Must be sure to group_by year in analysis.
h_repd <- pums_clean %>% 
  filter(pernum == 1) %>% 
  select(-c(PWGTP1:PWGTP80)) %>%
  tidycensus::to_survey(type = "housing", 
                        class = "srvyr", 
                        design = "rep_weights")

######## Explicit ways to define survey objects. ##########
## Same as above but very explicit. NOTE: I refer to the un-renamed repwt in this scope
# p_object <- pums_clean %>% select(-c(repwt:repwt80))
# p_repd <- survey::svrepdesign(variables = p_object[, !names(p_object) %in% c(paste("repwtp", seq(1:80), sep = ""), "repwtp")], 
#                               weights = p_object$perwt, 
#                               repweights = p_object %>% select(paste("repwtp", seq(1:80), sep = "")), 
#                               scale = 4/80, 
#                               rscales = rep(1, 80), 
#                               mse = TRUE, type = "JK1") %>%
#   srvyr::as_survey()

## Same as above but very explicit. NOTE: I refer to the un-renamed repwt in this scope
# h_object <- pums_clean %>% filter(pernum == 1) %>% select(-c(repwtp:repwtp80))
# h_repd <- survey::svrepdesign(variables = h_object[, !names(h_object) %in% c(paste("repwt", seq(1:80), sep = ""), "repwt")], 
#                               weights = h_object$hhwt, 
#                               repweights = h_object %>% select(paste("repwt", seq(1:80), sep = "")), 
#                               scale = 4/80, 
#                               rscales = rep(1, 80), 
#                               mse = TRUE, type = "JK1") %>%
#   srvyr::as_survey()
