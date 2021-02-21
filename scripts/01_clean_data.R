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

mutate(AGE = AGE - 1,
       INCTOT = if_else(INCTOT == 9999999, NA_real_, INCTOT * inflation_factor_2018),
       INCWAGE = if_else(INCWAGE == 999999, NA_real_, INCWAGE * inflation_factor_2018),
       INCBUS00 = if_else(INCBUS00 == 999999, NA_real_, INCBUS00 * inflation_factor_2018),
       INCSS = if_else(INCSS == 99999, NA_real_, INCSS * inflation_factor_2018),
       INCINVST = if_else(INCINVST == 999999, NA_real_, INCINVST * inflation_factor_2018),
       INCRETIR = if_else(INCRETIR == 999999, NA_real_, INCRETIR * inflation_factor_2018),
       INCSUPP = if_else(INCSUPP == 99999, NA_real_, INCSUPP * inflation_factor_2018),
       INCWELFR = if_else(INCWELFR == 99999, NA_real_, INCWELFR * inflation_factor_2018),
       INCOTHER = if_else(INCOTHER == 99999, NA_real_, INCOTHER * inflation_factor_2018),)

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
  mutate_at(vars(year, puma, rachisp:race1_rec), as.factor) %>%
  left_join(., repwt_raw, by = c("sample", "serial", "pernum"))


## Create survey object of households in Portland from cleaned data
h_repd <- pums_clean %>%
  select(-c(repwtp:repwtp80)) %>% # Remove person weights !! IMPORTANT !!
  filter(pernum == 1, # Select householder
         met2013 == 38900, # Keep only Portland MSA
         ) %>% 
  as_survey_rep(weights = "hhwt",
                repweights = starts_with("repwt"),
                combined_weights = T,
                type = "JK1",
                scale = 4/80,
                rscales = ncol('repwt[0-9]+'))

## Example data summary (income category by year and tenure)
lowincome_by_year_tenure <- h_repd %>%
  group_by(year, is_low_income, ownershp) %>%
  summarize(hh = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = hh_se / hh,
         moe = hh_se * 1.645) #%>% clipr::write_clip() # uncomment to write the table to clipboard to paste into excel

## Graph the summarized example data
lowincome_by_year_tenure %>%
  filter(!is.na(is_low_income)) %>%
  mutate(year = as.numeric(as.character(year)),
         is_low_income = if_else(is_low_income, "Low income", "Moderate income +")) %>%
  ggplot(aes(x = year, y = hh)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax= hh+moe, ymin=hh-moe), 
              alpha=0.2) +
  facet_grid(ownershp~is_low_income, scales = "free_y") +
  scale_y_continuous(labels=scales::comma_format()) +
  theme_minimal() +
  labs(title = "Trend in number of households by tenure and income category",
       subtitle = "Portland MSA, 2006 - 2019",
       x = "Year",
       y = "Number of Households",
       caption = "Source: University of Minnesota, IPUMS-USA; American Community Survey 1-year estimates.\nAnalysis by Portland Bureau of Planning & Sustainability (BPS) and Portland Housing Bureau (PHB).")
