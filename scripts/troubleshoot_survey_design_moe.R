## Load in cleaned data
source("scripts/01_clean_data.R")


###### Experiment with tidycensus get_pums()

pums_vars_2019 <- tidycensus::pums_variables %>% 
  filter(year == 2019, survey == "acs1")

or_repwt_both <- tidycensus::get_pums(
  variables = c("PUMA", "SEX", "AGEP"),
  state = "OR", 
  year = 2019, 
  survey = "acs1", 
  rep_weights = "both", 
  recode = T, 
  show_call = T) %>%
  arrange(desc(SERIALNO))

or_survey_design_h <- or_repwt_both %>% filter(SPORDER == 1) %>%
  to_survey(., type = "housing", class = "srvyr", design = "rep_weights")

or_survey_design_p <- or_repwt_both %>%
  to_survey(., type = "person", class = "srvyr", design = "rep_weights")

or_survey_design_p %>% 
  mutate(PUMA = as.numeric(PUMA)) %>%
  filter(PUMA %in% pdxpumas) %>% # Keep only Portland City
  group_by(SEX_label) %>%
  summarize(pop = survey_total())

p_repd %>%
  filter(puma %in% pdxpumas,
         year == 2019) %>% # Keep only Portland City
  group_by(sex) %>%
  summarize(pop = survey_total())





# or_repwt_hh <- tidycensus::get_pums(
#   variables = c("PUMA", "SEX", "AGEP"),
#   state = "OR", 
#   year = 2019, 
#   survey = "acs1", 
#   rep_weights = "household", 
#   recode = T, 
#   show_call = T) %>%
#   arrange(desc(SERIALNO))
# 
# or_repwt_pop <- tidycensus::get_pums(
#   variables = c("PUMA", "SEX", "AGEP"),
#   state = "OR", 
#   year = 2019, 
#   survey = "acs1", 
#   rep_weights = "person", 
#   recode = T, 
#   show_call = T) %>%
#   arrange(desc(SERIALNO))


or_ipums_rpwt <- pums_clean %>%
  filter(year == 2019, statefip == 41) %>%
  select(cbserial, hhwt, pernum, perwt, repwt:repwt80, repwtp:repwtp80) %>%
  mutate(cbserial = ifelse(str_sub(cbserial, 1, 6) == "201901", 
                           paste0("2019GQ", str_sub(cbserial, 7, nchar(cbserial))), 
                           paste0("2019HU", str_sub(cbserial, 7, nchar(cbserial))))) %>%
  arrange(desc(cbserial))


#### Compare two survey designs

or_survey_design %>%
  mutate(puma = as.numerica(PUMA)) %>%
  filter(puma %in% pdxpumas) %>% # Keep only Portland City
  summarize(pop = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = pop_se / pop,
         moe = pop_se * 1.645) %>% View()

p_repd %>%
  filter(puma %in% pdxpumas,
         year == 2019) %>% # Keep only Portland City
  group_by(year) %>%
  summarize(pop = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = pop_se / pop,
         moe = pop_se * 1.645,
         year = as.numeric(as.character(year))) %>% View()




### Dissect `tidycensus::to_survey()` function to figure out how to make ipums data into correctly defined survey object

variables <- pums_clean[, !names(pums_clean) %in% c(paste("repwt", seq(1:80), sep = ""), "repwt")]
weights <- pums_clean$repwt

if (!all(paste("repwt", seq(1:80), sep = "") %in% names(pums_clean))) {
  stop("Not all housing replicate weight variables are present in input data.", 
       call. = FALSE)
}
if (!"repwt" %in% names(pums_clean)) {
  stop("Housing weight variable is not present in input data.", 
       call. = FALSE)
}
repweights <- pums_clean[, paste("repwt", seq(1:80), sep = "")]

survey <- survey::svrepdesign(variables = variables %>% select(-c(repwtp:repwtp80)), 
                              weights = weights, 
                              repweights = repweights, scale = 4/80, 
                              rscales = rep(1, 80), mse = TRUE, type = "JK1") %>%
  srvyr::as_survey()





or_survey_design <- to_survey(or_pums_rep_weights)

or_survey_design %>% 
  survey_count(PUMA, SEX_label)


or_pumas <- pums_clean %>% filter(met2013 == 38900, statefip == 41, year == 2019) %>% pull(puma) %>% as.character() %>% unique()
wa_pumas <- pums_clean %>% filter(met2013 == 38900, statefip == 53, year == 2019) %>% pull(puma) %>% as.character() %>% unique()

msa_pumas <- c(or_pumas, wa_pumas) %>%
  setNames(c(rep("OR", length(or_pumas)), rep("WA", length(wa_pumas))))

age_sex_school <- tidycensus::get_pums(variables = c("PUMA", "SEX", "AGEP", "SCHL"),
                                       state = "multiple", 
                                       puma = msa_pumas, 
                                       year = 2019, 
                                       survey = "acs1", 
                                       rep_weights = "both", 
                                       recode = T, 
                                       show_call = T)






## Total population

or_survey_design %>%
  mutate(puma = as.numerica(PUMA)) %>%
  filter(puma %in% pdxpumas) %>% # Keep only Portland City
  summarize(pop = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = pop_se / pop,
         moe = pop_se * 1.645) %>% View()

p_repd %>%
  filter(puma %in% pdxpumas,
         year == 2019) %>% # Keep only Portland City
  group_by(year) %>%
  summarize(pop = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = pop_se / pop,
         moe = pop_se * 1.645,
         year = as.numeric(as.character(year))) %>% View()



survey %>%
  filter(puma %in% pdxpumas) %>% # Keep only Portland City
  group_by(year) %>%
  summarize(pop = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = pop_se / pop,
         moe = pop_se * 1.645,
         year = as.numeric(as.character(year))) %>% View()


