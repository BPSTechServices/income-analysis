
source("scripts/01_clean_data.R")

## Example data summary (income category by year and tenure)
lowincome_by_year_tenure <- h_repd %>%
  filter(met2013 == 38900,
         gq %in% gq_filter) %>% # Keep only Portland MSA
  group_by(year, met2013, is_low_income, ownershp) %>%
  summarize(hh = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = hh_se / hh,
         moe = hh_se * 1.645) #%>% clipr::write_clip() # un-comment to write the table to clipboard to paste into excel

## Graph the summarized example data
lowincome_by_year_tenure %>%
  filter(!is.na(is_low_income)) %>%
  mutate(year = as.numeric(as.character(year)),
         is_low_income = if_else(is_low_income, "Low income", "Moderate income +")) %>%
  ggplot(aes(x = year, y = hh)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax= hh+moe, ymin=hh-moe), alpha=0.2) +
  facet_wrap(ownershp~is_low_income, scales = "free") +
  scale_y_continuous(labels=scales::comma_format()) +
  theme_minimal() +
  labs(title = "Trend in number of households by tenure and income category",
       subtitle = "Portland MSA, 2006 - 2019",
       x = "Year", y = "Number of Households",
       caption = "Source: University of Minnesota, IPUMS-USA; American Community Survey 1-year estimates.\nAnalysis by Portland Bureau of Planning & Sustainability (BPS) and Portland Housing Bureau (PHB).")


## Total households for Portland only
h_repd %>%
  filter(PUMA %in% pdxpumas) %>% # Keep only Portland City
  group_by(year, PUMA) %>%
  summarize(hh = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = hh_se / hh,
         moe = hh_se * 1.645,
         year = as.numeric(as.character(year))) %>% 
ggplot(aes(x = year, y = hh)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax= hh+moe, ymin=hh-moe), alpha=0.2) +
  scale_y_continuous(labels=scales::comma_format()) +
  facet_wrap(~PUMA) +
  theme_minimal() +
  labs(title = "Trend in number of households",
       subtitle = "City of Portland, 2006 - 2019",
       x = "Year", y = "Number of Households",
       caption = "Source: University of Minnesota, IPUMS-USA; American Community Survey 1-year estimates.\nAnalysis by Portland Bureau of Planning & Sustainability (BPS) and Portland Housing Bureau (PHB).")




## Total population
p_repd %>%
  filter(PUMA %in% pdxpumas) %>% # Keep only Portland City
  group_by(year) %>%
  summarize(pop = survey_total(),
            n = unweighted(n())) %>% ungroup() %>% 
  mutate(cv = pop_se / pop,
         moe = pop_se * 1.645,
         year = as.numeric(as.character(year)))
