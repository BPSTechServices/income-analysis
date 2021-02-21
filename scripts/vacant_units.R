


raw_vacancy_pums<- read_ipums_micro(read_ipums_ddi("data/raw/usa_00119.xml")) %>% janitor::clean_names()

vacant_units <- raw_vacancy_pums %>%
  select(year:cbserial, hhwt, cluster:gq, rent, valueh:bedrooms) %>% 
  mutate_at(vars(met2013, gq, vacancy, kitchen, sink, stove, plumbing), haven::as_factor) %>%
  mutate_at(vars(year, hhwt, statefip, puma, rent, valueh, rooms:bedrooms), as.numeric) %>%
  left_join(., repwt_raw, by = c("sample", "serial")) %>%
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