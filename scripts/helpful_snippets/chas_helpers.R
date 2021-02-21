extract_fips <- function(geoid, sumlevel){
  ## Extract FIPS code from geoid provided a sumlevel
  ## TODO Add all relevant sumlevels and substrings
  fips <- case_when(sumlevel == 80 ~ paste0(substr(geoid, 8, 12), substr(geoid, 23, 29)),
                    sumlevel == 140 ~ substr(geoid, 8,20),
                    sumlevel == 160 ~ substr(geoid, 8,14),
                    TRUE ~ geoid)
  return(fips)
}

drop_cols <- function(chastable) {
  ## Drops all unnecessary columns except GEOID and name
  ## TODO Add other sumlevel types
  chastable$sumlevel <- as.numeric(chastable$sumlevel)
  sumlevel <- chastable$sumlevel[1]
  
  if(sumlevel == 140) {
    ret <- chastable %>% select(GEOID, year, everything(), -c(source, sumlevel, geoid, st, cnty, tract))
    return(ret)
  } else if(sumlevel == 160) {
    ret <- chastable %>% select(GEOID, year, everything(), -c(source, sumlevel, geoid, st, place))
    return(ret)
  } else if(sumlevel == 80) {
    ret <- chastable %>% select(GEOID, year, everything(), -c(source, sumlevel, geoid))
    return(ret)
  } else{
    break()
  }
}

clean_chas <- function(csvpath, year, filter_statement) {
  year <- year
  raw_chastable <- read_csv(csvpath)
  
  ret <- raw_chastable %>%
    mutate(year = year,
           GEOID = extract_fips(geoid, sumlevel)) %>%
    drop_cols(.) %>%
    pivot_longer(-c(GEOID, year, name),
                 names_to = c("table", "type", "varnum"),
                 names_pattern = "T(\\d+)_(est|moe)(\\d+)",
                 values_to = "value") %>% 
    pivot_wider(names_from = "type", values_from = "value") %>%
    mutate(table = as.numeric(table),
           varnum = as.numeric(varnum)) %>%
    filter(eval(rlang::parse_expr(filter_statement)))
  return(ret)
}

data <- 


clean_chas_zipped <- function(zip_path, table_path, year, filter_statement) {
  year <- year
  raw_chastable <- read.table(unz(zip_path, table_path), header=T, quote="\"", sep=",", stringsAsFactors = F)
  
  ret <- raw_chastable %>%
    mutate(year = year,
           GEOID = extract_fips(geoid, sumlevel)) %>%
    drop_cols(.) %>%
    pivot_longer(-c(GEOID, year, name),
                 names_to = c("table", "type", "varnum"),
                 names_pattern = "T(\\d+)_(est|moe)(\\d+)",
                 values_to = "value") %>% 
    pivot_wider(names_from = "type", values_from = "value") %>%
    mutate(table = as.numeric(table),
           varnum = as.numeric(varnum)) %>%
    filter(eval(rlang::parse_expr(filter_statement)))
  return(ret)
}



vintage_clean_chas <- function(csvpath, year, filter_statement) {
  year <- year
  raw_chastable <- read_csv(csvpath)
  sumlevel <- as.numeric(substr(raw_chastable$geoid[1], 1, 3))
  
  ret <- raw_chastable %>%
    mutate(year = year,
           sumlevel = substr(geoid, 1, 3),
           name = NA_character_,
           GEOID = extract_fips(geoid, as.numeric(sumlevel))) %>%
    drop_cols(.) %>%
    pivot_longer(-c(GEOID, year, name),
                 names_to = c("table", "type", "varnum"),
                 names_pattern = "T(\\d+)_(est|moe)(\\d+)",
                 values_to = "value") %>% 
    pivot_wider(names_from = "type", values_from = "value") %>%
    mutate(table = as.numeric(table),
           varnum = as.numeric(varnum)) %>%
    filter(eval(rlang::parse_expr(filter_statement)))
  
  if(sumlevel == 80) {
    ret <- ret %>%
      group_by(GEOID, year, name, table, varnum) %>%
      mutate(newest = sum(unlist(est), na.rm = T),
             newmoe = moe_sum(unlist(moe), estimate = newest)) %>% ungroup() %>%
      select(GEOID, year, name, table, varnum, est = newest, moe = newmoe) %>%
      mutate(moe = case_when(est == 0 ~ 22, ## Hard-code MoE to 22 for zero estimates. Needs more thought. Rationale is that MoEs are not coded right at 080 sumlevel
                             TRUE ~ moe)) ## However this doesn't seem to work...
  } else if(sumlevel == 160) {
    ret <- ret %>% select(GEOID, year, name, table, varnum, est, moe)
  } else {
    break()
  }
  
  return(ret)
}


vintage_clean_chas_zipped <- function(zip_path, table_path, year, filter_statement) {
  year <- year
  raw_chastable <- read.table(unz(zip_path, table_path), header=T, quote="\"", sep=",")
  sumlevel <- as.numeric(substr(raw_chastable$geoid[1], 1, 3))
  
  ret <- raw_chastable %>%
    mutate(year = year,
           sumlevel = substr(geoid, 1, 3),
           name = NA_character_,
           GEOID = extract_fips(geoid, as.numeric(sumlevel))) %>%
    drop_cols(.) %>%
    pivot_longer(-c(GEOID, year, name),
                 names_to = c("table", "type", "varnum"),
                 names_pattern = "T(\\d+)_(est|moe)(\\d+)",
                 values_to = "value") %>% 
    pivot_wider(names_from = "type", values_from = "value") %>%
    mutate(table = as.numeric(table),
           varnum = as.numeric(varnum)) %>%
    filter(eval(rlang::parse_expr(filter_statement)))
  
  if(sumlevel == 80) {
    ret <- ret %>%
      group_by(GEOID, year, name, table, varnum) %>%
      mutate(newest = sum(unlist(est), na.rm = T),
             newmoe = moe_sum(unlist(moe), estimate = newest)) %>% ungroup() %>%
      select(GEOID, year, name, table, varnum, est = newest, moe = newmoe) %>%
      mutate(moe = case_when(est == 0 ~ 22, ## Hard-code MoE to 22 for zero estimates. Needs more thought. Rationale is that MoEs are not coded right at 080 sumlevel
                             TRUE ~ moe)) ## However this doesn't seem to work...
  } else if(sumlevel == 160) {
    ret <- ret %>% select(GEOID, year, name, table, varnum, est, moe)
  } else {
    break()
  }
  
  return(ret)
}
