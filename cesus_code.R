library(tidyverse)
library(tidycensus)

## Consider adding additional demographic variables such as education, marriage rates, unemployment rates, etc. 

acs <- NULL
for(year in c(2012,2017)){
  
  temp_df <- get_acs(geography = "county", variables = c("B19013_001",
                                                         "B02001_001", "B02001_002",
                                                         "B02001_003", "B02001_004",
                                                         "B02001_005", "B02001_006",
                                                         "B03001_003", "B25002_001",
                                                         "B25002_002", 
                                                         "B25002_003", 
                                                         "B06012_002", year = year)) %>%
    
    select(-moe) %>%
    mutate(variable = case_when(variable == "B19013_001" ~ 'med_fam_inc',
                                variable == "B02001_001" ~ 'total_pop',
                                variable == "B02001_002" ~ "white",
                                variable == "B02001_003" ~ "black",
                                variable == "B02001_004" ~ "american_indian",
                                variable == "B02001_005" ~ "asian",
                                variable == "B02001_006" ~ "native_haw_oth_pacisl",
                                variable == "B03001_003" ~ 'hispanic_or_latino', 
                                variable == "B25002_001" ~ "total_housing_units", 
                                variable == "B25002_002" ~ "occ_housing_units", 
                                variable == "B25002_002" ~ "vacant_housing_units", 
                                variable == "B06012_002" ~ "total_below_poverty_level"),
           year = year ) %>%
    pivot_wider(names_from = c(variable), values_from = c(estimate))

  acs <- bind_rows(temp_df, acs)
  
}



