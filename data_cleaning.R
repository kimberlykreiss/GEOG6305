library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
setwd("~/Documents/git/GEOG6305")
options(tigris_use_cache = TRUE)
##################################### 
# Step 1: Pull data from tidycensus and do necessary cleaning 
#####################################
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
                                                         "B06012_002", 
                                                         "B25004_002", 
                                                         "B25004_004"), year = year) %>%
    
    select(-moe) %>%
    mutate(variable = case_when(variable == "B19013_001" ~ 'med_fam_inc',
                                variable == "B02001_001" ~ 'total_pop',
                                variable == "B02001_002" ~ "white",
                                variable == "B02001_003" ~ "black",
                                variable == "B02001_004" ~ "american_indian",
                                variable == "B02001_005" ~ "asian",
                                variable == "B02001_006" ~ "native",
                                variable == "B03001_003" ~ 'hispanic', 
                                variable == "B25002_001" ~ "hunits", 
                                variable == "B25002_002" ~ "occh", 
                                variable == "B25002_003" ~ "vach", 
                                variable == "B06012_002" ~ "pov", 
                                variable == "B25004_002" ~ "vacrent", 
                                variable == "B25004_004" ~ "vacsale"),
           year = year ) %>%
    pivot_wider(names_from = c(variable), values_from = c(estimate))

  acs <- bind_rows(temp_df, acs) %>% 
    select(-NAME)
  
}


acs <- acs %>%
  rename(GEOID=st_cty_fips)


##################################### 
# Step 2: HMDA cleaning
#####################################
# I am using public HMDA data for years 2012 and 2017. 

# hmda_2012 <- read_csv("data/hmda_2012_nationwide_all-records_labels.csv") %>% 
#   filter(property_type %in% c(1,2), 
#          loan_purpose == 1, 
#          action_taken %in% c(1,2,3,7,8)) 
# hmda_total_mortg <- hmda_2012 %>%  
#   filter(action_taken %in% c(1,2,3,7,8), 
#          !is.na(county_code)) %>%
#   mutate(st_cty_fips = paste0(str_pad(as.character(state_code),width = 2,side=c("left"), pad = "0"), 
#                               str_pad(as.character(county_code),width = 3,side=c("left"), pad = "0"), 
#                               sep=""), 
#          state_county_name = paste(county_name, state_name, sep = ", ")) %>% 
#   mutate(approved = if_else(action_taken %in% c(1,2,8), "approved", "denied"), 
#          year = 2012) %>% 
#   group_by(st_cty_fips, state_county_name,approved) %>% 
#   summarise(total_mortgages = n()) %>%
#   pivot_wider(names_from = approved, values_from = total_mortgages) %>% 
#   mutate(approved = replace_na(approved, 0), denied = replace_na(denied, 0)) %>%
#   mutate(total_mortgages = approved + denied) %>% 
#   mutate(approval_rate = approved/total_mortgages) 
# 
# save(hmda_total_mortg, file = 'hmda_2012_agg.Rda')

# hmda_2017 <- read_csv("data/hmda_2017_nationwide_all-records_labels.csv") %>% 
#   filter(property_type %in% c(1,2), 
#          loan_purpose == 1, 
#          action_taken %in% c(1,2,3,7,8), 
#          !is.na(county_code)) 
# 
# hmda_2017_agg <- hmda_2017 %>%
#   mutate(st_cty_fips = paste0(str_pad(as.character(state_code),width = 2,side=c("left"), pad = "0"), 
#                               str_pad(as.character(county_code),width = 3,side=c("left"), pad = "0"), 
#                               sep=""), 
#          state_county_name = paste(county_name, state_name, sep = ", "), 
#          year = 2017) %>% 
#   mutate(approved = if_else(action_taken %in% c(1,2,8), "approved", "denied")) %>% 
#   group_by(st_cty_fips, state_county_name,approved) %>% 
#   summarise(total_mortgages = n()) %>%
#   pivot_wider(names_from = approved, values_from = total_mortgages) %>% 
#   mutate(approved = replace_na(approved, 0), denied = replace_na(denied, 0)) %>%
#   mutate(total_mortgages = approved + denied) %>% 
#   mutate(approval_rate = approved/total_mortgages, 
#          year = 2017)
# 
# hmda_total_mortg <- hmda_total_mortg %>% 
#   mutate(year = 2012)
# hmda <- bind_rows(hmda_total_mortg, hmda_2017_agg) 

save(hmda, file = "hmda_final.Rda")
hmda <- load("GEOG6305_data/hmda_final.Rda")
###################################
# Step 3: Branch Data 
###################################
branches_2012 <- read_csv("GEOG6305_data/data/ALL_2012/All_2012.csv") %>% 
  filter(BRSERTYP %in% c(11,12)) %>% 
  mutate(GEOID = str_pad(STCNTYBR, 5, "left", 0)) %>%
  group_by(YEAR, GEOID) %>% 
    summarise(num_branches2012 = n()) %>% 
  select(year = YEAR, GEOID, num_branches2012) %>% 
  ungroup()

branches_2017 <- read_csv("GEOG6305_data/data/ALL_2017/All_2017.csv") %>%
  filter(BRSERTYP %in% c(11,12)) %>% 
  mutate(GEOID = str_pad(STCNTYBR, 5, "left", 0)) %>%
  group_by(YEAR, GEOID) %>% 
  summarise(num_branches2017 = n()) %>% 
  select(year = YEAR, GEOID, num_branches2017) %>% ungroup()

sum(branches_2012$num_branches) - sum(branches_2017$num_branches)

change <- left_join(branches_2012 %>% select(-year), branches_2017 %>% select(-year), by = 'GEOID') %>% 
  mutate(change = num_branches2017 - num_branches2012) %>% 
  mutate(branch_loss = if_else(change < 0, 1, 0)) %>% 
  select(GEOID, branch_loss)

change_count <- left_join(branches_2012 %>% select(-year), branches_2017 %>% select(-year), by = 'GEOID') %>% 
  mutate(change = num_branches2017 - num_branches2012) %>% 
  mutate(branch_loss = if_else(change < 0, 1, 0)) %>% 
  select(GEOID, change)

branches <- bind_rows(branches_2012 %>% 
                        select(year, GEOID, branches = num_branches2012), branches_2017 %>% 
                        select(year, GEOID, branches = num_branches2017)) %>%
  left_join(change, by = "GEOID")


##################################
# Step 4: Make into spatial dataset
##################################
df <- left_join(acs, hmda, by = c('year','GEOID')) %>% 
  left_join(branches, by = c("year", "GEOID")) %>% 
  filter(!substr(GEOID,1,2) %in% c("02","15", "60", "66", "69", "72", "78")) %>% 
  mutate(br_per_pop = branches/total_pop) %>% 
  left_join(change_count, by = "GEOID")

counties <- counties()
df_final <- geo_join(spatial_data = counties, data_frame = df, by_sp = "GEOID", by_df = "GEOID", how = "inner")

st_write(df_final, "GEOG6305_data/df_final.shp")
df_final_2012 <- filter(df_final, year == 2012) 
df_final_2017 <- filter(df_final, year == 2017) 
st_write(df_final_2012, "GEOG6305_data/df_final_2012.shp")
st_write(df_final_2017, "GEOG6305_data/df_final_2017.shp")

################################# 
# Additional cleaning after
################################# 

#df_final<-st_read("GEOG6305_data/df_final.shp")

