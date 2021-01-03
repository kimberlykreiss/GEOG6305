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
                                                         "B25004_004", 
                                                         "B15002_001", 
                                                         "B15002_015", "B15002_016", 
                                                         "B15002_017", "B15002_018", 
                                                         "B15002_032", "B15002_033", 
                                                         "B15002_034", "B15002_035"), year = year) %>%
    
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
                                variable == "B25004_004" ~ "vacsale",
                                variable == "B15002_015" ~ "male_ba", 
                                variable == "B15002_016" ~ "male_masters", 
                                variable == "B15002_017" ~ "male_prof", 
                                variable == "B15002_018" ~ "male_phd", 
                                variable == "B15002_032" ~ "female_ba", 
                                variable == "B15002_033" ~ "female_masters", 
                                variable == "B15002_034" ~ "female_prof", 
                                variable == "B15002_035" ~ "female_phd", 
                                variable == "B15002_001" ~ "total_25_pop"),
           year = year ) %>%
    pivot_wider(names_from = c(variable), values_from = c(estimate))

  acs <- bind_rows(temp_df, acs) %>% 
    select(-NAME)
  
}


acs <- acs %>%
#  rename(st_cty_fips=GEOID) %>% 
  mutate(ba_higher = rowSums(across(male_ba:female_phd))) %>% 
  mutate(share_ba_higher = ba_higher/total_25_pop) %>% 
  mutate_at(vars(white:pov, hunits:vacsale), funs(sh = ./total_pop))


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

#save(hmda, file = "hmda_final.Rda")
load("GEOG6305_data/hmda_final.Rda")
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


SOD_2017 <- read_csv("GEOG6305_data/data/ALL_2017/All_2017.csv") %>%
  filter(BRSERTYP %in% c(11,12))
SOD_2012 <- read_csv("GEOG6305_data/data/ALL_2012/All_2012.csv") %>%
  filter(BRSERTYP %in% c(11,12))

br_loc_changes <- SOD_2017 %>% 
  filter(!(UNINUMBR %in% SOD_2012$UNINUMBR)) %>% 
  write.csv(file = "GEOG6305_data/data/branch_location_changes.csv")

##################################
# Step 4: Make into spatial dataset
##################################
df <- left_join(acs, hmda, by = c('year','GEOID')) %>% 
  left_join(branches, by = c("year", "GEOID")) %>% 
  filter(!substr(GEOID,1,2) %in% c("02","15", "60", "66", "69", "72", "78")) %>% 
  mutate(br_per_pop = branches/total_pop) %>% 
  left_join(change_count, by = "GEOID") %>% 
  mutate_at(vars(total_mortgages,branch_loss), funs(sh = ./total_pop))

counties <- counties()
df_final <- geo_join(spatial_data = counties, data_frame = df, by_sp = "GEOID", by_df = "GEOID", how = "inner")

# st_write(df_final, "GEOG6305_data/df_final.shp")
 df_final_2012 <- filter(df_final, year == 2012) 
 df_final_2017 <- filter(df_final, year == 2017) 
# st_write(df_final_2012, "GEOG6305_data/df_final_2012.shp")
# st_write(df_final_2017, "GEOG6305_data/df_final_2017.shp")

################################# 
# Additional cleaning after
################################# 

#df_final<-st_read("GEOG6305_data/df_final.shp")

reg1 <- lm(total_mortgages ~ branch_loss + hunits + med_fam_inc + total_pop + black + hispanic, data = df_final_2017)
summary(reg1)

df_final_wide <- df_final %>% 
  mutate(case_when(year == 2012~12, 
                   year==2017~17)) %>%
  select(GEOID, year, b = branches, p = total_pop, bp = br_per_pop, bl= branch_loss_sh) %>% 
  st_drop_geometry() %>%
  pivot_wider(names_from = c(year), values_from = c(b, p, bl, bp), names_prefix = "y", names_sep="") %>% 
  geo_join(spatial_data = counties, data_frame = ., by_sp = "GEOID", by_df = "GEOID", how = "inner")

st_write(df_final_wide, "GEOG6305_data/df_final_wide_.shp")
summary(df_final_2017$branches)
summary(df_final_2017$change)
df_final_2017 <- df_final_2017 %>% 
  mutate(change2012_2017 = -change)
summary(df_final_2017$change2012_2017)

south2017 <- df_final_2017 %>% 
  filter(STATEFP %in% c("01", "05", "10", "12", "13", "21", "22", "24", "28", "37","40", "45", "47", 
                        "48", "51", "54"))
west2017 <- df_final_2017 %>% 
  filter(STATEFP %in% c("02", "04","06", "08", "15", "16", "30", "32", "35", "41", "49", "53", "56"))
ne2017 <- df_final_2017 %>% 
  filter(STATEFP %in% c("09", "23", "25", "33", "34", "36", "42", "44", "50", "10"))
midwest2017 <- df_final_2017 %>% 
  filter(STATEFP %in% c("17","18", "19", "20", "26", "27", "29", "31", "38", "39", "46", "55"))

st_write(south2017, "GEOG6305_data/south2017.shp")
st_write(west2017, "GEOG6305_data/west2017.shp")
st_write(ne2017, "GEOG6305_data/ne2017.shp")
st_write(midwest2017, "GEOG6305_data/midwest2017.shp")
