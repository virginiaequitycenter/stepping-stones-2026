# Get and save Population Estimate data

# Libraries ----
library(tidyverse)
library(janitor)
library(tidycensus)
library(stringr)

# Import Population Estimate Data ----

# 2020-2024 data ----
# File layout document w/ variable definitions:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2024/CC-EST2024-AGESEX.pdf

# Set URL
url_va_2024_county <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/asrh/cc-est2024-agesex-51.csv")


# url_test <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/asrh/cc-est2024-syasex-51.csv")

# Read in csv
va_2024_county <- read_csv(url_va_2024_county) %>% 
  clean_names()

# test <- read_csv(url_test) %>% 
#   clean_names()

# Filter and rename years
# Per above document
#   The key for the YEAR variable is as follows:
#     1 = 4/1/2020 population estimates base
#     2 = 7/1/2020 population estimate
#     3 = 7/1/2021 population estimate
#     4 = 7/1/2022 population estimate
#     5 = 7/1/2023 population estimate
#     6 = 7/1/2024 population estimate

va_2024_county <- va_2024_county %>% 
  filter(year >= 2) %>% 
  mutate(year = year + (2020-2))

# Filter for Charlottesville and Albemarle
pop_est_2024_county <- va_2024_county %>% 
  filter(county %in% c("003", "540")) %>% 
  mutate(fips = paste0(state, county), .before = state) %>% 
  rename(name = ctyname) %>% 
  select(-c(sumlev, state, county, stname, median_age_tot, median_age_male, median_age_fem))

# Calculate state totals
pop_est_2024_state <- va_2024_county %>% 
  group_by(state, stname, year) %>% 
  summarize(across(c(popestimate:age85plus_fem), sum)) %>% 
  rename(fips = state,
         name = stname)

# 2010-2019 data ----
# File layout document w/ variable definitions:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-agesex.pdf

# Set URL
url_va_2019_county <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-51.csv")

# Read in csv
va_2019_county <- read_csv(url_va_2019_county) %>% 
  clean_names()

# Filter and rename years
# Per above document
# The key for the YEAR variable is as follows:
#   1 = 4/1/2010 Census population
#   2 = 4/1/2010 population estimates base
#   3 = 7/1/2010 population estimate
#   4 = 7/1/2011 population estimate
#   5 = 7/1/2012 population estimate
#   6 = 7/1/2013 population estimate
#   7 = 7/1/2014 population estimate
#   8 = 7/1/2015 population estimate
#   9 = 7/1/2016 population estimate
#   10 = 7/1/2017 population estimate
#   11 = 7/1/2018 population estimate
#   12 = 7/1/2019 population estimate

va_2019_county <- va_2019_county %>% 
  filter(year >= 3) %>% 
  mutate(year = year + (2010-3))

# Filter for Charlottesville and Albemarle
pop_est_2019_county <- va_2019_county %>% 
  filter(county %in% c("003", "540")) %>% 
  mutate(fips = paste0(state, county), .before = state) %>% 
  rename(name = ctyname) %>% 
  select(-c(sumlev, state, county, stname, median_age_tot, median_age_male, median_age_fem))

# Calculate state totals
pop_est_2019_state <- va_2019_county %>% 
  group_by(state, stname, year) %>% 
  summarize(across(c(popestimate:age85plus_fem), sum)) %>% 
  rename(fips = state,
         name = stname)

# Combine state and county tables ----
pop_est_2010_2024 <- rbind(pop_est_2019_county, pop_est_2024_county, pop_est_2019_state, pop_est_2024_state)

colnames(pop_est_2010_2024 %>% select(ends_with("tot")))

# Develop population age group data ----
# Get population age groups: pop_total,	pop_18over,	pop_under18	pop_10to17	pop_10to19
pop_est_2010_2024 <- pop_est_2010_2024 %>% 
  mutate(age517_tot = age513_tot + age1417_tot,
         age1013_tot = age513_tot - age59_tot,
         age1017_tot = age1013_tot + age1417_tot,
         age1019_tot = age1014_tot + age1519_tot,
         age18under_tot = popestimate - age18plus_tot)

# Save ----
write_csv(pop_est_2010_2024, "data/census_population_2010_2024.csv")

# Population Estimates by race/ethnicity ----

# Set years
years <- 2010:2024

# Select variables
vars_B01001_race <- c("B01001A_003", "B01001A_018", # m/f under 5 White Alone
                 "B01001A_004", "B01001A_019", # m/f 5 to 9 yrs White Alone
                 "B01001A_005", "B01001A_020", # m/f 10 to 14 yrs White Alone
                 "B01001A_006", "B01001A_021", # m/f 15 to 17 yrs White Alone
                 "B01001B_003", "B01001B_018", # m/f under 5 Black or African American Alone
                 "B01001B_004", "B01001B_019", # m/f 5 to 9 yrs Black or African American Alone
                 "B01001B_005", "B01001B_020", # m/f 10 to 14 yrs Black or African American Alone
                 "B01001B_006", "B01001B_021", # m/f 15 to 17 yrs Black or African American Alone
                 "B01001C_003", "B01001C_018", # m/f under 5 American Indian and Alaska Native Alone
                 "B01001C_004", "B01001C_019", # m/f 5 to 9 yrs American Indian and Alaska Native Alone
                 "B01001C_005", "B01001C_020", # m/f 10 to 14 yrs American Indian and Alaska Native Alone
                 "B01001C_006", "B01001C_021", # m/f 15 to 17 yrs American Indian and Alaska Native Alone
                 "B01001D_003", "B01001D_018", # m/f under 5 Asian Alone
                 "B01001D_004", "B01001D_019", # m/f 5 to 9 yrs Asian Alone
                 "B01001D_005", "B01001D_020", # m/f 10 to 14 yrs Asian Alone
                 "B01001D_006", "B01001D_021", # m/f 15 to 17 yrs Asian Alone
                 "B01001E_003", "B01001E_018", # m/f under 5 Native Hawaiian and Other Pacific Islander Alone
                 "B01001E_004", "B01001E_019", # m/f 5 to 9 yrs Native Hawaiian and Other Pacific Islander Alone
                 "B01001E_005", "B01001E_020", # m/f 10 to 14 yrs Native Hawaiian and Other Pacific Islander Alone
                 "B01001E_006", "B01001E_021", # m/f 15 to 17 yrs Native Hawaiian and Other Pacific Islander Alone
                 "B01001F_003", "B01001F_018", # m/f under 5 Some Other Race Alone
                 "B01001F_004", "B01001F_019", # m/f 5 to 9 yrs Some Other Race Alone
                 "B01001F_005", "B01001F_020", # m/f 10 to 14 yrs Some Other Race Alone
                 "B01001F_006", "B01001F_021", # m/f 15 to 17 yrs Some Other Race Alone
                 "B01001G_003", "B01001G_018", # m/f under 5 Two or More Races
                 "B01001G_004", "B01001G_019", # m/f 5 to 9 yrs Two or More Races
                 "B01001G_005", "B01001G_020", # m/f 10 to 14 yrs Two or More Races
                 "B01001G_006", "B01001G_021", # m/f 15 to 17 yrs Two or More Races
                 "B01001H_003", "B01001H_018", # m/f under 5 White Alone, Not Hispanic or Latino
                 "B01001H_004", "B01001H_019", # m/f 5 to 9 yrs White Alone, Not Hispanic or Latino
                 "B01001H_005", "B01001H_020", # m/f 10 to 14 yrs White Alone, Not Hispanic or Latino
                 "B01001H_006", "B01001H_021", # m/f 15 to 17 yrs White Alone, Not Hispanic or Latino
                 "B01001I_003", "B01001I_018", # m/f under 5 Hispanic or Latino
                 "B01001I_004", "B01001I_019", # m/f 5 to 9 yrs Hispanic or Latino
                 "B01001I_005", "B01001I_020", # m/f 10 to 14 yrs Hispanic or Latino
                 "B01001I_006", "B01001I_021" # m/f 15 to 17 yrs Hispanic or Latino
)

# Get ACS Data
popunder18county <- map_dfr(years,
                            ~get_acs(geography = "county",
                                     year = .x,
                                     state = "VA",
                                     county = c("003", "540"),
                                     variables = vars_B01001_race,
                                     output = "wide",
                                     survey = "acs5",
                                     cache = TRUE) %>%
                              mutate(year = .x, .after = GEOID))

popunder18state <- map_dfr(years,
                           ~get_acs(geography = "state",
                                    year = .x,
                                    state = "VA",
                                    variables = vars_B01001_race,
                                    output = "wide",
                                    survey = "acs5",
                                    cache = TRUE) %>%
                             mutate(year = .x, .after = GEOID))

# combine tables
popunder18 <- rbind(popunder18county, popunder18state)

library(purrr)
# Sum and rename variables
popunder18 <- popunder18 %>%
  mutate(pop_u5_white = B01001A_003E + B01001A_018E,
         pop_5to9_white = B01001A_004E + B01001A_019E,
         pop_10to14_white = B01001A_005E + B01001A_020E,
         pop_15to17_white = B01001A_006E + B01001A_021E,
         pop_under18_white = pop_u5_white + pop_5to9_white + pop_10to14_white + pop_15to17_white, 
         pop_u5_black = B01001B_003E + B01001B_018E,
         pop_5to9_black = B01001B_004E + B01001B_019E,
         pop_10to14_black = B01001B_005E + B01001B_020E,
         pop_15to17_black = B01001B_006E + B01001B_021E,
         pop_under18_black = pop_u5_black + pop_5to9_black + pop_10to14_black + pop_15to17_black,
         pop_u5_aian = B01001C_003E + B01001C_018E,
         pop_5to9_aian = B01001C_004E + B01001C_019E,
         pop_10to14_aian = B01001C_005E + B01001C_020E,
         pop_15to17_aian = B01001C_006E + B01001C_021E,
         pop_under18_aian = pop_u5_aian + pop_5to9_aian + pop_10to14_aian + pop_15to17_aian,
         pop_u5_asian = B01001D_003E + B01001D_018E,
         pop_5to9_asian = B01001D_004E + B01001D_019E,
         pop_10to14_asian = B01001D_005E + B01001D_020E,
         pop_15to17_asian = B01001D_006E + B01001D_021E,
         pop_under18_asian = pop_u5_asian + pop_5to9_asian + pop_10to14_asian + pop_15to17_asian,
         pop_u5_nhpi = B01001E_003E + B01001E_018E,
         pop_5to9_nhpi = B01001E_004E + B01001E_019E,
         pop_10to14_nhpi = B01001E_005E + B01001E_020E,
         pop_15to17_nhpi = B01001E_006E + B01001E_021E,
         pop_under18_nhpi = pop_u5_nhpi + pop_5to9_nhpi + pop_10to14_nhpi + pop_15to17_nhpi,
         pop_u5_other = B01001F_003E + B01001F_018E,
         pop_5to9_other = B01001F_004E + B01001F_019E,
         pop_10to14_other = B01001F_005E + B01001F_020E,
         pop_15to17_other = B01001F_006E + B01001F_021E,
         pop_under18_other = pop_u5_other + pop_5to9_other + pop_10to14_other + pop_15to17_other,
         pop_u5_multi = B01001G_003E + B01001G_018E,
         pop_5to9_multi = B01001G_004E + B01001G_019E,
         pop_10to14_multi = B01001G_005E + B01001G_020E,
         pop_15to17_multi = B01001G_006E + B01001G_021E,
         pop_under18_multi = pop_u5_multi + pop_5to9_multi + pop_10to14_multi + pop_15to17_multi,
         pop_u5_white_nonhisp = B01001H_003E + B01001H_018E,
         pop_5to9_white_nonhisp = B01001H_004E + B01001H_019E,
         pop_10to14_white_nonhisp = B01001H_005E + B01001H_020E,
         pop_15to17_white_nonhisp = B01001H_006E + B01001H_021E,
         pop_under18_white_nonhisp = pop_u5_white_nonhisp + pop_5to9_white_nonhisp + pop_10to14_white_nonhisp + pop_15to17_white_nonhisp,
         pop_u5_hisp = B01001I_003E + B01001I_018E,
         pop_5to9_hisp = B01001I_004E + B01001I_019E,
         pop_10to14_hisp = B01001I_005E + B01001I_020E,
         pop_15to17_hisp = B01001I_006E + B01001I_021E,
         pop_under18_hisp = pop_u5_hisp + pop_5to9_hisp + pop_10to14_hisp + pop_15to17_hisp) %>% 
  mutate(locality = str_remove(NAME, c(" County, Virginia| city, Virginia")), .after = GEOID) %>% 
  select(-c(NAME, starts_with("B01001")))

# Save ----
write_csv(popunder18, file = "data/pop_race_ethn_acs.csv")

