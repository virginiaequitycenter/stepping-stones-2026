# Get and save Population Estimate data

# Libraries ----
library(tidyverse)
library(janitor)

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

