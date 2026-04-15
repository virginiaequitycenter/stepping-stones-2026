# Script for updating ACS 5-yr survey data for Stepping Stones 2026 report
# Gets new data and merges with prior years

# Included data updates for:
# - Sex by Age - replaced with pop_estimates.R
#   - Source: ACS Table B01001
#   - Used to create percents or rates for multiple metrics
# - High School Degree Attainment
#   - Source: ACS Table S1501 https://data.census.gov/table?q=S1501&tid=ACSST1Y2021.S1501
# - Youth Labor Force Participation and Unemployment
#   - Source: ACS Table B23001 https://data.census.gov/table?q=b23001&g=0500000US51003,51540
# - Children in Two-Parent Households
#   - Source: ACS Table B05009 https://data.census.gov/table/ACSDT5Y2023.B05009?q=B05009
# - Median Family income with children under 18 in household
#   - Source: ACS Table B19125  https://data.census.gov/table/ACSDT5Y2024.B19125?q=B19125&g=050XX00US51003,51540
#   - Inflation adjusted income: https://www.bls.gov/cpi/data.htm

# Libraries ----
library(tidycensus)
library(tidyverse)
library(janitor)
library(readxl)

# County FIPS codes 
county_codes <- c("003", "540") # Albemarle, Charlottesville FIPS Code

# Review ACS Tables and Variables ----
# Set ACS year
year <- 2024

# Get tables
vars_acs <- load_variables(year, "acs5", cache = TRUE)
vars_subject <- load_variables(year, "acs5/subject", cache = TRUE)
vars_profile <- load_variables(year, "acs5/profile", cache = TRUE)

## ...................................................
# Sex by Age ----
# Source: ACS Table B01001
# Tidycensus only support from 2005-2009 5 year ACS and on
# Get population age groups: pop_total,	pop_18over,	pop_under18	pop_10to17	pop_10to19

# Select variables
vars_B01001 <- c("B01001_001", 
                 "B01001_003", "B01001_027", # m/f under 5
                 "B01001_004", "B01001_028", # m/f 5 to 9 yrs
                 "B01001_005", "B01001_029", # m/f 10 to 14 yrs
                 "B01001_006", "B01001_030", # m/f 15 to 17 yrs
                 "B01001_007", "B01001_031" #m/f 18 to 19 yrs
                 )

# Get ACS data
acs_B01001_county <- map_df(2009:2024,
                           ~ get_acs(geography = "county",
                                     year = .x,
                                     state = "VA",
                                     county = county_codes,
                                     variables = vars_B01001,
                                     output = "wide",
                                     survey = "acs5",
                                     cache = TRUE) %>%
                             mutate(year = .x, .after = GEOID))

acs_B01001_state <- map_df(2009:2024,
                          ~ get_acs(geography = "state",
                                    year = .x,
                                    state = "VA",
                                    variables = vars_B01001,
                                    output = "wide",
                                    survey = "acs5",
                                    cache = TRUE) %>%
                            mutate(year = .x, .after = GEOID))

# combine tables
age_groups <- rbind(acs_B01001_county, acs_B01001_state) %>% 
  rename(fips = GEOID)

# Sum and rename variables
age_groups <- age_groups %>%
  mutate(pop_total = B01001_001E,
         pop_10to17 = B01001_005E + B01001_029E + B01001_006E + B01001_030E,
         pop_10to19 = pop_10to17 + B01001_007E + B01001_031E,
         pop_under18 = pop_10to17 + B01001_003E + B01001_027E + B01001_004E + B01001_028E,
         pop_18over = pop_total - pop_under18) %>% 
  mutate(locality = str_remove(NAME, c(" County, Virginia| city, Virginia")), .after = fips) %>% 
  select(-c(NAME, starts_with("B01001")))

# Keeping just ACS now
# # Read in prior data
# age_groups_prior <- read_csv("data/2023_data/pop_data_cdc.csv") %>% 
#   mutate(locality = case_when(fips == "51003" ~ "Albemarle",
#                               fips == "51540" ~ "Charlottesville",
#                               fips == "51" ~ "Virginia"), .after = fips) %>% 
#   select(fips, locality, year, pop_total = pop_tot, pop_10to17 = pop_1017, pop_10to19 = pop_1019,
#          pop_under18 = pop_17under, pop_18over)
# 
# # Bind tables
# age_groups_data <- rbind(age_groups_prior, age_groups)

# Save
write_csv(age_groups, "data/population_data_acs.csv")

## .......................................................
# High School Degree Attainment ----
# Source: ACS Table S1501 
# https://data.census.gov/table?q=S1501&tid=ACSST1Y2021.S1501

# Read in prior data
hs_deg_prior <- read_csv("data/2023_data/hsdegree_attainment.csv")

# Set variables
vars_S1501 <- c(pcthshigher = "S1501_C02_014",
                totpop25 = "S1501_C01_006")
# Get ACS data
acs_S1501_county <- map_df(2022:2024,
                            ~ get_acs(geography = "county",
                                      year = .x,
                                      state = "VA",
                                      county = county_codes,
                                      variables = vars_S1501,
                                      output = "wide",
                                      survey = "acs5",
                                      cache = TRUE) %>%
                              mutate(year = .x))

acs_S1501_state <- map_df(2022:2024,
                           ~ get_acs(geography = "state",
                                     year = .x,
                                     state = "VA",
                                     variables = vars_S1501,
                                     output = "wide",
                                     survey = "acs5",
                                     cache = TRUE) %>%
                             mutate(year = .x))

# merge tables
hs_degree <- rbind(acs_S1501_county,acs_S1501_state) %>% 
  select(-c(pcthshigherM, totpop25M))

hs_deg_data <- rbind(hs_deg_prior, hs_degree) %>% 
  mutate(NAME = str_remove(NAME, ", Virginia"),
         NAME = str_to_title(NAME))

# Save 
write_csv(hs_deg_data, "data/hsdegree_attainment.csv")

## .......................................................
# Youth Labor Force Participation and Unemployment ----
# Source: ACS Table B23001 
# https://data.census.gov/table?q=b23001&g=0500000US51003,51540

# Read in prior data
youth_employ_prior <- read_csv("data/2023_data/youth_employment_status.csv") %>% 
  select("geoid","name","year", "youth_total_sum", "youth_labor_sum", "youth_unemployed_sum", "laborforce_rate", "unemployment_rate")

# Set variables
# B23001_003 - male 16-19 total pop
# B23001_004 - male 16-19 in labor force 
# B23001_008 - male 16-19 unemployed 
# B23001_089 - female 16-19 total pop
# B23001_090 - female 16-19 in labor force 
# B23001_094 - female 16-19 unemployed

vars_B23001 <- c("B23001_003", "B23001_089",
                 "B23001_004", "B23001_090",
                 "B23001_008", "B23001_094")
# Get ACS data
acs_B23001_county <- map_df(2022:2024,
                           ~ get_acs(geography = "county",
                                     year = .x,
                                     state = "VA",
                                     county = county_codes,
                                     variables = vars_B23001,
                                     output = "wide",
                                     survey = "acs5",
                                     cache = TRUE) %>%
                             mutate(year = .x))

acs_B23001_state <- map_df(2022:2024,
                          ~ get_acs(geography = "state",
                                    year = .x,
                                    state = "VA",
                                    variables = vars_B23001,
                                    output = "wide",
                                    survey = "acs5",
                                    cache = TRUE) %>%
                            mutate(year = .x))

# combine tables
youth_employ <- rbind(acs_B23001_county, acs_B23001_state)

# Sum and rename variables
youth_employ <- youth_employ %>%
  mutate(total_16_19yr = B23001_003E + B23001_089E,
         laborforce_16_19yr = B23001_004E + B23001_090E,
         unemployed_16_19yr = B23001_008E + B23001_094E) %>%
  select(-starts_with("B23001"))

# Create percents
youth_employ <- youth_employ %>%
  mutate(per_laborforce_16_19yr = (laborforce_16_19yr/total_16_19yr)*100,
         per_unemployed_16_19yr = (unemployed_16_19yr/laborforce_16_19yr)*100)

# rename prior data columns
names(youth_employ_prior) <- c("GEOID", "NAME", "year", "total_16_19yr", "laborforce_16_19yr", "unemployed_16_19yr", "per_laborforce_16_19yr", "per_unemployed_16_19yr")

# merge with prior 
youth_employ_data <- rbind(youth_employ_prior, youth_employ) %>% 
  mutate(NAME = str_remove(NAME, ", Virginia"),
         NAME = str_to_title(NAME))

# Save
write_csv(youth_employ_data, "data/youth_employment_status.csv")

## .......................................................
# Children in Two-Parent Households ----
# Source: ACS Table B05009 https://data.census.gov/table/ACSDT5Y2023.B05009?q=B05009

# Read in prior data
twoparent_hh_prior <- read_csv("data/2023_data/children_twoparents.csv")

# Set variables
# B05009_001 = Total under 18 yrs
# B05009_003 = Children under 6 years living with two parents
# B05009_021 = Children 6 - 17 years living with two parents

vars_B05009 <- c("B05009_001", "B05009_003","B05009_021")

# Get ACS data
acs_B05009_county <- map_df(2022:2024,
                            ~ get_acs(geography = "county",
                                      year = .x,
                                      state = "VA",
                                      county = county_codes,
                                      variables = vars_B05009,
                                      output = "wide",
                                      survey = "acs5",
                                      cache = TRUE) %>%
                              mutate(year = .x))

acs_B05009_state <- map_df(2022:2024,
                           ~ get_acs(geography = "state",
                                     year = .x,
                                     state = "VA",
                                     variables = vars_B05009,
                                     output = "wide",
                                     survey = "acs5",
                                     cache = TRUE) %>%
                             mutate(year = .x))

# combine tables
twoparent_hh <- rbind(acs_B05009_county, acs_B05009_state)

# Sum and rename variables
twoparent_hh <- twoparent_hh %>%
  mutate(child_under6_twoparent_hh = B05009_003E,
         child_6to17_twoparent_hh = B05009_021E,
         total_child_twoparent_hh = B05009_003E + B05009_021E,
         total_children = B05009_001E) %>%
  select(-starts_with("B05009"))

# Create percents
twoparent_hh <- twoparent_hh %>%
  mutate(per_child_twoparent_hh = (total_child_twoparent_hh/total_children)*100)

# Wrangle prior data to match
twoparent_hh_prior <- twoparent_hh_prior %>% 
  mutate(percent = percent * 100) %>% 
  rename(child_under6_twoparent_hh = todlerestimate,
         child_6to17_twoparent_hh = childestimate,
         total_children = total,
         total_child_twoparent_hh = sum,
         per_child_twoparent_hh = percent) %>% 
  select("GEOID", "NAME", "year", "child_under6_twoparent_hh", "child_6to17_twoparent_hh","total_child_twoparent_hh", "total_children", "per_child_twoparent_hh")

# merge with prior 
twoparent_hh_data <- rbind(twoparent_hh_prior, twoparent_hh) %>% 
  mutate(NAME = str_remove(NAME, ", Virginia"),
         NAME = str_to_title(NAME))

# Save
write_csv(twoparent_hh_data, "data/children_twoparent_hh.csv")

## .......................................................
# Median Family income with children under 18 in household ----
# Source ACS 5-yr for 2015-2019, 2014-2018, 2013-2017, 2012-2016, 2011-2015, 2010 - 2014:
# KidsCount: https://datacenter.aecf.org/data/tables/9184-median-income-of-families-with-own-children-in-household?loc=48&loct=5#detailed/5/6813,6836/true/1983,1692,1691,1607,1572,1522/any/18208
# Source for 2020 on: ACS Table B19125  https://data.census.gov/table/ACSDT5Y2024.B19125?q=B19125&g=050XX00US51003,51540

# Read in prior data from KidsCount
# Download raw data from https://datacenter.aecf.org/data/tables/9184-median-income-of-families-with-own-children-in-household?loc=48&loct=5#detailed/5/6813,6836/true/1983,1692,1691,1607,1572,1522/any/18208
# Save to download_data/

medinc_fam_prior <- read_excel("download_data/kidscount_med_income_w_children.xlsx") %>% 
  clean_names()

# Filter for Albemarle, Charlottesville, Viriginia
medinc_fam_prior <- medinc_fam_prior %>% 
  filter(location %in% c("Virginia", "Albemarle", "Charlottesville")) %>% 
  mutate(year = as.integer(str_sub(time_frame, start = -4)))

# Get ACS data for 2020-2024 surveys
# B19125_002 = Median Family Income With own children of the householder under 18 years

acs_B19125_county <- map_df(2020:2024,
                            ~ get_acs(geography = "county",
                                      year = .x,
                                      state = "VA",
                                      county = county_codes,
                                      variables = "B19125_002",
                                      output = "wide",
                                      survey = "acs5",
                                      cache = TRUE) %>%
                              mutate(year = .x))

acs_B19125_state <- map_df(2020:2024,
                           ~ get_acs(geography = "state",
                                     year = .x,
                                     state = "VA",
                                     variables = "B19125_002",
                                     output = "wide",
                                     survey = "acs5",
                                     cache = TRUE) %>%
                             mutate(year = .x))

# combine tables
medinc_fam <- rbind(acs_B19125_county, acs_B19125_state)

# rename and select
medinc_fam <- medinc_fam %>% 
  mutate(medinc_w_children = B19125_002E,
         NAME = str_remove(NAME, ", Virginia"),
         NAME = str_to_title(NAME)) %>% 
  select(-starts_with("B19125"))

# Wrangle prior to match
medinc_fam_prior <- medinc_fam_prior %>% 
  mutate(GEOID = case_when(location == "Virginia" ~ "51",
                           location == "Albemarle" ~ "51003",
                           location == "Charlottesville" ~ "51540"),
         NAME = case_when(location == "Albemarle" ~ "Albemarle County",
                          location == "Charlottesville" ~ "Charlottesville City",
                          .default = location),
         medinc_w_children = as.numeric(data)) %>% 
  select(GEOID, NAME, year, medinc_w_children)

# merge with prior 
medinc_fam_data <- rbind(medinc_fam_prior, medinc_fam)

# Adjust income for inflation
# Consumer Price Index measure for inflation-adjusted dollar values
# Go to: https://www.bls.gov/cpi/data.htm
# (1) Under Databases section -> Databases, row for All Urban Consumers (Current Series) -> click "Top picks"
# (2) Select: "U.S City Average, All Items", click retrieve data
# (3) Click "More formatting options
# (4) Under "Select view of the data" select: "Table Format" and check "Original data value"
# (5) Under "Select the time frame for your data": Specify year range: From: 2014 To: 2026;
#                                                 Select one time period: Annual Data
# (6) Under "Output type" select: "HTML table"
# (7) Click "Retrieve Data"
# (8) Download xlsx to download_data folder

# Citation: 
# U.S. Bureau of Labor Statistics, "All items in U.S. city average, all urban consumers, not seasonally adjusted", Series Id CUUR0000SA0 (accessed April 15, 2026). 

bls_ann <- read_excel("download_data/SeriesReport-20260415133403_610a5f.xlsx", skip = 10)

# Revise to base year 2014
# (Default is 1982-1984)
base2014 <- as_vector(bls_ann %>% filter(Year == 2014) %>% select(Annual))

bls_ann <- bls_ann %>% 
  mutate(cpi14 = Annual/base2014*100)

# merge with dollar-valued data frame
# calculate adjusted dollar values
#   mutate(adj_inc = inc/cpi10*100)

medinc_fam_data <- medinc_fam_data %>% 
  left_join(bls_ann %>% select(year = Year, cpi14)) %>% 
  mutate(adj_medinc_w_children = (medinc_w_children/cpi14)*100)

# Save
write_csv(medinc_fam_data, "data/median_income_w_children.csv")

## .......................................................





