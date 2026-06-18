# Script for getting and cleaning measures for supplement to Stepping Stones 2026 report

# Included data: ----
# Children Living below the Poverty Threshold by Race/Ethnicity
  # Source: ACS Tables B17001A-I
# Students Identified as Economically Disadvantaged by Race/Ethnicity
  # Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
# Students Eligible for Special Education Services by Race/Ethnicity
  # Source: https://p1pe.doe.virginia.gov/buildatable/dec1
# Out-of-School Suspensions by Race/Ethnicity
# High school graduation rate by Race/Ethnicity, Economically Disadvantaged
  # Source: https://p1pe.doe.virginia.gov/buildatable/cohortgraduation
# Post-Secondary Enrollment by Race/Ethnicity, Economically Disadvantaged
  # Source: https://p1pe.doe.virginia.gov/postsec_public/
# Youth (15-24) Labor Force Participation by Census Tract
# Youth (15-24) in the Labor Force Experiencing Unemployment by Census Tract
# Low Birth-Weight Infants by Race/Ethnicity
# Infant Deaths by Race/Ethnicity
# Children in foster care by Race/Ethnicity
# Child Abuse/Neglect Investigations/Assessments by Race/Ethnicity

# Libraries ----
library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

## .......................................................
# Children Living below the Poverty Threshold by Race/Ethnicity ----
# Source: ACS Tables B17001A-I

# Set years
years <- 2010:2024

# Get ACS Data
# get data function
acs_years_func <- function(tablename){
  acs_B17001_county <- map_dfr(years,
                                ~get_acs(geography = "county",
                                         year = .x,
                                         state = "51",
                                         county = c("003", "540"),
                                         table = tablename,
                                         output = "wide",
                                         survey = "acs5",
                                         cache = TRUE) %>%
                                  mutate(year = .x, .after = GEOID))
  
  acs_B17001_state <- map_dfr(years,
                             ~get_acs(geography = "state",
                                      year = .x,
                                      state = "51",
                                      table = tablename,
                                      output = "wide",
                                      survey = "acs5",
                                      cache = TRUE) %>%
                               mutate(year = .x, .after = GEOID))
  
  # combine tables
  acs_B17001 <- rbind(acs_B17001_county, acs_B17001_state)

  acs_B17001 <- acs_B17001 %>%
    rename_with(~ str_replace(., tablename, "var"), contains(tablename)) %>% 
    mutate(under18_m = var_004E + var_005E + var_006E + var_007E + var_008E + var_009E,
           under18_f = var_018E + var_019E + var_020E + var_021E + var_022E + var_023E,
           pov_under18 = under18_m + under18_f) %>% 
    mutate(locality = str_remove(NAME, c(" County, Virginia| city, Virginia")), .after = GEOID) %>% 
    select(-c(NAME, starts_with("var_"), starts_with("under18_")))
  
  acs_B17001
}

# White Alone
acs_B17001A <- acs_years_func("B17001A") %>% 
  dplyr::rename(pov_under18_white = pov_under18)

# Black Alone
acs_B17001B <- acs_years_func("B17001B") %>% 
  dplyr::rename(pov_under18_black = pov_under18)

# Asian
acs_B17001D <- acs_years_func("B17001D") %>% 
  dplyr::rename(pov_under18_asian = pov_under18)

# Multi
acs_B17001G <- acs_years_func("B17001G") %>% 
  dplyr::rename(pov_under18_multi = pov_under18)

# White Non Hispanic
acs_B17001H <- acs_years_func("B17001H") %>% 
  dplyr::rename(pov_under18_white_nonhisp = pov_under18)

# Hispanic
acs_B17001I <- acs_years_func("B17001I") %>% 
  dplyr::rename(pov_under18_hisp = pov_under18)

# All children
acs_B17001 <- acs_years_func("B17001")

# Join tables
acs_B17001_all <- acs_B17001 %>% 
  left_join(acs_B17001A) %>% 
  left_join(acs_B17001B) %>% 
  left_join(acs_B17001D) %>% 
  left_join(acs_B17001G) %>% 
  left_join(acs_B17001H) %>% 
  left_join(acs_B17001I)

# Bring in population data
pop_under18 <- read_csv("data/pop_race_ethn_acs.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  select(GEOID, locality, year, starts_with("pop_under18")) %>% 
  select(-c("pop_under18_aian","pop_under18_nhpi","pop_under18_other"))

age_groups <- read_csv("data/population_data_acs.csv") %>% 
  mutate(GEOID = as.character(fips)) %>% 
  select(GEOID, locality, year, pop_under18)

# Join population data
child_pov <- acs_B17001_all %>% 
  left_join(age_groups) %>% 
  left_join(pop_under18)

# Pivot
child_pov_rates <- child_pov %>% 
  pivot_longer(c(starts_with("pov_"), starts_with("pop")), names_to = c("name","group"), names_pattern = "(.*?)_(.*)") %>%      
  pivot_wider(id_cols = c(GEOID, locality, year, group), names_from = name, values_from = value, names_repair = "check_unique") %>% 
  left_join(acs_B17001) %>% 
  rename(pov_num_group = pov,
         population_num_group = pop,
         pov_num_under18 = pov_under18)

# Create poverty rates
child_pov_rates <- child_pov_rates %>%
  mutate(pov_rate_bygroup = round(pov_num_group/population_num_group * 100, 2),
         pov_rate_byallpov = round(pov_num_group/pov_num_under18 * 100, 2))

# # Create poverty rates
# child_pov_rates <- child_pov %>% 
#   mutate(across(pov_under18:pov_under18_hisp, ~ round((.x / get(str_replace(cur_column(), "pov", "pop"))) * 100, 2), .names = "{col}_rate")) 

# Save
write_csv(child_pov_rates, file = "data/child_poverty_by_race.csv")

## .......................................................
# Fall Membership by Race/Ethn ----

# Read in data
fallmem <- read_csv("download_data/fall_membership_statistics_count_2010_2025.csv") %>% 
  clean_names() %>% 
  rename(total_students = total_count) %>% 
  select(-c(division_number, ft_count, pt_count))

fallmem_race <- read_csv("download_data/fall_membership_statistics_race_2010_2025.csv") %>% 
  clean_names() %>% 
  rename(total_byrace = total_count) %>% 
  select(-c(division_number, ft_count, pt_count))

## .......................................................
# Students Identified as Economically Disadvantaged by Race/Ethnicity ----
#   Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership

# Read in & clean data
disadvan_race <- read_csv("download_data/fall_membership_statistics_disadvan_race.csv") %>% 
  clean_names()

disadvan_race <- disadvan_race %>% 
  mutate(disadvan_race_count = as.numeric(str_remove_all(total_count, ","))) %>% 
  select(-c(division_number, disadvantaged, ft_count, pt_count, total_count))

disadvan_all <- read_csv("download_data/fall_membership_statistics_disadvan_2010_2025.csv") %>% 
  clean_names()

disadvan_all <- disadvan_all %>% 
  rename(disadvan_total_count = total_count) %>% 
  select(-c(division_number, disadvantaged, ft_count, pt_count))

# Join tables
disadvan <- disadvan_race %>% 
  left_join(disadvan_all) %>% 
  left_join(fallmem_race) %>% 
  left_join(fallmem)

# Create rates
disadvan_rates <- disadvan %>% 
  mutate(disadvan_percent_byrace = round(disadvan_race_count/total_byrace * 100, 2),
         disadvan_percent_byalldisadv = round(disadvan_race_count/disadvan_total_count * 100, 2),
         division_name = ifelse(is.na(division_name), "Virginia", division_name))

# Save
write_csv(disadvan_rates, file = "data/econ_disadvan_students_by_race.csv")

## .......................................................
# Students Eligible for Special Education Services by Race/Ethnicity ----
# Source: https://p1pe.doe.virginia.gov/buildatable/dec1

# Read in & clean data
sped_race <- read_csv("download_data/dec_1_statistics_race_2010_2024.csv") %>% 
  clean_names()

sped_race <- sped_race %>% 
  mutate(sped_race_count = as.numeric(str_remove_all(total_count, ","))) %>% 
  select(-c(division_number, total_count))

sped_all <- read_csv("download_data/dec_1_statistics_all_2010_2024.csv") %>% 
  clean_names()

sped_all <- sped_all %>% 
  rename(sped_total_count = total_count) %>% 
  select(-c(division_number))

# Join tables
sped <- sped_race %>% 
  left_join(sped_all) %>% 
  left_join(fallmem_race) %>% 
  left_join(fallmem)

# Create rates
sped_rates <- sped %>% 
  mutate(sped_percent_byrace = round(sped_race_count/total_byrace * 100, 2),
         sped_percent_byallsped = round(sped_race_count/sped_total_count * 100, 2),
         division_name = ifelse(is.na(division_name), "Virginia", division_name))

# Save
write_csv(sped_rates, file = "data/special_ed_services_by_race.csv")

## .......................................................
# High school graduation rate by Race/Ethnicity, Economically Disadvantaged ----
# Source: https://p1pe.doe.virginia.gov/buildatable/cohortgraduation

# Read in data
grad_rates_race <- read_csv("download_data/cohort_statistics_race_2010_2025.csv") %>% 
  clean_names() %>% 
  rename(group = race)

grad_rates_disadvan <- read_csv("download_data/cohort_statistics_disadvan_2010_2025.csv") %>% 
  clean_names() %>% 
  mutate(group = "Economically Disadvantaged", .after = "rate_type") %>% 
  select(-disadvantaged)

# Merge tables
grad_rates_raw <- rbind(grad_rates_race, grad_rates_disadvan)

# Clean data
grad_rates <- grad_rates_raw %>% 
  mutate(across(c(graduation_rate,completion_rate,dropout_rate),  ~ as.numeric(str_remove(.x, "%"))),
         across(c(students_in_cohort,total_graduates), ~ as.numeric(str_remove_all(.x, ","))),
         division_name = ifelse(is.na(division_name), "Virginia", division_name),
         division_number = ifelse(is.na(division_number), 0, division_number))

# Save
write_csv(grad_rates, file = "data/graduation_rates_race_econ_disadvan.csv")

## .......................................................
# Post-Secondary Enrollment by Race, by Economically Disadvantaged ----
# Available at https://p1pe.doe.virginia.gov/postsec_public/
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/postsec_public/) 
#         (2) Select Report: State Fiscal Stabilization Fund Indicator (C)(11) Report
#         (3) For Each FGI Cohort Year: 2021; 2022; 2023
#             FGI Cohort Year: 2021; 2022; 2023
#             Graduation Rate Type: Division; State
#             School Division Results Selection: Charlottesville, Albemarle (individually) 
#             State Results Selection (separate from division results)
#         (3) View Excel to save (save individual excel to relevant download_data/sfsf_*/)

# Read in data
cvillefiles <- list.files(path = "download_data/sfsf_cville", pattern = "*.xlsx", full.names = TRUE)
albfiles <- list.files(path = "download_data/sfsf_alb", pattern = "*.xlsx", full.names = TRUE)
vafiles <- list.files(path = "download_data/sfsf_va", pattern = "*.xlsx", full.names = TRUE)

# rename columns to be identical across years/files
colnames = c("group", "cohortsize", "enrolledany", "percentany", 
             "enrolled4pub", "percent4pub", "enrolled4priv", "percent4priv",
             "enrolled2yr", "pecen2yr")

## Charlottesville
years <- map_dfr(cvillefiles, ~read_excel(.x, sheet = 1, range = "A3")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

cville_data <- map(cvillefiles, ~read_excel(.x, sheet = 1, range = "A7:J18"))
names(cville_data) <- years # add year as names for list

cville_data <- map(cville_data, ~ rename_with(., ~ colnames))

# bind into data frame
cvl <- bind_rows(cville_data, .id = "year") %>% 
  mutate(locality = "Charlottesville")


## albemarle
years <- map_dfr(albfiles, ~read_excel(.x, sheet = 1, range = "A3")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

alb_data <- map(albfiles, ~read_excel(.x, sheet = 1, range = "A7:J18"))
names(alb_data) <- years # add year as names for list

alb_data <- map(alb_data, ~ rename_with(., ~ colnames))

# bind into data frame
alb <- bind_rows(alb_data, .id = "year") %>% 
  mutate(locality = "Albemarle")

## virginia
years <- map_dfr(vafiles, ~read_excel(.x, sheet = 1, range = "A3")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

va_data <- map(vafiles, ~read_excel(.x, sheet = 1, range = "A7:J18"))
names(va_data) <- years # add year as names for list

va_data <- map(va_data, ~ rename_with(., ~ colnames))

# bind into data frame
va <- bind_rows(va_data, .id = "year") %>% 
  mutate(locality = "Virginia")

# Bind all and filter
post_second_ed <- rbind(cvl, alb, va) %>% 
  mutate(across(-c("group", "locality"), as.numeric)) %>% 
  mutate(enrolled4yr = case_when(is.na(enrolled4pub)& !is.na(enrolled4priv) ~ enrolled4priv,
                                 is.na(enrolled4priv)& !is.na(enrolled4pub) ~ enrolled4pub,
                                 is.na(enrolled4pub)& is.na(enrolled4priv) ~ NA,
                                 .default = enrolled4pub + enrolled4priv), .after = percent4priv) %>% 
  mutate(percent4yr = round(enrolled4yr/cohortsize *100, 2), .after = enrolled4yr)

# Save
write_csv(post_second_ed, "data/postsecond_education_race_econ_disadvan.csv")

## .......................................................
# Youth (16-19) Labor Force Participation & Unemployment by Census Tract ----
# Source: ACS Table B23001 
# https://data.census.gov/table?q=b23001&g=0500000US51003,51540

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
acs_B23001_tract <- map_df(2010:2024,
                            ~ get_acs(geography = "tract",
                                      year = .x,
                                      state = "VA",
                                      county = c("003", "540"),
                                      variables = vars_B23001,
                                      output = "wide",
                                      survey = "acs5",
                                      cache = TRUE) %>%
                              mutate(year = .x))

# Sum and rename variables
youth_employ_tract <- acs_B23001_tract %>%
  mutate(total_16_19yr = B23001_003E + B23001_089E,
         laborforce_16_19yr = B23001_004E + B23001_090E,
         unemployed_16_19yr = B23001_008E + B23001_094E) %>%
  select(-starts_with("B23001"))

# Create percents
youth_employ_tract <- youth_employ_tract %>%
  mutate(per_laborforce_16_19yr = round((laborforce_16_19yr/total_16_19yr)*100,2),
         per_unemployed_16_19yr = round((unemployed_16_19yr/laborforce_16_19yr)*100,2))

# Bring in tract names
tractnames <- read_csv("data/regional_tractnames.csv") %>% 
  mutate(GEOID = as.character(GEOID))

# Join tractnames
youth_employ_tract <- youth_employ_tract %>% 
  left_join(tractnames)

# Save
write_csv(youth_employ_tract, "data/youth_employment_status_by_tract.csv")


library(leaflet)
library(tigris)
library(sf)
library(rcartocolor)

county_geo <- counties(state = "VA") %>% 
  subset(COUNTYFP %in% c("003", "540")) %>% 
  st_transform(crs = 4326)
tracts_geo <- tracts(state = "VA", county = c("003", "540"))
tracts_geo <- tracts_geo %>% st_transform(tracts_geo, crs = 4326) 

youth_employ_map <- youth_employ_tract %>% 
  filter(year == 2022) %>% 
  left_join(tracts_geo, by = join_by(GEOID == GEOID)) %>%
  st_as_sf()

pal_ahdi <- carto_pal(6, "Purp")
# pal_ahdi <- c("white", "#F3E0F7", "#DBBAED", "#B998DD", "#9178C4", "#63589F")
map_pal <- colorBin(pal_ahdi, domain = c(0,100), bins = c(0, 0.1, 20, 40, 60, 80, 100), reverse = FALSE, na.color = "#e0e0e0")
map_pal_rev <- colorBin(pal_ahdi, domain = c(0,100),bins = c(0, 0.1, 20, 40, 60, 80, 100), reverse = TRUE, na.color = "#e0e0e0")

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = youth_employ_map,
              fillColor = ~map_pal(per_laborforce_16_19yr),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.7,
              # highlight = highlightOptions(fillOpacity = 1),
              popup = paste0("Place: ", youth_employ_map$tractnames, "<br>",
                             "Census Tract: ", youth_employ_map$NAME.x, "<br>",
                             "Percent: ", youth_employ_map$per_laborforce_16_19yr, "")) %>%
  addPolygons(data= county_geo, color = "black",
              fill = FALSE,
              weight = 1.5) %>%
  addLegend("bottomright", pal = map_pal_rev, values = youth_employ_map$per_laborforce_16_19yr, 
            title = "Percent 16-19 year olds<br/>in the Labor Force", opacity = 0.7,
            # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE), suffix = "%"),
            labFormat = function(type, breaks) {
              return(c("80 - 100%", "60 - 80%", "40 - 60%", "20 - 40%", "1 - 20%", "0%"))
            })

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = youth_employ_map,
              fillColor = ~map_pal(per_unemployed_16_19yr),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.7,
              highlight = highlightOptions(fillOpacity = 1),
              popup = paste0("Place: ", youth_employ_map$tractnames, "<br>",
                             "Census Tract: ", youth_employ_map$NAME.x, "<br>",
                             "Percent: ", youth_employ_map$per_unemployed_16_19yr, "")) %>%
  addPolygons(data= county_geo, color = "black",
              fill = FALSE,
              weight = 1.5) %>%
  addLegend("bottomright", pal = map_pal_rev, values = youth_employ_map$per_unemployed_16_19yr, 
            title = "Percent 16-19 year olds<br/>in the Labor Force & Unemployed", opacity = 0.7,
            # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE), suffix = "%"),
            labFormat = function(type, breaks) {
              return(c("80 - 100%", "60 - 80%", "40 - 60%", "20 - 40%", "1 - 20%", "0%"))
            })

## .......................................................
# Low Birth-Weight Infants by Race/Ethnicity ----
lbw_raw <- read_excel("download_data/VDH_LBW_IMR_June_2026.xlsx", sheet = 2, skip = 1)

lbw <- lbw_raw %>% 
  clean_names()
  # mutate(across(low_birth_weight_births:percentage_with_low_birth_weight, ~ as.numeric(.x)))

# Save
write_csv(lbw, "data/low_birthweight_race_ethn.csv")

## .......................................................
# Infant Deaths by Race/Ethnicity ----
imr_raw <- read_excel("download_data/VDH_LBW_IMR_June_2026.xlsx", sheet = 3, skip = 1)

imr <- imr_raw %>% 
  clean_names()
  # mutate(across(infant_deaths:infant_mortality_rate, ~ as.numeric(.x)))

# Save
write_csv(imr, "data/infant_mortality_race_ethn.csv")

## .......................................................
# Children in Foster Care by Race/Ethnicity ----
# Source: 2024-2025: https://www.dss.virginia.gov/research-and-planning/reports-data/fc-data-reports/
# Source: 2021-2023: https://www.dss.virginia.gov/archives/research-and-planning-archives/

# read in files 
fcfiles <- list.files(path = "download_data/dssfc", pattern = "*.xlsx", full.names = TRUE)

fcyears <- map_dfr(fcfiles, ~read_excel(.x, sheet = 1, range = "B1")) %>% 
  names() %>% 
  str_extract(pattern = "[0-9]{4}")

fc_data <- map(fcfiles, ~read_excel(.x, sheet = 1, range = "A4:AA144"))
names(fc_data) <- fcyears # add year as names for list

# bind into data frame
fc_all <- bind_rows(fc_data, .id = "year")

# filter
fc_all <- fc_all %>% 
  filter(LOCALITY %in% c("Albemarle", "Charlottesville", "STATE")) %>% 
  mutate(year = as.numeric(year),
         LOCALITY = ifelse(LOCALITY == "STATE", "Virginia", LOCALITY),
         FIPS = case_when(
           FIPS == 3 ~ "51003",
           FIPS == 540 ~ "51540",
           FIPS == 0 ~ "51",
           is.na(FIPS) ~ "51"
         )) %>% 
  select(-starts_with("Percent")) %>% 
  select(-c("MALE", "FEMALE", "Gender Unknown", "Am Indian Alaskan Native", "Hawaiian Pacific Islander", "Race Unknown")) %>% 
  clean_names() %>% 
  rename(GEOID = fips) %>% 
  mutate(fc_under18 = total_children_in_care, .after = total_children_in_care) %>% 
  rename_with(~ paste0("fc_under18_", .x), .cols = black:hispanic)

# Bring in population data
pop_under18 <- read_csv("data/pop_race_ethn_acs.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  select(GEOID, locality, year, starts_with("pop_under18")) %>% 
  select(-c("pop_under18_aian", "pop_under18_nhpi", "pop_under18_white_nonhisp", "pop_under18_other"))

age_groups <- read_csv("data/population_data_acs.csv") %>% 
  mutate(GEOID = as.character(fips)) %>% 
  select(GEOID, locality, year, pop_under18)

# Join population data
fc <- fc_all %>% 
  left_join(age_groups, by = join_by(GEOID == GEOID, year == year, locality == locality)) %>% 
  left_join(pop_under18, by = join_by(GEOID == GEOID, year == year, locality == locality))

# Pivot
fc_rates <- fc %>% 
  rename(fc_under18_multiracial = fc_under18_multi_race,
         pop_under18_multiracial = pop_under18_multi,
         pop_under18_hispanic = pop_under18_hisp) %>% 
  pivot_longer(c(starts_with("fc_"), starts_with("pop")), names_to = c("name","group"), names_pattern = "(.*?)_(.*)") %>%      
  pivot_wider(id_cols = c(GEOID, locality, region, year, group, total_children_in_care), names_from = name, values_from = value, names_repair = "check_unique") %>% 
  rename(fc_count_group = fc,
         population_count_group = pop) %>% 
  mutate(group = case_when(group == "under18" ~ "all",
                           startsWith(group, "under18_") ~ str_remove(group, "under18_")))

# Create poverty rates
fc_rates <- fc_rates %>%
  mutate(fc_per_bygroup = round(fc_count_group/population_count_group * 100, 2),
         fc_per_byallfc = round(fc_count_group/total_children_in_care * 100, 2),
         fc_rate_bygroup = round(fc_count_group/population_count_group * 1000, 2),
         fc_rate_byallfc = round(fc_count_group/total_children_in_care * 1000, 2))

# Save
write_csv(fc_rates, "data/foster_care_race_ethn.csv")

## .......................................................
# Child Abuse/Neglect Investigations/Assessments by Race/Ethnicity ----
# Source: Data request made to research@dss.virginia.gov
#  - Individual Accepted Referrals reports for SFY 2016 through SFY 2025
#  - Data retreived from Virginia OASIS (On-line Automated Services Information System). 
#    - This is the Commonwealth's state automated child welfare information system (SACWIS) 
#       for recording foster care and child protective services cases

# welfare_cases <- read_csv("data/child_welfare_cases.csv")

# read in files 
acc_cases_files <- list.files(path = "download_data/cps_OASIS_reports", pattern = "*.xlsx", full.names = TRUE)

acc_cases_years <- acc_cases_files %>% 
  str_extract(pattern = "[0-9]{2}")

acc_cases_data <- map(acc_cases_files, ~read_excel(.x, sheet = 1))
names(acc_cases_data) <- acc_cases_years # add year as names for list

# bind into data frame
acc_cases_all <- bind_rows(acc_cases_data, .id = "sfy_year") %>% 
  clean_names()

# rename and filter
acc_cases_all <- acc_cases_all  %>% 
  mutate(local_agency = case_when(is.na(local_agency) ~ "Virginia",
                                  .default = local_agency),
         region_name = case_when(is.na(region_name) ~ "State",
                                 .default = region_name)) %>% 
  filter(local_agency %in% c("Albemarle", "Charlottesville", "Virginia")) %>% 
  mutate(sfy_year = as.numeric(paste0("20",sfy_year)),
         GEOID = case_when(
           local_agency == "Albemarle" ~ "51003",
           local_agency == "Charlottesville" ~ "51540",
           local_agency == "Virginia" ~ "51"
         )) %>% 
  select(-c("male", "female", "sex_unknown")) %>% 
  select(-starts_with("age_")) %>% 
  mutate(cps_under18 = total_children, .after = total_children) %>% 
  rename_with(~ paste0("cps_under18_", .x), .cols = hispanic:race_unknown)

# Bring in population data
pop_under18 <- read_csv("data/pop_race_ethn_acs.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  select(GEOID, locality, year, starts_with("pop_under18")) %>% 
  select(-c("pop_under18_multi","pop_under18_white_nonhisp", "pop_under18_other"))

age_groups <- read_csv("data/population_data_acs.csv") %>% 
  mutate(GEOID = as.character(fips)) %>% 
  select(GEOID, locality, year, pop_under18)

# Join population data
acc_cases <- acc_cases_all %>% 
  left_join(age_groups, by = join_by(GEOID == GEOID, sfy_year == year)) %>% 
  left_join(pop_under18, by = join_by(GEOID == GEOID, sfy_year == year, locality == locality))

# Pivot
acc_cases_rates <- acc_cases %>% 
  select(-c("cps_under18_race_unknown")) %>% 
  rename(cps_under18_aian = cps_under18_american_indian,
         cps_under18_nhpi = cps_under18_hawaiian_pacific_islander,
         pop_under18_hispanic = pop_under18_hisp) %>% 
  pivot_longer(c(starts_with("cps_"), starts_with("pop")), names_to = c("name","group"), names_pattern = "(.*?)_(.*)") %>%      
  pivot_wider(id_cols = c(GEOID, local_agency, region_name, sfy_year, group, total_children), names_from = name, values_from = value, names_repair = "check_unique") %>% 
  rename(cps_count_group = cps,
         population_count_group = pop,
         cps_total_children = total_children) %>% 
  mutate(group = case_when(group == "under18" ~ "all",
                           startsWith(group, "under18_") ~ str_remove(group, "under18_")))

# Create poverty rates
acc_cases_rates <- acc_cases_rates %>%
  mutate(cps_per_bygroup = round(cps_count_group/population_count_group * 100, 2),
         cps_per_byallcps = round(cps_count_group/cps_total_children * 100, 2),
         cps_rate_bygroup = round(cps_count_group/population_count_group * 1000, 2),
         cps_rate_byallcps = round(cps_count_group/cps_total_children * 1000, 2))

# Save
write_csv(acc_cases_rates, "data/welfare_cases_race_ethn.csv")
