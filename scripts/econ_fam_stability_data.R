# Script for additional data sources for Stepping Stones 2026 report
# Gets new data and merges with prior years

# Included data updates:
# - Children Living below Poverty Threshold 
#   - Source: SAIPE https://www.census.gov/programs-surveys/saipe/data/datasets.html
# - Assessments and Investigations by Child Protective Services 
#   - Source: VDSS PowerBI dashboard (2021-2024) https://cpsaccountability.dss.virginia.gov/index-social-services.html
# - Children in Foster Care
#   - Source: VDSS (2020-2025) https://www.dss.virginia.gov/geninfo/reports/children/fc.cgi
# - McKinney-Vento
#   - Source: Ed Data Express https://eddataexpress.ed.gov/download/data-library?field_year_target_id=All&field_population_value=&field_data_topic_target_id=All&field_reporting_level_target_id=All&field_program_target_id=42&field_file_spec_target_id=All&field_data_group_id_target_id=All&combine=
#   - This data source was not updated since retrieving data for the 2025 Community Well-being Profiles
#   - Data from that project was copied over in stepping-stones-2026/data


# Libraries ----
library(tidyverse)
library(readxl)
library(janitor)

## .......................................................
# Children Living below Poverty Threshold ----
# Source: SAIPE https://www.census.gov/programs-surveys/saipe/data/datasets.html
# Choose State and County Estimates
# https://www.census.gov/data/datasets/2024/demo/saipe/2024-state-and-county.html

# Read data 

# Make a function
# load saipe and generate childpov
# arguments: url, year, countyfips
extract_childpov <- function(url, year, countyfips){
  saipe <- read_fwf(url,
                    fwf_cols(state_fips = c(1, 2), county_fips = c(4,6),
                             num_child_pov = c(50,57), pct_child_pov = c(77,80),
                             num_child_pov_lb = c(59,66), num_child_pov_ub = c(68,75),
                             pct_child_pov_lb = c(82,85), pct_child_pov_ub = c(87,90)),
                    col_types = "ccnnnnnn")
  
  cpov <- saipe %>% 
    filter(county_fips %in% c("0", countyfips)) %>% 
    mutate(county_fips = str_pad(county_fips, width = 3, side = "left", pad = "0"),
           year = year) %>% 
    select(county_fips, year, num_child_pov, pct_child_pov)
  
  return(cpov)
}

# Create list of urls
# they follow a clear pattern where only the value of the year shifts
# 2022-2024 have .txt
years <- tibble(y = seq(22,24,1), x = "txt") %>% 
  mutate(y = as.character(y),
         y = str_pad(y, 2, side = "left", pad = "0"))
urllist <- paste0("https://www2.census.gov/programs-surveys/saipe/datasets/20",years$y,"/20",years$y,"-state-and-county/est",years$y,"-va.",years$x)


# Apply function
# make a data frame of paired url and year arguments
url_year <- tibble(url = urllist, year = years$y) %>% 
  mutate(year = paste0("20",year), 
         year = as.numeric(year))

# apply function across the url/year pairs, add countyfips argument
cpov_list <- map2(url_year$url, url_year$year, 
                  ~extract_childpov(.x, .y, countyfips = c("3", "540"))) 

# bind the list of resulting data frames into a single dataframe
cpov <- bind_rows(cpov_list)

# Read in prior data
cpov_prior <- read_csv("data/2023_data/child_pov.csv")

# Bind tables
cpov_data <- rbind(cpov_prior, cpov)

# Save
write_csv(cpov_data, "data/child_poverty.csv")

## .......................................................
# Assessments and Investigations by Child Protective Services ----
# Source: VDSS PowerBI dashboard (2021-2024) https://cpsaccountability.dss.virginia.gov/index-social-services.html
# Year is Fiscal Year 7/1 - 6/30
# Create data from from VDSS PowerBI dashboard
# (1) Select each locality and state total individually (for state, select all localities)
# (2) Select each year individually, 
# (3) Record Cases Accepted (for state total, select all localities and record total)
cases_accepted_df <- tibble(year = rep(seq(2021,2024, by=1), 3),
                 locality = rep(c("Albemarle", "Charlottesville", "Virginia"), c(4,4,4)),
                 cases_accepted = c(357, 433, 468, 455,
                                    188, 177, 176, 192,
                                    34194, 37113, 37185, 36845),
                 fips = rep(c("51003", "51540", "51"), c(4,4,4)))

# Bring in population data (Created in scripts/acs_5yr.R)
pop_under18_df <- read_csv("data/population_data_acs.csv") %>% 
  mutate(fips = as.character(fips)) %>% 
  rename(under18_acs = pop_under18) %>% 
  select(fips, year, under18_acs)

# Join updated rates with population data
child_welfare <- cases_accepted_df %>% 
  left_join(pop_under18_df) %>% 
  pivot_longer(cols = starts_with(c("under18_")),
               names_to = c(".value", "pop_source"), 
               names_sep = "_")

# Read in prior data
child_welfare_prior <- read_csv("data/2023_data/child_welfare_reports.csv") %>% 
  rename(cases_accepted = cps_count,
         under18_cdc = pop_17under) %>% 
  mutate(fips = as.character(fips)) %>% 
  select(-cps_rate)

# Join prior data with ACS population (to recalulate/compare rates)
child_welfare_prior <- child_welfare_prior %>% 
  left_join(pop_under18_df) %>% 
  pivot_longer(cols = starts_with(c("under18_")),
               names_to = c(".value", "pop_source"), 
               names_sep = "_")

# Bind tables
child_welfare_data <- rbind(child_welfare, child_welfare_prior)

# Create rate per 1000
child_welfare_data <- child_welfare_data %>%
  mutate(cps_rate = (cases_accepted/under18)*1000)

# Save
write_csv(child_welfare_data, "data/child_welfare_cases.csv")

## .......................................................
# Children in Foster Care ----
# Source: VDSS (2020-2025) https://www.dss.virginia.gov/geninfo/reports/children/fc.cgi

# Create vector of urls 
## choose september report for each year (manually copy-paste)
## (could pull all reports and average as well)

urls <- c(
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2025/children_demographic/fc_demo_202509.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2024/children_demographic/fc_demo_202409.xlsx",
  "https://www.dss.virginia.gov/files/about/reports/children/foster_care/2023/children_demographic/fc_demo_202309.xlsx"
)

# create vector of destination file names 
dest <- paste0("download_data/dssfc/", basename(urls))

# download files 
if (!dir.exists("download_data/dssfc/")) 
{dir.create("download_data/dssfc/")}

walk2(urls, dest, download.file, method = "curl", extra = "-k")

# read in files 
# test <- read_excel("download_data/dssfc/fc_demo_202509.xlsx", sheet = 1, skip = 3)
# test <- read_excel(dest[1], sheet = 1, skip = 3)

fc_data <- map(dest, ~read_excel(.x, sheet = 1, skip = 3))
names(fc_data) <- 2025:2023 # add year as names for list

fc_all <- bind_rows(fc_data, .id = "year")

# Pull cville, alb, state data
fc <- fc_all %>% 
  filter(LOCALITY %in% c("Albemarle", "Charlottesville", "STATE")) %>% 
  clean_names() %>% 
  select(year, locality, fips, fc_count = total_children_in_care) %>% 
  mutate(year = as.numeric(year),
         locality = ifelse(locality == "STATE", "Virginia", locality),
         fips = case_when(
           fips == 3 ~ "51003",
           fips == 540 ~ "51540",
           is.na(fips) ~ "51"
         ))

# Bring in population data (Created in scripts/acs_5yr.R)
pop_under18_df <- read_csv("data/population_data_acs.csv") %>% 
  mutate(fips = as.character(fips)) %>% 
  rename(under18_acs = pop_under18) %>% 
  select(fips, year, under18_acs)

# Join updated rates with population data
foster_care <- fc %>% 
  left_join(pop_under18_df) %>% 
  pivot_longer(cols = starts_with(c("under18_")),
               names_to = c(".value", "pop_source"), 
               names_sep = "_")

# Read in prior data
foster_care_prior <- read_csv("data/2023_data/foster_care.csv") %>% 
  rename(under18_cdc = pop_17under) %>% 
  mutate(fips = case_when(locality == "Virginia" ~ "51",
                          .default = as.character(fips))) %>% 
  select(-fc_rate)

# Join prior data with ACS population (to recalulate/compare rates)
foster_care_prior <- foster_care_prior %>% 
  left_join(pop_under18_df) %>% 
  pivot_longer(cols = starts_with(c("under18_")),
               names_to = c(".value", "pop_source"), 
               names_sep = "_")

# Bind tables
foster_care_data <- rbind(foster_care, foster_care_prior)

# Create rate per 1000
foster_care_data <- foster_care_data %>%
  mutate(fc_rate = (fc_count/under18)*1000)

# Save
write_csv(foster_care_data, "data/foster_care.csv")

## .......................................................
# McKinney-Vento ----
# Source: Ed Data Express https://eddataexpress.ed.gov/download/data-library?field_year_target_id=All&field_population_value=&field_data_topic_target_id=All&field_reporting_level_target_id=All&field_program_target_id=42&field_file_spec_target_id=All&field_data_group_id_target_id=All&combine=
# This data source was not updated since retrieving data for the 2025 Community Well-being Profiles
# Data from that project was copied over in stepping-stones-2026/data
# For more on retrieval methods see:
# https://github.com/virginiaequitycenter/regional-equity-profile/blob/3f8605eb5f86b03d93baf0dc0aa1047babfeb715/datacode/supplemental_downloadsremoved/unhoused.R#L166

