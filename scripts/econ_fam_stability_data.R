# Script for additional data sources for Economic and Family Stability for the Stepping Stones 2026 report
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

# Bring in population data (Created in scripts/pop_estimates.R)
age18under_df <- read_csv("data/census_population_2010_2024.csv") %>%
  mutate(fips = as.character(fips)) %>%
  select(fips, year, age18under_tot)

# Join updated rates with population data
child_welfare <- cases_accepted_df %>% 
  left_join(age18under_df)

# Read in prior data
child_welfare_prior <- read_csv("data/2023_data/child_welfare_reports.csv") %>% 
  rename(cases_accepted = cps_count) %>% 
  mutate(fips = as.character(fips)) %>% 
  select(-c(cps_rate, pop_17under))

# Join prior data with population est (to recalulate/compare rates)
child_welfare_prior <- child_welfare_prior %>% 
  left_join(age18under_df)

# Bind tables
child_welfare_data <- rbind(child_welfare, child_welfare_prior)

# Create rate per 1000
child_welfare_data <- child_welfare_data %>%
  mutate(cps_rate = (cases_accepted/age18under_tot)*1000)

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

# Read in prior data
foster_care_prior <- read_csv("data/2023_data/foster_care.csv") %>% 
  mutate(fips = case_when(locality == "Virginia" ~ "51",
                          .default = as.character(fips))) %>% 
  select(-c(fc_rate, pop_17under))

# Combine new and prior data
foster_care <- rbind(fc, foster_care_prior)

# Bring in population data (Created in scripts/pop_estimates.R)
age18under_df <- read_csv("data/census_population_2010_2024.csv") %>%
  mutate(fips = as.character(fips)) %>%
  select(fips, year, age18under_tot)

# Join with population data
foster_care <- foster_care %>% 
  left_join(age18under_df)

# Create rate per 1000
foster_care_data <- foster_care %>%
  mutate(fc_rate = (fc_count/age18under_tot)*1000)

# Save
write_csv(foster_care_data, "data/foster_care.csv")

## .......................................................
# McKinney-Vento ----
# Source: Ed Data Express https://eddataexpress.ed.gov/download/data-library?field_year_target_id=All&field_population_value=&field_data_topic_target_id=All&field_reporting_level_target_id=All&field_program_target_id=42&field_file_spec_target_id=All&field_data_group_id_target_id=All&combine=
# This data source was not updated since retrieving data for the 2025 Community Well-being Profiles
# Data from that project was copied over in stepping-stones-2026/data
# For more on retrieval methods see:
# https://github.com/virginiaequitycenter/regional-equity-profile/blob/3f8605eb5f86b03d93baf0dc0aa1047babfeb715/datacode/supplemental_downloadsremoved/unhoused.R#L166

# Create Rates

# Read in division data
mv_df <- read_csv("data/mckinney-vento_2011_2023.csv") %>% 
  clean_names() %>% 
  mutate(division = ifelse(lea=="ALBEMARLE COUNTY PUBLIC SCHOOLS","Albemarle","Charlottesville"))


# MV State data ----

# https://eddataexpress.ed.gov/download/data-library?field_year_target_id=All&field_population_value=&field_data_topic_target_id=All&field_reporting_level_target_id=All&field_program_target_id=42&field_file_spec_target_id=All&field_data_group_id_target_id=All&combine=
url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_11718/SY2122_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "download_data/mv2122.zip")
unzip("dowloads/mv2122.zip", exdir = "download_data/mv2122/")
mv22 <- read_csv("download_data/mv2122/SY2122_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA")

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_8321/SY2021_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "download_data/mv2021.zip")
unzip("download_data/mv2021.zip", exdir = "download_data/mv2021/")
mv21 <- read_csv("download_data/mv2021/SY2021_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA")

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_6526/SY1920_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "download_data/mv1920.zip")
unzip("download_data/mv1920.zip", exdir = "download_data/mv1920/")
mv20 <- read_csv("download_data/mv1920/SY1920_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA")

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_2111/SY1819_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "download_data/mv1819.zip")
unzip("download_data/mv1819.zip", exdir = "download_data/mv1819/")
mv19 <- read_csv("download_data/mv1819/SY1819_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA") %>% 
  mutate(`NCES LEA ID` = as.character(`NCES LEA ID`))

url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip"
download.file(url, destfile = "download_data/mv1018.zip")
unzip("download_data/mv1018.zip", exdir = "download_data/mv1018/")
mv18 <- read_csv("download_data/mv1018/SY1018_FS118_DG655_LEA.csv") %>% 
  filter(State == "VIRGINIA")
# The above file contains 2010-2011 through 2017-2018, but individual files also exist
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2016-17
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2015-16
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2014-15
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2013-14
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2012-13
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2011-12
# url <- "https://eddataexpress.ed.gov/sites/default/files/data_download/EID_1350/SY1018_FS118_DG655_LEA_data_files.zip" # 2010-11

# generate yearly totals
mv_va_all_schools <- bind_rows(
  mv22 %>% filter(Subgroup == "All Students in LEA") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv21 %>% filter(Subgroup == "All Students in LEA") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv20 %>% filter(Subgroup == "All Students in LEA") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv19 %>% filter(Subgroup == "All Students") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup),
  mv18 %>% filter(Subgroup == "All Students") %>% select(`School Year`, `NCES LEA ID`, `LEA`, `Data Description`, Value, Population, Subgroup)
)

# save
write_csv(mv_va_all_schools, "data/mv_va_all_schools_2011_2022.csv")

# Add 2023 ----
mv <- read_csv("data/mv_va_all_schools_2011_2022.csv") %>% 
  mutate(`NCES LEA ID` = as.character(`NCES LEA ID`),
         Value = as.numeric(Value))

# 2022-2023 only available as of 2025-01-08 via data download tool
# https://eddataexpress.ed.gov/download/data-builder/data-download-tool?f%5B1%5D=all_students%3AAll%20Students%20in%20SEA&f%5B2%5D=data_group_id%3A655&f%5B3%5D=file_spec%3A118&f%5B4%5D=school_year%3A2022-2023&f%5B5%5D=state_name%3AVIRGINIA&f%5B0%5D=all_students%3AAll%20Students%20in%20LEA
mv23 <- read_csv("download_data/mv2223.csv")
glimpse(mv23)

mv23 <- mv23 %>% 
  select(`School Year`, `NCES LEA ID`, LEA, `Data Description`, Value, Population, Subgroup) %>% 
  mutate(LEA = str_to_upper(LEA),
         `NCES LEA ID` = as.character(`NCES LEA ID`),
         Value = as.numeric(Value))

mv <- mv %>% 
  mutate(LEA = str_replace(LEA, "CO PBLC SCHS", "COUNTY PUBLIC SCHOOLS"),
         LEA = str_replace(LEA, "CTY PBLC SCHS", "CITY PUBLIC SCHOOLS"))

mv_with_23 <- bind_rows(mv23, mv)

# Create state totals
mv_va <- mv_with_23 %>% 
  clean_names() %>% 
  filter(subgroup != "All Students in SEA") %>% 
  group_by(school_year, data_description, population, subgroup) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>%
  mutate(division = "Virginia") 

# Join state mv with division
mv_rates <- mv_df %>% 
  select(school_year, data_description, population, subgroup, value, division) %>% 
  bind_rows(mv_va)


# Read in fall membership
fallmem <- read_csv("download_data/fall_membership_statistics.csv") %>% 
  clean_names() %>% 
  rename(students = total_count,
         division = division_name) %>% 
  mutate(division = case_when(division=="Albemarle County" ~ "Albemarle",
                              division=="Charlottesville City" ~ "Charlottesville",
                              is.na(division) ~ "Virginia")) %>% 
  select(school_year, division, students)

fall_mem_prior <- read_csv("data/2023_data/student_totals.csv") %>% 
  filter(!school_year %in% c("2021-2022", "2022-2023"))

# bind
fall_mem <- rbind(fall_mem_prior, fallmem)

# Join with fall membership
mv_rates <- mv_rates %>% 
  left_join(fall_mem)

# Create rate per 1000
mv_rates <- mv_rates %>%
  mutate(mv_rate = (value/students)*1000) %>% 
  rename(mv_count = value,
         total_students = students) %>% 
  select(school_year, division, population, mv_count, total_students, mv_rate)

# Save
write_csv(mv_rates, "data/mckinney_vento_rates.csv")
