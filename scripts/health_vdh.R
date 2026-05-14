# Script for updating VDH data for Stepping Stones 2026 report
# Gets new data and merges with prior years

# Included data updates:
# - Prenatal Care 
#   - Source: KidsCount (1993-2023) https://datacenter.aecf.org/data/tables/3234-prenatal-care-beginning-in-the-first-trimester?loc=48&loct=5#detailed/5/6813,6836/true/2545,1095,2048,574,1729,37,871,870,573,869/any/23763,6672
# - Low Birth-Weight Infants 
#   - Source: KidsCount (1993-2023) https://datacenter.aecf.org/data/tables/3252-low-birthweight-babies?loc=48&loct=5#detailed/5/6813,6836/false/2545,1095,2048,574,1729,37,871,870,573,869/any/12515,6708
# - Infant Deaths
#   - Source: KidsCount (1993-2023) https://datacenter.aecf.org/data/tables/3236-infant-mortality?loc=48&loct=5#detailed/5/6813,6836/true/2545,1095,2048,574,1729,37,871,870,573,869/any/6676,14138
# - Teen Birth Rate
#   - Source: KidsCount (1995-2023) https://datacenter.aecf.org/data/tables/3235-teen-birth-rate-per-1000-by-age-group?loc=48&loct=5#detailed/5/6813,6836/true/2545,1095,2048,574,1729,37,871,870,573,869/2712,179,180,189/12641
# - Sexually Transmitted Infections in Youth
#   - Source: VDH Data request https://redcap.vdh.virginia.gov/redcap/surveys/?s=LH9TTYCMA4

# Libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(zoo)

## .......................................................
# Prenatal Care ----
# Source: KidsCount (1993-2023) https://datacenter.aecf.org/data/tables/3234-prenatal-care-beginning-in-the-first-trimester?loc=48&loct=5#detailed/5/6813,6836/true/2545,1095,2048,574,1729,37,871,870,573,869/any/23763,6672

prenatal <- read_xlsx("download_data/kidscount_prenatal_care.xlsx") %>% 
  clean_names() %>% 
  filter(location %in% c("Virginia", "Albemarle", "Charlottesville")) %>% 
  filter(data_format == "Percent") %>% 
  mutate(time_frame = as.numeric(time_frame),
         percent_prenatal_care = as.numeric(data) * 100) %>% 
  filter(time_frame >=2010) %>% 
  select(-c("data_format", "data"))

# ggplot(prenatal, aes(time_frame, data, color = location, group = location)) + 
#   geom_line() + 
#   scale_y_continuous(limits = c(40, 100))

# save
write_csv(prenatal, "data/prenatal_care.csv")

## .......................................................
# Low Birth-Weight Infants ----
# Source: KidsCount (1993-2023) https://datacenter.aecf.org/data/tables/3252-low-birthweight-babies?loc=48&loct=5#detailed/5/6813,6836/false/2545,1095,2048,574,1729,37,871,870,573,869/any/12515,6708

low_birthweight_raw <- read_xlsx("download_data/kidscount_low_birthweight.xlsx")

# Wrangle
low_birthweight <- low_birthweight_raw %>% 
  clean_names() %>% 
  filter(location %in% c("Virginia", "Albemarle", "Charlottesville")) %>% 
  filter(data_format == "Percent") %>% 
  mutate(time_frame = as.numeric(time_frame)) %>% 
  filter(time_frame >=2008) %>% 
  mutate(percent_low_birthweight = as.numeric(data) * 100)

low_birthweight <- low_birthweight %>% 
  arrange(location, time_frame) %>% 
  group_by(location) %>% 
  mutate(percent_low_birthweight_3yravg = zoo::rollmean(percent_low_birthweight, k = 3, fill = NA, align = "center")) %>% 
  ungroup() %>% 
  select(-c("data_format", "data"))

# save
write_csv(low_birthweight, "data/low_birthweight.csv")

# ggplot(low_birthweight, aes(time_frame, percent_low_birthweight_3yravg, color = location, group = location)) + 
#   geom_line() + 
#   geom_point() +
#   scale_y_continuous(limits = c(0, 60))

## .......................................................
# Infant Mortality ----
# Source: KidsCount (1993-2023) 

infant_mortality_raw <- read_xlsx("download_data/kidscount_infant_mortality.xlsx")

# Wrangle
infant_mortality <- infant_mortality_raw %>% 
  clean_names() %>% 
  filter(location %in% c("Virginia", "Albemarle", "Charlottesville")) %>% 
  filter(data_format == "Rate per 1,000 births") %>% 
  mutate(time_frame = as.numeric(time_frame)) %>% 
  filter(time_frame >=2008) %>% 
  mutate(rate_infant_mortality = as.numeric(data))

infant_mortality <- infant_mortality %>% 
  arrange(location, time_frame) %>% 
  group_by(location) %>% 
  mutate(rate_infant_mortality_3yravg = zoo::rollmean(rate_infant_mortality, k = 3, fill = NA, align = "center")) %>% 
  ungroup() %>% 
  select(-c("data"))

# save
write_csv(infant_mortality, "data/infant_mortality.csv")

## .......................................................
# Teen Pregnancies ----
# Source: https://virginiawellbeing.com/virginia-community-health-improvement-data-portal/vdh-assessment/?REPORT=%7B%22name%22%3A%22Virginia%20Community%20Health%20Assessment%22%2C%22style%22%3A%22VDH%22%2C%22contentId%22%3A%22%23cdt-report-content%22%2C%22output%22%3A%7B%22countylist%22%3Atrue%2C%22statelist%22%3Atrue%2C%22ziplist%22%3Afalse%2C%22tractlist%22%3Afalse%2C%22map%22%3Atrue%2C%22breakout%22%3Atrue%7D%2C%22indicatorUid%22%3A%5B%228124%22%5D%2C%22indicator%22%3A%5B61%5D%2C%22location%22%3A%7B%22type%22%3A%22county%22%2C%22remove_county%22%3Afalse%2C%22show_county%22%3Atrue%2C%22show_state%22%3Atrue%2C%22show_zip%22%3Afalse%2C%22show_listdata_onload%22%3Afalse%2C%22show_tract%22%3Afalse%2C%22key%22%3A%22county%22%2C%22id%22%3A%5B%2251003%22%2C%2251540%22%5D%2C%22name%22%3A%5B%22Albemarle%20County%2C%20VA%22%2C%22Charlottesville%20City%2C%20VA%22%5D%7D%7D

# Steps:
# (1) Go to: https://virginiawellbeing.com/virginia-community-health-improvement-data-portal/vdh-assessment/
# (2) For Locality: Select Albemarle County and Charlottesville City
# (3) For Data Indicators: Expand Maternal and Child Health and Select "Teen Pregnancy, Rate Per 1,000 Females Ages 15-19"
# (4) Click "Report" to get to final page
# (5) Scroll to "Teen Pregnancy Rate by Year, 2015 - 2023" table

teen_preg_raw <- read_csv("data/vdh_teen_pregnancies_recorded.csv")

# Wrangle
teen_preg <- teen_preg_raw %>% 
  clean_names() %>% 
  filter(report_area %in% c("Virginia", "Albemarle County, VA", "Charlottesville city, VA")) %>% 
  pivot_longer(cols = x2015:x2023) %>% 
  mutate(report_area = str_to_title(str_remove(report_area, ", VA")),
         year = str_remove(name, "x"),
         rate_teen_pregnancy_per_1000 = value) %>% 
  select(-c(name, value))

# save
write_csv(teen_preg, "data/teen_pregnancy.csv")

## .......................................................
# Not Used in report due to mismatch between age groups in data
# Teen Birth Rate ----
# Source: KidsCount (1993-2023)

teen_birth_raw <- read_xlsx("download_data/kidscount_teen_birth_rate.xlsx")

# Wrangle
teen_birth <- teen_birth_raw %>%
  clean_names() %>%
  filter(location %in% c("Virginia", "Albemarle", "Charlottesville")) %>%
  filter(data_format == "Rate") %>%
  filter(age_group != "< 15") %>%
  mutate(time_frame = as.numeric(time_frame)) %>%
  filter(time_frame >=2009) %>%
  mutate(rate_teen_births = as.numeric(data),
         data_format = case_when(data_format == "Rate" ~ "Rate per 1,000",
                                 .default = data_format)) %>%
  select(-c("data"))

# save
write_csv(teen_birth, "data/teen_birth_rate.csv")

## .......................................................
# Sexually Transmitted Infections in Youth ----
# Source: VDH Data request https://redcap.vdh.virginia.gov/redcap/surveys/?s=LH9TTYCMA4

# VDH Data Request:
# Reported Annual STI/HIV Case Counts and Incidence Rates among Persons 10-19 Years of Age at Diagnosis, 2010-2024
# Conditions: Chlamydia, Gonorrhea, HIV, Syphilis
# Age group: 10-19yr olds
# VDH provided Annual and 3-year avg rates
options(scipen = 999)
youth_sti_vdh <- read.xlsx("download_data/CvilleAlbermarle Data Request 2026-03-24.xlsx", startRow = 3, fillMergedCells = TRUE) %>% 
  mutate(across(c(`2010`:`2024`), ~ as.numeric(.x), .names = "{.col}"))

# Reshape
youth_sti <- youth_sti_vdh %>% 
  pivot_longer(cols = c(`2010`:`2024`), names_to = "year", values_to = "value") %>% 
  pivot_wider(names_from = "Disease") %>% 
  clean_names() %>% 
  rename(pop_est_10to19yr = population_estimate)

# Save
write_csv(youth_sti, "data/youth_sti_rates.csv")

## .......................................................



## .......................................................
