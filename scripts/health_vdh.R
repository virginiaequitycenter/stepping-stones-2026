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
library(openxlsx)
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
         data = as.numeric(data) * 100) %>% 
  filter(time_frame >=2000)

ggplot(prenatal, aes(time_frame, data, color = location, group = location)) + 
  geom_line() + 
  scale_y_continuous(limits = c(40, 100))

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
  filter(time_frame >=2000) %>% 
  mutate(data = as.numeric(data) * 100)

low_birthweight <- low_birthweight %>% 
  arrange(location, time_frame) %>% 
  group_by(location) %>% 
  mutate(pct_3yr = zoo::rollmean(data, k = 3, fill = NA, align = "center")) %>% 
  ungroup()

ggplot(low_birthweight, aes(time_frame, pct_3yr, color = location, group = location)) + 
  geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0, 60))

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
