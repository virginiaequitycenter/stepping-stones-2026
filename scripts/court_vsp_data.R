# Script for additional sources for School and Community Disciplinary Actions data for the Stepping Stones 2026 report
# Gets new data and merges with prior years

# Included data updates:
# - Children in Need of Services or Supervision
#   - Source: Moved since last report - PowerBi and Monthly here: https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/jdr/home
# - Juvenile Delinquency Judgments
#   - Source: Moved since last report - PowerBi and Monthly here: https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/jdr/home
# - Arrests for Violent Crimes (10-17)
#   - Source: VSP https://va.beyond2020.com/va_public/Browse/browsetables.aspx?PerspectiveLanguage=en
# - Arrests for Crimes involving Firearms (10-17) 
#   - Source: VSP https://va.beyond2020.com/va_public/Browse/browsetables.aspx?PerspectiveLanguage=en

# Libraries ----
library(tidyverse)

## .......................................................
# Children in Need of Services or Supervision ----
# Source: PowerBi and Monthly here: https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/jdr/home
# (1) Go to: https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/jdr/home
# (2) Under Case Dispositions, click "View Daily Report" to go to PowerBi dashboard
# (3) Go to dashboard page "State (Years)"
# (4) Selections in top right corner drop down options:
#   - Years dropdown: Select each year recording data individually
#   - Second dropdown (disposition types): De-select all, expand "Civil" category, 
#       select "Child in Need of Services - CS" and "Truancy/Runaway - TR" (hold command for multiple selections)
# (5) Record values in spreadsheet 
#   - State total from "Dispositions by Calendar Year" bar chart
#   - Locality totals from "Dispositions by Case Type" (for individual CS and TR counts) or 
#       "Dispositions by Major Case Category" for summed total of CS and TR
# Recorded in CSV: jdr_services_supervision_recorded.csv

# Rate per 1000 5-17yr olds 
# Age range determined by combination of under 18 for Child in Need of Services and 5-18yr olds required to be at school (Truancy)

jdr_services_recorded <- read_csv("data/jdr_services_supervision_recorded.csv") %>% 
  mutate(fips = as.character(fips))

# Bring in population data (Created in scripts/pop_estimates.R)
age517_df <- read_csv("data/census_population_2010_2024.csv") %>%
  mutate(fips = as.character(fips)) %>%
  select(fips, year, age517_tot)

# Join tables
jdr_services <- jdr_services_recorded %>% 
  left_join(age517_df)

# create rates
jdr_services <- jdr_services %>% 
  mutate(rate_services = (count_services/age517_tot)*1000,
         rate_truancy = (count_truancy/age517_tot)*1000,
         rate_total = (count_total/age517_tot)*1000)

# check
ggplot(jdr_services, aes(year, rate_total, group = locality))+
  geom_line()

# Save
write_csv(jdr_services, "data/jdr_services_supervision_rates.csv")

## .......................................................
# Juvenile Delinquency Judgments ----
# Source: PowerBi and Monthly here: https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/jdr/home
# (1) Go to: https://www.vacourts.gov/courtadmin/aoc/djs/programs/cpss/csi/jdr/home
# (2) Under Case Dispositions, click "View Daily Report" to go to PowerBi dashboard
# (3) Go to dashboard page "State (Years)"
# (4) Selections in top right corner drop down options:
#   - Years dropdown: Select each year recording data individually
#   - Second dropdown (disposition types): De-select all, expand "Delinquncy" category, 
#       select "Delinquency Felony - DF" and "Delinquency Misdemeanor - DM" (hold command for multiple selections)
# (5) Record values in spreadsheet 
#   - State total from "Dispositions by Calendar Year" bar chart
#   - Locality totals from "Dispositions by Case Type" (for individual DF and DM counts) or 
#       "Dispositions by Major Case Category" for summed total of DF and DM
# Recorded in CSV: jdr_delinquency_recorded.csv

# Rate per 1000 10-17yr olds - age range as close to delinquency as possible from population totals (11-17yrs)

jdr_delinquency_recorded <- read_csv("data/jdr_delinquency_recorded.csv") %>% 
  mutate(fips = as.character(fips))

# Bring in population data (Created in scripts/pop_estimates.R)
age1017_df <- read_csv("data/census_population_2010_2024.csv") %>%
  mutate(fips = as.character(fips)) %>%
  select(fips, year, age1017_tot)

# Join tables
jdr_delinquen <- jdr_delinquency_recorded %>% 
  left_join(age1017_df)

# create rates
jdr_delinquen <- jdr_delinquen %>% 
  mutate(rate_felony = (count_felony/age1017_tot)*1000,
         rate_misdemeanor = (count_misdemeanor/age1017_tot)*1000,
         rate_total = (count_total/age1017_tot)*1000)

# check
ggplot(jdr_delinquen, aes(year, rate_total, group = locality))+
  geom_line()

# Save
write_csv(jdr_delinquen, "data/jdr_delinquency_rates.csv")

## .......................................................
# Arrests for Violent Crimes ----
#   - Source: VSP https://vsp.virginia.gov/sections-units-bureaus/bass/criminal-justice-information-services/uniform-crime-reporting/#UCR-IBR
# Retrieving arrest and crime data from va.beyond2020.com 
# https://va.beyond2020.com/va_public/Browse/browsetables.aspx?PerspectiveLanguage=en
# 
# Table 1
# Number of Group A Arrestees by Age by Arrest Offense
# 
# * Start with Arrest reports - Number of Group A Arrestees by age by arrest offense (switch to table view)
# * Click arrest offense (on table): select crimes against persons
# * Click arrestee age (on table): select Under 18 (and under 10, 10-17), deselect all others
# * Click choose arrest date (on table): select all years
# * Click jurisdiction by geography (on table): unselect all but VA, Cvl, Alb (division 3)
# * Rearrange report: arrest offense and jurisdiction by geography in rows; arrest date, arrest age in columns
# * Download

# Read in data
# Keep only arrests ages 10-17 yrs (no under 10 arrests in Alb/Cville during these years)
vsp_arrests <- read_csv("download_data/vabeyond/Number of Group A Arrestees by Age by Arrest Offense Cleaned.csv", skip = 6, col_select = c(1:2, 35:50)) %>%
  clean_names() %>% 
  rename_with(~ str_sub(.x,2,5), starts_with("x"))
  
# Reshape
vsp_arrest_rates <- vsp_arrests %>% 
  pivot_longer(cols = c(`2009`:`2024`), names_to = "year", values_to = "number_of_arrestees_10to17") %>% 
  mutate(year = as.numeric(year))

# Bring in population data (Created in scripts/pop_estimates.R)
age1017_df <- read_csv("data/census_population_2010_2024.csv") %>%
  mutate(name = str_to_title(name)) %>%
  select(name, year, age1017_tot)

# Join tables
vsp_arrest_rates <- vsp_arrest_rates %>% 
  left_join(age1017_df, by = join_by(jurisdiction_by_geography == name, year == year))

# create rate
vsp_arrest_rates <- vsp_arrest_rates %>% 
  mutate(rate_arrests = (number_of_arrestees_10to17/age1017_tot)*1000)

# Save
write_csv(vsp_arrest_rates, "data/vsp_arrest_rates.csv")

## .......................................................
# Arrests for Crimes involving Firearms (Under 10-17) ----
# Retrieving arrest and crime data from va.beyond2020.com 
# https://va.beyond2020.com/va_public/Browse/browsetables.aspx?PerspectiveLanguage=en
# 
# Table 2
# All Crime Types By City or County Firearms
# 
# * Start with Crimes by Jurisdiction - All crime by jurisdiction
# * Select Type of weapon/force involved (on sidebar): select firearem
# * Click offender age (on table): select Under 18 (and under 10, 10-17), deselect all others
# * Click incident date (on table or sidebar): select all years
# * Click jurisdiction by geography (on table or sidebar): unselect all but VA, Cvl, Alb (division 3)
# * Rearrange report: jurisdiction by geography in rows; arrest date, arrest age in columns
# * Download

# Read in Data
# Keep only arrests ages 10-17 yrs (no under 10 arrests in Alb/Cville during these years)
vsp_firearms <- read_csv("download_data/vabeyond/All Crime Types By City or County Firearms Cleaned.csv", skip = 8, col_select = c(1, 34:49)) %>%
  clean_names() %>% 
  rename_with(~ str_sub(.x,2,5), starts_with("x"))

# Reshape
vsp_firearms_rates <- vsp_firearms %>% 
  pivot_longer(cols = c(`2009`:`2024`), names_to = "year", values_to = "number_of_arrestees_10to17") %>% 
  mutate(year = as.numeric(year))

# Bring in population data (Created in scripts/pop_estimates.R)
age1017_df <- read_csv("data/census_population_2010_2024.csv") %>%
  mutate(name = str_to_title(name)) %>%
  select(name, year, age1017_tot)

# Join tables
vsp_firearms_rates <- vsp_firearms_rates %>% 
  left_join(age1017_df, by = join_by(jurisdiction_by_geography == name, year == year))

# create rate
vsp_firearms_rates <- vsp_firearms_rates %>% 
  mutate(rate_firearms = (number_of_arrestees_10to17/age1017_tot)*1000)

# Save
write_csv(vsp_firearms_rates, "data/vsp_firearms_rates.csv")
