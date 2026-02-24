# Script for updating VDOE data for Stepping Stones 2026 report
# Gets new data and merges with prior years

# Included data updates:
# - Fall Membership
#   - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
#   - needed for calculating percents for ELL and Economically Disadvantaged students
# - SOL Pass Rates
#   - Math: Grades 3, 5
#   - Reading: Grades 3, 5
#     - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/testresults
# - Students Eligible for Special Education Services
#   - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/dec1
# - English Learners
#   - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
# - Chronic Absenteeism
#   - Source: VDOE https://schoolquality.virginia.gov/download-data
# - On-Time Graduation Rates
#   - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/cohortgraduation
# - Post-Secondary Enrollment
#   - Source: VDOE https://p1pe.doe.virginia.gov/postsec_public/
# - Students Identified as Economically Disadvantaged
#   - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
# - Student Behavior and Administrative Response (SBAR)
#   - Source: VDOE SBAR Build-a-Table https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
#     - Relevant Behavior Code (Larger Categories):
#       - BESO: Behaviors that Endanger Self or Others (BESO) These behaviors endanger the health, safety, or welfare of either the student or others in the school community.
#       - PD: Behaviors described in the Virginia’s Unsafe School Choice Option Policy required by the federal Every Student Succeeds Act of 2015.
# - In-School Suspensions
#   - VDOE Safe Schools Information Resource (SSIR) (2016-17 to 2020-21) https://p1pe.doe.virginia.gov/pti/selection.do
#   - VDOE SBAR Build-a-Table (2021-22 to 2023-24) https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
# - Out-of-School Suspensions
#   - Source: VDOE SBAR Build-a-Table (2021-22 to 2023-24) https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351

# Libraries ----
library(tidyverse)
library(readxl)
library(janitor)

## .......................................................
# Fall Membership ----
# Available at https://p1pe.doe.virginia.gov/buildatable/fallmembership
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/buildatable/fallmembership) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2023-2024; 2024-2025; 2025-2026
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select Student Characteristics : Select All for all categories
#             Self-reporting categories: Select All for all categories
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
fallmem <- read_csv("download_data/fall_membership_statistics.csv") %>% 
  clean_names() %>% 
  rename(total_students = total_count) %>% 
  select(-c(ft_count, pt_count))

## .......................................................
# SOL Pass Rates ----
# Available at https://p1pe.doe.virginia.gov/buildatable/testresults 
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/buildatable/testresults) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2022-2023; 2023-2024; 2024-2025
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select Student Characteristics and Reporting Categories: Select All for all categories
#             Select Test Level (multi-select): Grade 3, Grade 5
#             Select Test Source: SOL
#             Select Test Subject Area (multi-select): English:Reading; Mathematics
#             Select Test: English Reading; Mathematics
#             Select Statistic: Total Count, Pass Rate, Pass Count
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
sol_pass <- read_csv("download_data/assessment_statistics.csv") %>% 
  clean_names()

# Clean up and select columns to match prior data
sol_pass <- sol_pass %>% 
  mutate(division_name = ifelse(is.na(division_name), "Virginia", division_name)) %>% 
  select("school_year","division_name","subject","test_level","pass_count","total_count","pass_rate")

# Read in prior data
sol_prior <- read_csv("data/2023_data/sol_pass.csv")

# Bind tables
sol_data <- rbind(sol_prior, sol_pass)

# Save
write_csv(sol_data, "data/sol_pass_rates.csv")

## .......................................................
# Students Eligible for Special Education Services ----
# Available at https://p1pe.doe.virginia.gov/buildatable/dec1 
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/buildatable/dec1) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2022-2023; 2023-2024; 2024-2025
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select Student Characteristics and Reporting Categories: Select All for all categories
              #   Race: All races
              #   Gender: All genders
              #   Placement: All placements
              #   Age Group: All ages
              #   Grade: All grades
              #   Disadvantaged: All students 
              #   Foster Care: All students
              #   Homeless: All students
              #   Migrant: All students
              #   Military Connected: All students
              #   English Learning: All students
              #   Disability Type: All students
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
sped <- read_csv("download_data/dec_1_statistics.csv") %>% 
  clean_names()

# Join with fall membership
sped <- sped %>% 
  left_join(fallmem)

# Create percents
sped <- sped %>% 
  mutate(percent_sped = (total_count/total_students) *100) %>% 
  rename(total_sped = total_count) %>% 
  mutate(division_name = ifelse(is.na(division_name), "Virginia", division_name),
         division_number = ifelse(is.na(division_number), 0, division_number)) 

# Read in prior data
sped_prior <- read_csv("data/2023_data/sped_services.csv") %>% 
  mutate(level = ifelse(division_name=="Virginia","State","Division"), .after = school_year) %>% 
  rename(total_students = total_count) %>% 
  select(-total_fulltime)

# Bind tables
sped_data <- rbind(sped_prior, sped)

# Save
write_csv(sped_data, "data/special_ed_services.csv")

## .......................................................
# English Learners ----
# Available at https://p1pe.doe.virginia.gov/buildatable/fallmembership
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/buildatable/fallmembership) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2023-2024; 2024-2025
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select Student Characteristics : Select All for all categories
#             Self-reporting categories: All students for all except 
#             English Learners: Yes
#             Include Former ELs: No (don't check)
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
ell <- read_csv("download_data/fall_membership_statistics_ell.csv") %>% 
  clean_names()

# Join with fall membership
ell <- ell %>% 
  left_join(fallmem)

# Create percents
ell <- ell %>% 
  select(-c(division_number, english_learners, ft_count, pt_count)) %>% 
  mutate(pct_english_learners = (total_count/total_students) *100,
         division_name = ifelse(is.na(division_name), "Virginia", division_name)) %>% 
  rename(english_learners = total_count)

# Read in prior data
ell_prior <- read_csv("data/2023_data/english_language_learners.csv") %>% 
  mutate(level = ifelse(division_name=="Virginia","State","Division"), .after = school_year) %>% 
  rename(total_students = student_count) %>% 
  select(-c(`...1`,division_number,locality,year))

# Bind tables
ell_data <- rbind(ell_prior, ell)

# Save
write_csv(ell_data, "data/english_language_learners.csv")

## .......................................................
# Chronic Absenteeism ----
# Available at https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education 
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education) 
#         (3) Download Spreadsheets for each year by state and division (save to download_data/absenteeism_school_climate/* state or division folders)

# Read in data
files_division <- list.files(path = "download_data/absenteeism_school_climate/division", pattern = "*.xlsx", full.names = TRUE)
files_state <- list.files(path = "download_data/absenteeism_school_climate/state", pattern = "*.xlsx", full.names = TRUE)

list_division <- map(files_division, ~read_excel(.x, sheet = 1))
list_state <- map(files_state, ~read_excel(.x, sheet = 1, col_types = "text"))

# bind into data frame
absent_division_full <- bind_rows(list_division) %>% 
  clean_names()

absent_state_full <- bind_rows(list_state) %>% 
  clean_names()

# Filter data
absent_division <- absent_division_full %>% 
  filter(div_name %in% c("Albemarle County", "Charlottesville City")) %>% 
  filter(subgroup == "All Students") %>% 
  filter(grade ==  "All Students") %>% 
  select(-c("div_num", "grade"))

absent_state <- absent_state_full %>% 
  filter(subgroup == "All Students") %>% 
  filter(grade ==  "All Students") %>% 
  mutate(div_name = "Virginia", .after = school_year) %>% 
  select(-c("grade"))

# merge tables
absenteeism <- rbind(absent_division, absent_state)

# Save
write_csv(absenteeism, "data/absenteeism.csv")

## .......................................................
# On-Time Graduation Rates ----
# Available at https://p1pe.doe.virginia.gov/buildatable/cohortgraduation
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/buildatable/cohortgraduation) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2023-2024; 2024-2025
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select Student Characteristics and Reporting Categories: Select All for all categories
#             Select Type of Graduation Rate: Virginia on-time graduation rate
#             Select Number of Years after entering High School: 4 years
#             Select Statistics (multi-select): Graduation rate, Total graduates, Students in cohort
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
grad_rates <- read_csv("download_data/cohort_statistics.csv") %>% 
  clean_names()

# Clean data
grad_rates <- grad_rates %>% 
  mutate(graduation_rate = str_remove(graduation_rate, "%"),
         graduation_rate = as.numeric(graduation_rate),
         division_name = ifelse(is.na(division_name), "Virginia", division_name),
         division_number = ifelse(is.na(division_number), 0, division_number))

# Read in prior data
grad_prior <- read_csv("data/2023_data/graduation_rates.csv") %>% 
  mutate(level = ifelse(division_name=="Virginia","State","Division"), .after = cohort_year)

# merge tables
grad_data <- rbind(grad_prior, grad_rates)

# Save
write_csv(grad_data, "data/graduation_rates.csv")

## .......................................................
# Post-Secondary Enrollment ----
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
  filter(group == "All Students") %>% 
  mutate(across(-c("group", "locality"), as.numeric))

# Read in prior data
post_second_prior <- read_csv("data/2023_data/postsecondary_education.csv")

# merge tables
post_second_data <- rbind(post_second_prior, post_second_ed)

# Save
write_csv(post_second_data, "data/postsecondary_education.csv")

## .......................................................
# Students Identified as Economically Disadvantaged ----
# Available at https://p1pe.doe.virginia.gov/buildatable/fallmembership
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/buildatable/fallmembership) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2023-2024; 2024-2025; 2025-2026
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select Student Characteristics : Select All for all categories
#             Self-reporting categories: All students for all except 
#             Disadvantaged: Yes
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
disadvan <- read_csv("download_data/fall_membership_statistics_disadvan.csv") %>% 
  clean_names()

# Join with fall membership
disadvan <- disadvan %>% 
  left_join(fallmem)

# Create percents
disadvan <- disadvan %>% 
  select(-c(division_number, disadvantaged, ft_count, pt_count)) %>% 
  mutate(percent_disadvantaged = (total_count/total_students) *100,
         division_name = ifelse(is.na(division_name), "Virginia", division_name)) %>% 
  rename(economically_disadvantaged_students = total_count)

# Read in prior data
disadvan_prior <- read_csv("data/2023_data/economically_disadvantaged_students.csv") %>% 
  mutate(level = ifelse(division_name=="Virginia","State","Division"), .after = school_year) %>% 
  rename(total_students = totstudents,
         percent_disadvantaged = percent) %>% 
  select(-c(division_number))

# Bind tables
disadvan_data <- rbind(disadvan_prior, disadvan)

# Save
write_csv(disadvan_data, "data/economically_disadvantaged_students.csv")

## .......................................................
# Student Behavior and Administrative Response (SBAR)  ----
# Available at https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2021-2022; 2022-2023; 2023-2024
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select SBAR Report: Events Report
#             Select Behavior Category: BESO; PD 
#             Selevt Behavior: All Behaviors
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
sbar_events <- read_csv("download_data/sbar_statistics.csv") %>% 
  clean_names()

# pivot to create columns by behavior code
sbar_events <- sbar_events %>% 
  pivot_wider(id_cols = -behavior_category_desc, names_from = behavior_category_code, values_from = number_of_events) %>% 
  clean_names()

# Join with fall membership
sbar_events <- sbar_events %>% 
  left_join(fallmem)

# Create rates and clean up
sbar_events <- sbar_events %>% 
  mutate(division_name = ifelse(is.na(division_name), "Virginia", division_name),
         pd = ifelse(is.na(pd), 0, pd)) %>%
  mutate(beso_rate = (beso/total_students)*1000,
         pd_rate = (pd/total_students)*1000) %>% 
  mutate(source = "SBAR") %>% 
  rename(beso_count = beso,
         pd_count = pd)

# Save
write_csv(sbar_events, "data/student_behavior_events.csv")

## .......................................................
# In-School Suspensions ----
# VDOE Safe Schools Information Resource (SSIR) (2016-17 to 2020-21) https://p1pe.doe.virginia.gov/pti/selection.do
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/pti/selection.do) 
#         (2) Make the following selections:
#             School Year: All
#             Region Name: All
#             Division Name: All
#             School Type: All
#             School Name : 		All
#             Offense Category : 		All
#             Offense Type : 		All
#             Discipline Type : 		In-School Suspension
#             Ethnicity : 		All
#             Grade : 		All
#             Gender : 		All
#             Disability : 		All
#             Report type: Disciplinary Outcome
#         (3) View CSV to download (save to download_data/)

# Read in data
iss_ssir_raw <- read_csv("download_data/Disciplinary Outcome Report.csv", skip = 17) %>% 
  clean_names()

# Clean up
iss_ssir_all <- iss_ssir %>% 
  filter(!(is.na(division_name))) %>% 
  select(division_name:individual_student_offenders) %>% 
  mutate(individual_student_offenders = as.numeric(individual_student_offenders),
         individual_student_offenders = ifelse(is.na(individual_student_offenders), 1, individual_student_offenders)) # make censored values 1

# Get state total
iss_ssir_state <- iss_ssir_all %>% 
  group_by(school_year) %>% 
  summarise(population = sum(population),
            individual_student_offenders = sum(individual_student_offenders)) %>% 
  mutate(division_name = "Virginia", .before = 1)

# Filter divisions
iss_ssir_div <- iss_ssir_all %>% 
  filter(division_name %in% c("Albemarle County Public Schools", "Charlottes ville City Public Schools")) %>% 
  select(-c("division_number", "discipline_type")) %>% 
  mutate(division_name = str_remove(division_name, " Public Schools"),
         division_name = str_replace(division_name, "Charlottes ville", "Charlottesville"))

# Bind tables
iss_ssir <- rbind(iss_ssir_div, iss_ssir_state) %>% 
  mutate(type = "In-School Suspension",
         source = "SSIR")

# VDOE SBAR Build-a-Table (2021-22 to 2023-24) https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2021-2022; 2022-2023; 2023-2024
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select SBAR Report: Sanction Report
#             Select Sanction Type: ISS - In-School Suspension (1/2 day or more)
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
iss_sbar_raw <- read_csv("download_data/sbar_statistics_iss.csv") %>% 
  clean_names()

# Join with fall membership
iss_sbar <- iss_sbar_raw %>% 
  left_join(fallmem) %>% 
  rename(population = total_students)

# Clean up
iss_sbar <- iss_sbar %>% 
  mutate(division_name = case_when(level == "State" ~ "Virginia",
                                   .default = division_name)) %>% 
  
  select(division_name, school_year, population, number_of_students) %>% 
  rename(individual_student_offenders = number_of_students) %>% 
  mutate(type = "In-School Suspension",
         source = "SBAR")

# Combine ISS sources
iss <- rbind(iss_ssir, iss_sbar)

# Create rate
iss <- iss %>% 
  mutate(rate = round((individual_student_offenders/population)*1000, 2), .after = individual_student_offenders)

# Save 
write_csv(iss, "data/in_school_suspensions.csv")

## .......................................................
# Out-of-School Suspensions ----
# VDOE SBAR Build-a-Table (2021-22 to 2023-24) https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
# Get data for Albemarle County Public Schools; Charlottesville City Public Schools and Virginia overall
# Go to:  (1) Download Data (https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351) 
#         (2) Make the following selections:
#             Select School Years (multi-select): 2021-2022; 2022-2023; 2023-2024
#             Select Report Level (multi-select): Division; State
#             Select Divisions (multi-select): Albemarle County; Charlottesville City 
#             Select SBAR Report: Sanction Report
#             Select Sanction Type: OSS - Out of school suspension
#         (3) Submit and click CSV Download (save to download_data/)

# Read in data
oss_sbar_raw <- read_csv("download_data/sbar_statistics_oss.csv") %>% 
  clean_names()


# Clean up and combine sanction codes
oss_sbar <- oss_sbar_raw %>% 
  group_by(school_year, level, division_number, division_name) %>% 
  summarise(number_of_students = sum(number_of_students)) %>% 
  ungroup()

# Join Fall membership
oss_sbar <- oss_sbar %>% 
  left_join(fallmem) %>% 
  rename(population = total_students) %>% 
  mutate(division_name = ifelse(is.na(division_name), "Virginia", division_name)) %>%
  select(division_name, school_year, population, number_of_students) %>% 
  mutate(type = "Out of school suspension",
         source = "SBAR")

# Create rate
oss_sbar <- oss_sbar %>% 
  mutate(rate = (number_of_students/population)*1000, .after = number_of_students)

# Read in previous OSS data
oss_prior <- read_csv("data/2023_data/school_suspensions.csv") %>% 
  select(division_name, school_year, population, total, rate, source) %>% 
  rename(number_of_students = total) %>%
  mutate(type = "Out of school suspension", .after = rate)

# Bind all data
oss <- rbind(oss_prior, oss_sbar)

# Save
write_csv(oss, "data/out_of_school_suspensions.csv")

  