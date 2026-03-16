# Stepping Stones Report 
## Update Spring 2026

R scripts and data for updating the Stepping Stones Report.

### `data/`

Updated CSV files for current measures in report. CSV files created in `scripts/`.
  
### `scripts/`

The following scripts require `data/2023_data/` -- currently listed in `.gitignore` -- to merge updates with data from 2023 report. 

#### `vdoe.R`

Running this script requires `download_data/` -- currently listed in `.gitignore`. Instructions for retrieving VDOE data from the listed sources included in script.

Get, wrangle and save VDOE data for report. Updated report measures are merged with data from 2023 report. Script includes:

  - Fall Membership
    - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
    - Needed for calculating percents for other measures
  - SOL Pass Rates
    - Math: Grades 3, 5
    - Reading: Grades 3, 5
      - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/testresults
  - Students Eligible for Special Education Services
    - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/dec1
  - English Language Learners
    - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
  - Chronic Absenteeism (New for 2026)
    - Source: VDOE https://schoolquality.virginia.gov/download-data
  - On-Time Graduation Rates
    - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/cohortgraduation
  - Post-Secondary Enrollment
    - Source: VDOE https://p1pe.doe.virginia.gov/postsec_public/
  - Students Identified as Economically Disadvantaged
    - Source: VDOE https://p1pe.doe.virginia.gov/buildatable/fallmembership
  - Student Behavior and Administrative Response (SBAR)
    - Source: VDOE SBAR Build-a-Table https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
      - Relevant Behavior Code (Larger Categories):
        - BESO: Behaviors that Endanger Self or Others (BESO) These behaviors endanger the health, safety, or welfare of either the student or others in the school community.
        - PD: Behaviors described in the Virginia’s Unsafe School Choice Option Policy required by the federal Every Student Succeeds Act of 2015.
  - In-School Suspensions (New for 2026)
    - VDOE Safe Schools Information Resource (SSIR) (2016-17 to 2020-21) https://p1pe.doe.virginia.gov/pti/selection.do
    - VDOE SBAR Build-a-Table (2021-22 to 2023-24) https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
  - Out-of-School Suspensions
    - Source: VDOE SBAR Build-a-Table (2021-22 to 2023-24) https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351
    
#### `acs_5yr.R`

Get, wrangle and save US Census ACS 5-yr survey data for report. Updated report measures are merged with data from 2023 report. Script includes:

  - High School Degree Attainment
    - Source: ACS Table S1501 https://data.census.gov/table?q=S1501&tid=ACSST1Y2021.S1501
  - Youth Labor Force Participation and Unemployment
    - Source: ACS Table B23001 https://data.census.gov/table?q=b23001&g=0500000US51003,51540
  - Children in Two-Parent Households
    - Source: ACS Table B05009 https://data.census.gov/table/ACSDT5Y2023.B05009?q=B05009
  - Median Family income with children under 18 in household (New for 2026)
    - KidsCount (2010-2019): https://datacenter.aecf.org/data/tables/9184-median-income-of-families-with-own-children-in-household?loc=48&loct=5#detailed/5/6813,6836/true/1983/any/18208
    - Source (2020 and later): ACS Table B19125  https://data.census.gov/table/ACSDT5Y2024.B19125?q=B19125&g=050XX00US51003,51540

#### `econ_fam_stability_data.R`

Get, wrangle and save additional sources on Economic and Family Stability data for report. Updated report measures are merged with data from 2023 report. Script includes:

  - Children Living below Poverty Threshold 
    - Source: SAIPE https://www.census.gov/programs-surveys/saipe/data/datasets.html
  - Assessments and Investigations by Child Protective Services 
    - Source: VDSS PowerBI dashboard (2021-2024) https://cpsaccountability.dss.virginia.gov/index-social-services.html
  - Children in Foster Care
    - Source: VDSS (2020-2025) https://www.dss.virginia.gov/geninfo/reports/children/fc.cgi
  - McKinney-Vento
    - Source: Ed Data Express https://eddataexpress.ed.gov/download/data-library?field_year_target_id=All&field_population_value=&field_data_topic_target_id=All&field_reporting_level_target_id=All&field_program_target_id=42&field_file_spec_target_id=All&field_data_group_id_target_id=All&combine=
    - This data source was not updated since retrieving data for the 2025 Community Well-being Profiles
    - Data from that project was copied over in stepping-stones-2026/data