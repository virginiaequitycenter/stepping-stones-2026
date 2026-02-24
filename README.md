# Stepping Stones Report 
## Update Spring 2026

R scripts and data for updating the Stepping Stones Report.

### `data/`

Updated CSV files for current measures in report. CSV files created in `scripts/`.
  
### `scripts/`

The following scripts require `data/2023_data/` -- currently listed in `.gitignore` -- to merge updates with data from 2023 report. 

#### `vdoe.R`

Running this script requires `download_data/` -- currently listed in `.gitignore`. Instructions for retrieving VDOE data from the listed sources included in script.

Get, wrangle and save VDOE data for report. Prior report measures are merged with data from 2023 report. Script includes:

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
