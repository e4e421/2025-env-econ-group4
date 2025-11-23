Link to work report document:\
([link to doc](https://emckclac-my.sharepoint.com/:f:/r/personal/k25059492_kcl_ac_uk/Documents/2025%20Env%20Econ%20Group%204?csf=1&web=1&e=giOShd)) \# Report structure

1.  Gather GDP and GNI data from 1970 to 2020 for all Asia countries, calculate the change and compare to Energy industry sector CO2 emission data from the same time frame.
    1.  Find the decoupling(absolute decoupling, relative decoupling, not decoupling) trend for countries.
    2.  Rank the decoupling level of countries for highest and lowest.
2.  Focus on the most and least decoupling countries in Asia, and dive into their economic and environmental performance.

# Key steps

1.  (*done*)Find a topic (Emission Contributor in continent and cities, and their policy)
2.  (*done*)Submit group topic ([Link to KEATS submission page](https://keats.kcl.ac.uk/mod/assign/view.php?id=9308925))
3.  (*done*)Choose a database (focus on [Emissions in Urban Areas](https://edgar.jrc.ec.europa.eu/dataset_ucdb))
4.  Submit roadmap
5.  (*done*)Download database
6.  Examine database for potential insights
    1.  Compare GDP growth and emission of countries to calculate decoupling.
7.  Clean data, check outlier and tackle blank
8.  Always update to GitHub (preferably use [GitHub desktop](https://desktop.github.com/download/))
9.  Write analysis code in Rmd file
10. Generate graphs to assist our interpretation
11. Write interpretation in word doc ([link to doc](https://emckclac-my.sharepoint.com/:f:/r/personal/k25059492_kcl_ac_uk/Documents/2025%20Env%20Econ%20Group%204?csf=1&web=1&e=giOShd))
12. make a report referencing visual from COP30 report ([link](https://github.com/e4e421/2025-env-econ-group4/blob/f04107b0c5df147f9c31b225540ea7bc29b8c430/Support%20documents/Sample%20report%20visual-State%20of%20the%20Climate%202025%20COP30%20(31%20oct).pdf))
13. Draft report submission
14. Final report submission
15. keep happy

# Data structure of this GitHub repository

-   Please ignore files start with a dot(.) those are binary file for computer to read. (i.e. **.DS_Store** and **.Rhistory**)
-   **main-R-markdown-workbook.Rmd** ([Link to file](https://github.com/e4e421/2025-env-econ-group4/blob/ea071cd25ba3f0fe3f34bbf8ca92680185730cdf/main-R-markdown-workbook.Rmd))
    -   For main R coding, dataset working directory(for local file retrieval) set by and maintained by lead programmer Tanmay.
-   **EDGAR_emiss_on_UCDB_v2024.csv** ([Link to file](https://github.com/e4e421/2025-env-econ-group4/blob/ea071cd25ba3f0fe3f34bbf8ca92680185730cdf/EDGAR_emiss_on_UCDB_v2024.csv))
    -   Our chosen data set, downloaded from [EDGAR - Emissions Database for Global Atmospheric Research: Emissions in Urban Areas](https://edgar.jrc.ec.europa.eu/dataset_ucdb) in 11 Nov 2025 by Marvin
-   **Supprot documents folder** ([Link to folder](https://github.com/e4e421/2025-env-econ-group4/tree/ea071cd25ba3f0fe3f34bbf8ca92680185730cdf/Support%20documents))
    -   Containing reference pdf files downloaded from KEATS
    -   **Sample report visual-State of the Climate 2025 COP30 (31 oct).pdf** ([link to file](https://github.com/e4e421/2025-env-econ-group4/blob/ea071cd25ba3f0fe3f34bbf8ca92680185730cdf/Support%20documents/Sample%20report%20visual-State%20of%20the%20Climate%202025%20COP30%20(31%20oct).pdf)) is our reference file for fianl report formatting. (file downloaded from [link](https://wmo.int/sites/default/files/2025-11/State%20of%20the%20Climate%202025%20Update%20COP30%20%2831%20oct%29.pdf) in 12 Nov 2025 by Marvin)

## Final report

Our final report in docx format can be found and edit online via [this link](https://emckclac-my.sharepoint.com/:f:/r/personal/k25059492_kcl_ac_uk/Documents/2025%20Env%20Econ%20Group%204?csf=1&web=1&e=giOShd). Because GitHub does not work well with docx editing, we can do everything in Rmd file first and then change it to docx and put into the document link above.

## Task division and event log

# Introduction

This is the data report for Environmental Economic module Data report group 4, team member includes **Tanmay Kharmarbha**, **Marvin Lee** and **Wei-Ting Tan**. We choose R as main programming language and GitHub as version control tool. All work files are stored and synced via GitHub repository under account of e4e421(Marvin Lee), using .Rmd file as main R code workbook for every member to update to.

### Tanmay Kharmarbha (Head of technical programming)

-   Lead programmer
    -   Coding for data organisation with R studio
    -   Run code checks

### Marvin Lee (Head of project management and infrastructure)

-   Project management
    -   GitHub repository maintenance
    -   File version and naming convention control
-   Assistant programmer
    -   Data

### Wei-Ting Tan (Head of reporting and design)

-   Data gathering for GDP and GNI data
-   Formulate final visual of the report

# Log book

**Nov 14 2025** - (all) **Meeting**

-   Action item before meeting
-   look through the emission data sheet, csv file and excel file
-   think about questions for the data and what you want to know more about, essentially they will be our topics
-   (for Marvin) design roadmap for the report development

-   decision:
    -   Compare GDP and GNI change with Energy sector CO2 emission change throughout 1970 to 2020. Region = Asia countries.
-   Task assignment:
    -   (WeiTing) Gather GDP and GNI data for all Asia countries from 1970 to 2020, with interval of 5 years or less.
    -   (Marvin) Draw roadmap for the final report

**Nov 11, 2025** - (all) **initial Meeting** Choose database source. Divide task. Choose database of EDGAR_emiss_on_UCDB_v2024.csv.

**Nov 8, 2025** - (Marvin) Create this file. Set GitHub repository. Set GitHub access.

# Other adminstrative materials
## Important dates

(*done*)Monday, November 17th - submit database choice

Thursday, December 17th - submit final report

## Submission

o Roadmap: latest week 7 (November 27th), 11:00 am (formative)

o First draft: latest week 10 (December 8th), 11:00 am (formative)

o Final report: Thursday, 18th December 2025, 10:00 am (summative)

## Assessments

Assessment 1 - Group Coursework (40%). File formats accepted are .doc, .docx. The deadline to submit is 10:00hrs Thursday 18th December 2025. Provisional marks will be released on or after Thursday 29th January 2026.

Assessment 2 - In-person Mid-term Examination (30%). Tuesday 11th November 2025. Provisional marks will be released on or after Tuesday 9th December.

Assessment 3 - In-person Final Examination (30%). The University Assessment Period 1 schedule is: Friday 9 - Thursday 15 January 2026. Specific dates will be released by the University Exams Team in November. Provisional marks will be released on or after Monday 16th February 2026.

## notes

You can find the groups for the group assignment on Keats now in the "[Assessment](https://keats.kcl.ac.uk/course/section.php?id=2202514)" section.

Please get in touch with your group members to make sure everyone is aware of the group allocation. Ideally, you manage to meet and discuss before our next lecture/seminar next Thursday.

The next milestone is that you select and submit your choice of a specific policy, e.g. China's Carbon Trading Scheme, or database, e.g. EU Pollution Registry, of your choice by Monday, November 17th. Feel free to reach out via email if you would like to discuss your choice.

## Additional learning resource

R and R studio: [Learn R in 39 minutes](https://www.youtube.com/watch?v=yZ0bV2Afkjc)

Github: [GitHub Basics Made Easy: A Fast Beginner's Tutorial!](https://www.youtube.com/watch?v=Oaj3RBIoGFc)
