class_work Overview
===================

## Project Summary

The `class_work` directory stores group assignments from the Fall 2024 R for Data Science course. The work spans two themes: building an R Shiny dashboard for surplus fruit data and performing bibliometric analysis on Web of Science (WoS) exports. This README summarizes the folder layout, the purpose of each subproject, and how to run the code so you can publish it to GitHub or share it with collaborators.

## Directory Structure

- `10.30 fruits/`
  - `Group3_build_in_shiny/`: complete R Shiny source code and assets
    - `app.R`: main entry point for the interactive dashboard
    - `single_farmer_500_records.csv`: sample dataset consumed by the app
    - `www/icons/`: icon set for the ten fruit types
    - `rsconnect/`: ShinyApps.io deployment metadata (for optional cloud hosting)
  - `Group3_Digital_Prototyping.jpg` / `Group3_Hand_Prototyping.jpg`: design mockups
- `11.4:7 WOS/`
  - `cluster.R`: LDA topic modeling pipeline with supporting visualizations
  - `EDA_part1.R`: data cleaning, categorical summaries, and exploratory visuals
  - `EDA_part2.R`: author citation ranking, document type mix, and journal word cloud
  - `Class1_Group3.docx`: original project write-up
  - `merged_publications_Robotics.xlsx`: consolidated WoS robotics dataset

## 10.30 fruits — Shiny Dashboard

- **Highlights**
  - Sidebar controls for fruit type, farmer selection, freezing status, and price range
  - Main area delivers Plotly bar charts (market overview and farmer breakdown), a data table, and a numeric value box
  - Custom color palette and iconography matched to the ten fruits
- **Requirements**
  - R (version 4.2 or newer recommended)
  - R packages: `shiny`, `tidyverse`, `DT`, `plotly`, `RColorBrewer`, `scales`
- **Run Locally**
  1. Change into `10.30 fruits/Group3_build_in_shiny/`
  2. Launch from the R console:
     ```r
     setwd("/Users/hongyuecheng/R_BigData/class_work/10.30 fruits/Group3_build_in_shiny")
     shiny::runApp("app.R")
     ```
  3. Your browser will open the Shiny app automatically
- **Deployment Notes**
  - `rsconnect/shinyapps.io/mikehong/in_class_act.dcf` contains a saved ShinyApps.io configuration that can be reused with `rsconnect::deployApp()`

## 11.4-7 WOS — Bibliometric Analysis

- **Data Sources**
  - Five WoS export files named `savedrecs-*.xls`; `EDA_part1.R` merges them into `merged_publications_Robotics.xlsx`
- **Script Overview**
  - `EDA_part1.R`: computes WoS category frequencies, publication-year trends, and top 15 publisher countries while exporting PNG plots
  - `EDA_part2.R`: uses the merged Excel file to produce author citation rankings, document type distribution, and a journal word cloud
  - `cluster.R`: trains an LDA topic model, outputs top terms per topic, related word clouds, and citation impact summaries
- **Requirements**
  - R packages: `tidyverse`, `readxl`, `dplyr`, `treemapify`, `ggplot2`, `stringr`, `scales`, `wordcloud`, `RColorBrewer`, `tm`, `topicmodels`, `tidytext`, `writexl`
- **Quick Start**
  1. Place the WoS exports (`savedrecs-*.xls`) inside `11.4:7 WOS/`
  2. Run `EDA_part1.R` to build the merged dataset and initial visuals
  3. Run `EDA_part2.R` for author, document type, and journal analyses
  4. Run `cluster.R` for topic modeling and citation insights
- **Caveats**
  - `EDA_part2.R` currently reads `merged_publications__Robotics.xlsx` (double underscore); update it to `merged_publications_Robotics.xlsx` before execution
  - Confirm the dataset contains populated `Article Title` and `Abstract` columns prior to running the LDA workflow

## Suggested Git Commit Message

- `Initial commit: add shiny fruit dashboard and WoS analysis scripts`
- Keep dependency and run instructions in the README so reviewers can onboard quickly

## Licensing and Copyright

- Add a `LICENSE` file (for example, MIT License) at the repository root to clarify usage rights
- Verify that imagery and WoS-derived data comply with the relevant usage policies before redistribution


