# Predicting County-Level School Enrollment in the United States (1990–2001)

This project was developed as part of the final assignment for a Data Science course using R. The objective was to analyze and predict trends in public school enrollment at the county level by integrating multiple datasets on demographics, crime statistics, and economic indicators.

## 📊 Project Description

Using panel data from 1990 to 2001, this project merged datasets from:

- ELSI (Elementary/Secondary Information System) for school enrollment
- U.S. Census for demographic data (e.g., population, age distribution, racial composition, gender)
- FBI UCR data for county-level crime statistics
- BEA data for per capita income and employment

The data was cleaned, merged, and transformed into panel format. Demeaned variables were created for fixed-effects modeling.

## 🔍 Methodology

- Cleaned and standardized school enrollment and auxiliary datasets
- Merged on county and year identifiers using FIPS codes
- Generated visualizations for national trends in enrollment and predictors
- Conducted correlation analysis and multiple regression models
- Built individual, full, and parsimonious models to identify key predictors

## 🧠 Key Findings

- **Positive predictors**: Population, proportion of nonwhite residents, new violent crime rates, and per capita income were positively associated with school enrollment.
- **Negative predictors**: Proportion of males, individuals aged 15–24, and old crime rates negatively affected enrollment.
- The final regression model achieved an R² of ~57% using all predictors and ~13% in the most parsimonious version.

## 📁 Files

- `R_Final_Project_Part1.R`: Data cleaning, merging, and transformation steps
- `R_Final_Project_Part2.R`: Visualization, correlation, and regression modeling
- `DSII_R_Final_Project_Regressions_School_Enroll.docx`: Regression output interpretations

## 🛠️ Tools

- R, tidyverse, panelr, haven, readxl
- Linear regression and data visualization in base R

## 📌 Author

Shreya Sudhir Kadav  
Bachelor of Science in Data Science (Public Health concentration)  
The University of Toledo | Spring 2025
