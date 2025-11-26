## 📊 DS5011 Project: BRFSS Interactive Dashboard

This repository presents an interactive Shiny dashboard for analyzing public health data from the **Behavioral Risk Factor Surveillance System (BRFSS)**.

---

## Project Overview and Data Source

The BRFSS is the largest continuous telephone survey in the world, collaboratively conducted by the Centers for Disease Control and Prevention (CDC) and all U.S. states and territories. The survey collects crucial information on **health-related risk behaviors, chronic health conditions, and the use of preventive services**.

The goal of this project is to provide a dynamic tool that allows users to select specific survey questions and instantly visualize the aggregated results across key demographic and temporal factors. The core analysis focuses on **weighted prevalence percentages** and their confidence intervals for accurate population estimation.

---

## Data Initialization (`initialize.r`)

The **`initialize.r`** file is responsible for the crucial setup steps necessary before the dashboard can run:

1.  **Data Loading:** It loads the main, large BRFSS prevalence data CSV file and the separate question key CSV file into the R environment.
2.  **Function Definition:** It defines all necessary data transformation functions, such as `merge_ResponseID`, `merge_Response`, `merge_BreakoutID`, and `merge_Break_Out`. These functions are essential for standardizing responses (e.g., grouping multiple income codes into broader categories like "$50,000+") and ensuring consistency across the dataset.
3.  **Hierarchy Creation:** It generates the `layerQ` object, a structured data frame containing the **Class, Topic, and Question** hierarchy used to populate the dropdown menus in the Shiny application's sidebar.

**Note on Performance:** By design, the `dashboard.r` script sources this initialization file to ensure all merge logic and data objects are available globally. This process involves loading the raw CSV into memory, which may cause a significant delay (several minutes) when the application starts.

---

## Dashboard Functionality

The Shiny application is built around a powerful, flexible aggregation function that takes the massive, filtered BRFSS data and calculates weighted summary statistics.

### Core Analysis

The custom **`aggregate_brfss`** function performs the following steps:
1.  It uses the `Sample_Size` and `Data_value` columns to calculate the **effective total sample size**.
2.  It groups the data by the selected demographic variable (e.g., Age Group, Income Level) and the user's chosen response.
3.  It calculates the **aggregated weighted percentage** of responses.
4.  It estimates the **standard deviation** and **95% confidence intervals** around the aggregated percentage, providing statistically robust results.

### Visualization Panels

The main panel of the dashboard displays results across seven distinct tabs, each subsetting the data based on a different breakout category:

| Panel | Breakout Category | Visualization |
| :--- | :--- | :--- |
| **Overall** | Population Total | Single stacked bar showing response distribution. |
| **Temporal** | Year | Stacked bar chart showing trends over time. |
| **By Gender** | Gender | Stacked bar chart comparing responses between genders. |
| **By Age Group** | Age Group | Stacked bar chart comparing responses across age brackets. |
| **By Race/Ethnicity** | Race/Ethnicity | Stacked bar chart comparing responses across racial groups. |
| **By Education** | Education Level | Stacked bar chart comparing responses across education levels. |
| **By Income** | Income Level | Stacked bar chart comparing responses across income levels. |
| **By Location** | State/Territory | Bar chart highlighting the highest and lowest 10 states for the most relevant (least common) response. |

---

## Running the Application

To launch the dashboard, ensure you have the necessary R packages (`shiny`, `tidyverse`, `ggplot2`, and `arrow`) installed and that the main BRFSS data CSV file and the question key file are in your working directory alongside the project files (`initialize.r` and `app.r`).

Execute the `app.r` file in your R console:

1.  Open RStudio.
2.  Set the working directory to the project folder.
3.  Run the command to start the application.
