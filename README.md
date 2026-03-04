#  Carbon Prints Analysis

[![R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Framework-Shiny-orange.svg)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**CarbonVista** is an interactive R Shiny application built to analyze air quality pollutants and calculate personal carbon footprints. By integrating environmental data from Nairobi with modern visualization tools and AI-driven insights, CarbonVista empowers users to understand their environmental impact and take action through sustainability projects.

---

##  Key Features

* **Interactive Dashboard:** Visualize PM2.5, Organic Carbon (OC), and Black Carbon (BC) trends using `Plotly` and `Leaflet` maps.
* **Carbon Footprint Calculator:** Input your electricity, gas, and driving habits to see your annual $CO_2$ tonnage.
* **Carbon Credit Estimator:** Calculates the "Carbon Credits" needed to offset your lifestyle based on dietary choices (Vegetarian, Average, High Meat).
* **AI-Powered Insights:** Uses the OpenAI GPT API to provide dynamic information on how to get involved in climate projects.
* **Sustainability Hub:** Direct access to join or contribute to Solar Energy, Tree Plantation, and Wind Power initiatives.

---

##  Installation & Setup

To run this application locally, follow these steps:

### 1. Install Required Libraries
Open R or RStudio and run:

```r
install.packages(c("shiny", "leaflet", "plotly", "bslib", "dplyr", 
                   "ggplot2", "shinyjs", "httr"))


