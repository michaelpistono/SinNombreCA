# SinNombreCA

**R Shiny web application for visualizing Sin Nombre hantavirus cases in California**

---

## 🚀 Overview

This repository contains two complementary Shiny apps:

1. **`app.R`**  
   - A self-contained, local version that reads:  
     - **`Deer Mouse Prevalence.csv`** (vector data)  
     - **`Countypopulations.csv`** (human population counts)  
     - California county shapefiles (`.shp`, `.dbf`, `.shx`, etc.)  
   - Merges data by county FIPS code and renders:  
     - Choropleth maps of mouse seropositivity, human cases, and prevalence  
     - Bar charts of county‐level trends  

2. **`SNVdashboard.R`**  
   - A “live” version that fetches its data from remote URLs (GitHub raw CSV & CA Open Data shapefile ZIP)  
   - Includes helper functions to download and unzip shapefiles on the fly  
   - Requires API tokens (GitHub & ESRI) stored in the script  

Use whichever app fits your workflow:  
- **Local**: edit and run `app.R` in RStudio.  
- **Remote**: launch `SNVdashboard.R` for automatic data updates.

---


## ⚙️ Requirements

- **R** ≥ 4.0  
- **R packages** (install with `install.packages()`):

  ```r
  install.packages(c(
    "shiny", "tidyverse", "readr", "leaflet", "sf", 
    "reshape2", "magrittr", "patchwork", "car", "httr"
  ))

---

🎯 Features
Interactive maps with leaflet choropleths

Bar charts of:

Deer mouse seroprevalence (2000–2022)

Human hantavirus cases (1980–2022)

Cases per 10,000 population

Dynamic data fetching (remote app only)

County‐level merging via FIPS/GEOID codes
