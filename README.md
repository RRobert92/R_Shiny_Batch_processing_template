# Open-Source R Shiny Batch processing template
![GitHub release (latest by date)](https://img.shields.io/github/v/release/RRobert92/R_Shiny_Batch_processing_template)
![GitHub top language](https://img.shields.io/github/languages/top/RRobert92/R_Shiny_Batch_processing_template)
![GitHub](https://img.shields.io/github/license/RRobert92/R_Shiny_Batch_processing_template)
![GitHub Release Date](https://img.shields.io/github/release-date/RRobert92/MR_Shiny_Batch_processing_template)
![GitHub contributors](https://img.shields.io/github/contributors/RRobert92/R_Shiny_Batch_processing_template)
![Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

**Shiny Batch processing template** is an open-source template that allows for easy implementation of batch processing on multiple files with R scripts with modern GUI. 
Benefits of applying this template with your workflow:
  
1. Online access

2. Simple and intuitive to use with implemented UI. Only require installation of the R software environment and optional Rstudio.

3. It is designed from the ground up to incrementally scale up based on evolving use cases. 

4. Allows working on projects reproducible and compare data.

5. Allows to uniform and completely automate analysis.

# Table of Contents

* [Quick start](#Quick_start)
  * [Data Preparation](#Quick_start_DP)
  * [Dependency](#Dependency)
  * [Installation](#Quick_start_IN)
* [Contributing](#Contributing)
* [Copyright](#Copyright)
  
<a name="Quick_start"></a>
# Quick start
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.
You can see the real life use example of this template with in an online-version under the [Shinyapp.io](https://kiewisz.shinyapps.io/ASGA/).

<a name="Quick_start_DP"></a>
### Fork the repository and run the Shiny_template.Rproj
```
To use the template with your file format you need modifying the following files:

* Utility/Upload_Data.R <- select file format for upload and specified what to upload from the file
* Utility/Load_Data.R <- Load data for each batch round from the file format
* Utility/Check_Data.R <- Check file structure of the uploaded file to eliminate potential error during the analysis

To add any R script you need modified the following files:

* bin/tool/... <- Add your function that is needed for the analysis
* bin/Packages/Analysis/... <- Add a R script that run on loaded data with "Load_Data.R" with the function from "bin/tool/..."
* Global.R <- Source tool and packages added above
* Utility/Setting_Buttons.R <- Create new button and it reactivity for your tool. This button will be used in server.R for selecting which R script should be run 
* Server.R <- Enable newly created button under ObserveEvent(input$'Submit'){}
* Utility/Save_Data.R <- Add which data from the .Globalenviroment should be saved as a .xlsx file in the Data/... folder. This is used in Export_Data.R to collect all files and zip-them for the export.
* Utility/Report.R <- Add auto-generating plot at the end of the analysis.
```

<a name="Dependency"></a>
### Dependency for Shiny template
```
R v3.5.3 or newer
Rstudio v1.2 or newer
Java SE 11 (LTS)

R library
- shiny
- shinycssloaders
- shinyWidgets
- shinyBS
- shinyalert
- colourpicker
- readxl
- plyr
- tidyverse
- ggplot2
- egg
- base
- xlsx
- zip
```

<a name="Quick_start_IN"></a>
### Installation
```
install.packages("remotes")
remotes::install_github("RRobert92/R_Shiny_Batch_processing_templates")

```
<a name="Contributing"></a>
# Contributing
Contributions, collaborators and/or constructive criticism are welcome! Please see our Contributing Guide "Soon available" for more details.

<a href="https://sourcerer.io/rrobert92"><img src="https://avatars0.githubusercontent.com/u/56911280?v=4" height="50px" width="50px" alt=""/></a>
<a name="Copyright"></a>
## Copyright
This project is distributed under the General Public License (GPL) version 3.0 - see the [LICENSE.md](LICENSE.md) file for details.
