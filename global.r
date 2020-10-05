################################################################################
# Shiny Global
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################
# Title of the app -------------------------------------------------------------
App_title <- "Shiny batch template"

# Global CSS  ------------------------------------------------------------------
includeCSS("www/css/style.css")

# Global HTML  -----------------------------------------------------------------
source("www/Home/index.R")
source("www/Get_Started/index.R")

# Global Utility  --------------------------------------------------------------
source("bin/Utility/Library.R") ## load all necessary library 
source("bin/Utility/Check_Data.R") ## check if data are in the right format
source("bin/Utility/Upload_Data.R") ## upload data in .xlsx or any other format
source("bin/Utility/Setting_Buttons.R") ## set-up the button for selecing analysis
source("bin/Utility/Pre_Analysis.R") ## standarise data befor analysis (sorting)
source("bin/Utility/Load_Data.R") ## Load uploaded data into the enviroment
source("bin/Utility/Save_Data.R") ## save .xlsx data from Data/... 

# Global server settings  ------------------------------------------------------
options(shiny.maxRequestSize = 1024 * 1024^2)
options(shiny.host = "127.0.0.1")
options(shiny.port = 7878)

# Global tool settings  --------------------------------------------------------
DataTest <<- 0
numfiles <<- 0
Minus_Distance <<- 0.035 # Minus end distance of any MT to the KMT [um]
MT_point_config <<- 0.035 # Distance of any MT to the MT [um]

# Global Functions for pre analysis --------------------------------------------
source("bin/Tools/Pre_Analysis/Sort_by_Fiber.R")
source("bin/Tools/Pre_Analysis/Select_Points.R")
source("bin/Tools/Pre_Analysis/Find_XYZ.R")
source("bin/Tools/Pre_Analysis/Kinetochore_Position.R")
source("bin/Tools/Pre_Analysis/Sort_All_Points_to_Start_From_the_Kinetochore.R")
source("bin/Tools/Pre_Analysis/T_Relative_Position.R")
source("bin/Tools/Pre_Analysis/Length_Distiribution.R")

# Global Functions for analysis ------------------------------------------------
source("bin/Tools/Analysis/T_Inter_Kinetochore_Dist.R")
source("bin/Tools/Analysis/T_Fiber_Area.R")
source("bin/Tools/Analysis/T_Bridging_MT.R")

source("bin/Utility/Report.R")

# Global Packages  -------------------------------------------------------------
source("bin/Packages/Analysis/A_IKD.R")
source("bin/Packages/Analysis/A_Fiber_Area.R")
source("bin/Packages/Analysis/A_MT_Bridging.R")

# Test Unit ---------------------------------------------------------------------
source("tests/Test_Output.R")