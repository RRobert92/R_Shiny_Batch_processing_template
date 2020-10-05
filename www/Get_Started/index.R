################################################################################
# Shiny UI-GetStarted
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################

# UI-GetStarted  ---------------------------------------------------------------

GetStarted_UI <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = App_title, # see Global.R
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar-GS",
    selected = "UploadData",

    # UI-GetStarted-Home  ----------------------------------------------------------

    tabPanel("Home", "Home"),

    # UI-GetStarted-UploadData  ----------------------------------------------------

    tabPanel("Upload Data", value = "UploadData",
             
      tags$div(class = "splash-container-GS",
        tags$div(class = "splash-GS",
          tags$h1(class = "splash-head-GS",
            "Upload your files"
          ),

          tags$p(class = "splash-subhead-GS",
            "Upload multiple .xlsx or any other files format. For more information see Utility/Upload_Data.R."
          ),

          tags$div(class = "splash-input-GS-row",
            tags$div(class = "splash-input-GS",
              "Upload file for analysis:",

              fileInput(ns("file"),
                label = "", 
                multiple = TRUE,
                accept = c(".xlsx", ".am")
              )
            ),
## This upload allows to upload already analyzed data for automatic plot generation.
            tags$div(class = "splash-input-GS",
              "Upload analyzed data:",

              fileInput(ns("file1"), 
                        label = "", 
                        multiple = TRUE, 
                        accept = c(".xlsx"))
            )
          ),

## The test unit can be deleted if needed. This unit allows running R code on standardized data for debugging
          tags$div(class = "splash-input-GS-row",
            tags$div(class = "splash-input-GS-test",

              actionBttn(
                inputId = "Test_unit",
                label = "Test",
                style = "material-flat",
                color = "primary"
              )
            )
          )
        ),

        tags$div(class = "footer l-box is-center",
          tags$p("© Copyright GPL V3.0 2019, Robert Kiewisz")
        )
      )
    ),

    # UI-GetStarted-Settings  -----------------------------------------------------
      ## This part is used to select which R script should be run
    tabPanel("Settings", value = "Settings",
             
      tags$div(class = "splash-container-GS",
        tags$div(class = "splash-GS",
          tags$h1( class = "splash-head-GS",
            "Set-Up the analysis"
          )
        ),

        tags$div(class = "table-GS",
          fluidRow(
            Setting_Buttons_UI("Home"), ## See bin/Utility/Setting_Buttons.R
            column(8, uiOutput(ns("Tool_Info_1"))) ## see bin/Utility/Setting_Buttons.R function Setting_Buttons_Server()
          ),

          tags$div(class = "table-GS-Center",
            fluidRow(
              actionBttn(
                inputId = "Submit",
                label = "Start Analysis",
                style = "material-flat",
                color = "primary"
              ),

              tags$div(class = "asga-button asga-button-primary",
                uiOutput(ns("Download_Button"))
              )
            )
          )
        ),

        tags$div(class = "footer l-box is-center",
          tags$p("© Copyright GPL V3.0 2019, Robert Kiewisz")
        )
      )
    ),

    # UI-GetStarted-Settings  -----------------------------------------------------
      ## UI for the result of the R script, automatic plot generation
    tabPanel("Report",value = "Report",
             
      tags$div(class = "splash-container-report",
        tags$h1(class = "splash-head-GS",
          "Analysis Report"
        )
      ),

      tags$div(class = "splash-content-report",
        tags$p(class = "splash-subhead-Report",
          "Settings for the plots"
        ),

        uiOutput(ns("Plot_Settings")), ## Add setting for plot, color, labels, etc. see bin/Utility/Report.R
        uiOutput(ns("Report_Page"))## Add setting for plot, color, labels, etc. see bin/Utility/Report.R
      ),

      tags$div(class = "footer-report l-box is-center",
        tags$p("© Copyright GPL V3.0 2019, Robert Kiewisz")
      )
    )
  )
}
