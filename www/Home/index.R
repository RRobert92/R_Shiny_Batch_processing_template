################################################################################
# Shiny UI-Home
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################

# Shiny UI-Home  ---------------------------------------------------------------
  ## Home page seen after the shiny is started ---------------------------------

homeUI <- function(id) {
  ns <- NS(id)

  tags$div(class = "splash-container",
    tags$div(class = "splash",
      tags$h1(class = "splash-head",
        "Shiny Batch processing template"
      ),
      tags$p(class = "splash-subhead",
        "The template intended to allow for easy running the R script on multiple files wrap up in modern UI."
      ),
      
      actionButton("GetStarted", "Get Started", class = "asga-button asga-button-primary"),
      actionButton("Wiki", "Wiki", class = "asga-button asga-button-primary")
    ),
    
    tags$div(class = "footer l-box is-center",
      tags$p("© Copyright GPL V3.0 2019, Robert Kiewisz")
    )
  )
}

# UI-Footnote  ---------------------------------------------------------------

footnoteUI <- function(id) {
  ns <- NS(id)

  tags$div(class = "footer l-box is-center",
    tags$p("© Copyright GPL V3.0 2019, Robert Kiewisz")
  )
}
