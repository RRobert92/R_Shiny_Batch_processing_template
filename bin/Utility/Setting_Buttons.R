################################################################################
# Module Setting_Buttons UI/Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-19
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
################################################################################

# Setting_BUttons_UI  ----------------------------------------------------------
Setting_Buttons_UI <- function(id) {
  ns <- NS(id)

  column(4,
    materialSwitch(
      inputId = ns("All_Anaysis"),
      label = "All",
      value = TRUE,
      right = TRUE,
      status = "info"
    ),
    materialSwitch(
      inputId = ns("IKD"),
      label = "Example 1: Inter-kinetochore Distance",
      value = FALSE,
      right = TRUE,
      status = "info"
    ),
    
    materialSwitch(
      inputId = ns("Fiber_Area"),
      label = "Example 2: Fiber Area & Neighorhood Densit",
      value = FALSE,
      right = TRUE,
      status = "info"
    ),
    
    materialSwitch(
      inputId = ns("MT_Interaction"),
      label = "Example 3: MT Interaction (parallel processing)",
      value = FALSE,
      right = TRUE,
      status = "info"
    )
  )
}

# Setting_BUttons_Server  -------------------------------------------------------
Setting_Buttons_Server <- function(input, output, session) {
  
  # Reactivity for IKD ----------------------------------------------------------
  observeEvent(input$`IKD`, {
    All_Closed()
    Any_One()
    
    Sys.sleep(0.1)
    
    output$`Tool_Info_1` <- renderUI({
      if (input$`IKD` == TRUE) {
        "This example run on single core, itarating throw the pre-processed data.
        For more information see 'Wiki' page"
      }
    })
    
    if (input$`IKD` == TRUE) {
      confirmSweetAlert(
        session = session,
        type = "question",
        inputId = "IKD_confirmation",
        input = "text",
        title = "Want to confirm ?",
        text = "The Inter-Kinetochore distance will be calculated. This analysis relies on corresponding k-fiber labels.
      e.g. sister-kinetochore for Pole1_00 is Pole2_00.",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }
    
    observeEvent(input[["IKD_confirmation"]], {
      if (input[["IKD_confirmation"]] == FALSE) {
        updateMaterialSwitch(session, "IKD", FALSE)
      }
    })
  })
  
  # Reactivity for fiber area ---------------------------------------------------
  observeEvent(input$`Fiber_Area`, {
    All_Closed()
    Any_One()
    
    Sys.sleep(0.1)
    
    output$`Tool_Info_1` <- renderUI({
      if (input$`Fiber_Area` == TRUE) {
        "This tool will analyze the area of each fiber based on the polygon approach, and
        the neighborhood density along Kinetochore-Pole axis.
        For more information see 'Wiki' page."
      }
    })
    
    if (input$`Fiber_Area` == TRUE) {
      inputSweetAlert(
        session = session,
        type = "info",
        inputId = "Fiber_area_config",
        input = "text",
        title = "Set-up analysis parameter",
        text = "Bin size used to calculate fiber area every specified distance on the spindle pole axis. Unit [nm]"
      )
    }
    
    observeEvent(input[["Fiber_area_config"]], {
      assign("Fiber_area_config",
             round(as.numeric(input[["Fiber_area_config"]]) / 20, 0) - 1,
             envir = .GlobalEnv
      )
    })
  })
  
  # Reactivity for MT interaction button --------------------------------
  observeEvent(input$`MT_Interaction`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`MT_Interaction` == TRUE) {
        "The example of paraller processing with shiny.
        For more information see 'Wiki' page"
      }
    })

    if (input$`MT_Interaction` == TRUE) {
      confirmSweetAlert(
        session = session,
        type = "question",
        inputId = "Interaction_confirmation", 
        input = "text",
        title = "Want to confirm ?",
        text = "These tools usually take a lot of computation time, let the user decide if
        they want to wait!
        It is strongly suggested to run this analysis using a computer cluster not shiny.io server.",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }

    observeEvent(input[["Interaction_confirmation"]], {
      if (input[["Interaction_confirmation"]] == FALSE) {
        updateMaterialSwitch(session, "MT_Interaction", FALSE)
      } else if (input[["Interaction_confirmation"]] == TRUE &&
        input$`MT_Interaction` == TRUE) {
        inputSweetAlert(
          session = session,
          type = "info",
          inputId = "MT_point_config", 
          input = "text",
          title = "Set-up analysis parameter",
          text = "Set up a threshold for the analysis, or any other variable you want 
        to let user to change. Unit [um]"
        )
      }
    })

    observeEvent(input[["MT_point_config"]], {
      assign("MT_point_config",
        as.numeric(input[["MT_point_config"]]),
        envir = .GlobalEnv
      )
    })
  })

  # Reactivity for all button --------------------------------------------------

  observeEvent(input$`All_Anaysis`, {
    All_Closed()
    Any_One()
    
    Sys.sleep(0.1)
    if (input$`All_Anaysis` == TRUE) {
      updateMaterialSwitch(session, "IKD", FALSE)
      updateMaterialSwitch(session, "Fiber_Area", FALSE)
      updateMaterialSwitch(session, "MT_Interaction", FALSE)

      output$`Tool_Info_1` <- renderUI({
        "All analysis will be run with the stamdard settings.## see Global.R
        For more information see 'Wiki' page"
      })
    }
  })

  All_Closed <- function() {
    if (input$`IKD` == FALSE &&
        input$`Fiber_Area` == FALSE &&
        input$`MT_Interaction` == FALSE) {
      updateMaterialSwitch(session, "All_Anaysis", TRUE)
    }
  }

  Any_One <- function() {
    if (input$`IKD` == TRUE ||
        input$`Fiber_Area` == TRUE ||
        input$`MT_Interaction` == TRUE) {
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
  }
}
