################################################################################
# Shiny Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################

# Shiny Server  ----------------------------------------------------------------
function(input, output, session) {

  # Hide pages  ----------------------------------------------------------------
  hideTab(inputId = "innavbar", target = "GetStarted")
  hideTab(inputId = "innavbar-GS", target = "Settings")
  hideTab(inputId = "innavbar-GS", target = "Report")

  # Get_Started button  --------------------------------------------------------
  observeEvent(input$GetStarted, {
    if (numfiles == 0) {
      updateTabsetPanel(session, "innavbar", selected = "GetStarted")
      showTab(inputId = "innavbar", target = "GetStarted")
      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
    } else {
      updateTabsetPanel(session, "innavbar", selected = "GetStarted")
      showTab(inputId = "innavbar", target = "GetStarted")
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }
  })

  # Wiki button  ---------------------------------------------------------------
  observeEvent(input$Wiki, {
    updateTabsetPanel(session, "innavbar", selected = "Wiki")
    hideTab(inputId = "innavbar", target = "GetStarted")
  })

  # Get file and Load data  ----------------------------------------------------
  callModule(Getfiles_Server, "Home")

  # Upload data UI  ------------------------------------------------------------
  output$Upload <- renderUI({
    UploadData_UI("GetStarted")
  })

  # Load standard data ---------------------------------------------------------
  observeEvent(input$`Test_unit`, {
    showTab(inputId = "innavbar-GS", target = "Settings")
    updateTabsetPanel(session, "innavbar-GS", selected = "Settings")

    numfiles <<- 1
    DataTest <<- 1
    Test <<- TRUE
  })

  # Download zip files with analyzed data --------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      "ASGA_Data.zip"
    },
    content = function(fname) {
      setwd("Data/")
      on.exit(setwd("../"))

      Zip_Files <- list.files(path = getwd(), pattern = ".xlsx$")
      zipr(zipfile = fname, files = Zip_Files)

      file.remove(Zip_Files)
    }
  )

  # Page responsiveness after loading data  ------------------------------------
  observeEvent(input$`Home-file`, {
    Test <<- FALSE

    showTab(inputId = "innavbar-GS", target = "Settings")
    if (DataTest == 1) {
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    } else {
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
  })

  # Page responsiveness after analyzing data  ----------------------------------
    ## This part collect information about all analyzed data and based on that 
    ## will generate plot. This part is only used when analyzed data were uploaded
  observeEvent(input$`Home-file1`, {
    Test <<- FALSE

    showTab(inputId = "innavbar-GS", target = "Report")
    updateTabsetPanel(session, "innavbar-GS", selected = "Report")

    File_name <<- as.data.frame(File_name)
    numfiles <<- readr::parse_number(File_name[nrow(File_name), 1])

    df <- data.frame()

    for (i in 1:nrow(File_name)) {
      name <- as.data.frame(str_split(File_name[i, 1], "_"))
      df[i, 1] <- as.numeric(name[2, 1])
      name <- as.data.frame(str_split(File_name[i, 1], paste("Data_", df[i, 1], "_", sep = "")))
      df[i, 2] <- as.character(name[2, 1])
    }

    File_name <<- df

    rm(df, name)

    # Collect information to start a plot after analysis -----------------------

    lapply(1:numfiles, function(i) {
      observeEvent(input[[paste("Data_label", i, sep = "_")]], {
        assign(paste("Data_label", i, sep = "_"),
          input[[paste("Data_label", i, sep = "_")]],
          envir = .GlobalEnv
        )
      })

      observeEvent(input[[paste("Data_color", i, sep = "_")]], {
        assign(paste("Data_color", i, sep = "_"),
          input[[paste("Data_color", i, sep = "_")]],
          envir = .GlobalEnv
        )
      })
    })

    callModule(Report_Plot_Settings, "Home") ## see bin/Utility/Report.R
    callModule(Report_Plot, "Home") ## see bin/Utility/Report.R
  })

  # Reactivity for the Home and GS button  -------------------------------------
  observe({
    if (req(input$`innavbar-GS`) == "Home") {
      updateTabsetPanel(session, "innavbar", selected = "Home")
      hideTab(inputId = "innavbar", target = "GetStarted")
    } else if (req(input$`innavbar-GS`) == "UploadData") {
      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
    } else if (req(input$`innavbar-GS`) == "Settings") {
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }
  })

  # Reactivity for the Settings button  ----------------------------------------
  callModule(Setting_Buttons_Server, "Home") ## see bin/Utility/Setting_Buttons.R

  # Reactivity for Pre-Analysis  -----------------------------------------------
   ## This part running the R code based on user selection for each uploaded file
  
  observeEvent(input$`Submit`, {
    if (Test == FALSE) {
      withProgress(message = "Analyzing:", value = 1, {
        for (y in 1:numfiles) {
          current_data <<- y
          incProgress(1 / numfiles, detail = paste("Data set no.", y, sep = " "))
          Sys.sleep(0.1)

          callModule(Load_Data, "Home")      ## see bin/Utility/Pre_Analysis.R
          callModule(Pre_Analysis, "Home")

          if (input$`Home-All_Anaysis` == TRUE) {
            callModule(A_IKD, "Home") ## see bin/Packages/Analysis/A_IKD.R
            callModule(A_Fiber_Area, "Home") ## see bin/Packages/Analysis/A_Fiber_Area.R
            callModule(A_MT_Bridging, "Home")## see bin/Packages/Analysis/_MT_Bridging.R
          }

          if (input$`Home-KMT_number` == TRUE) {
            callModule(A_KMT_number, "Home") ## see bin/Packages/Analysis/A_KMT_number.R
          }
          
          if (input$`Home-KMT_number` == TRUE) {
            callModule(A_Fiber_Area, "Home") ## see bin/Packages/Analysis/A_Fiber_Area.R
          }
          
          if (input$`Home-MT_Interaction` == TRUE) {
            callModule(A_MT_Bridging, "Home")## see bin/Packages/Analysis/_MT_Bridging.R
          }

          callModule(Save_Data, "Home")      ## see bin/Utility/Save_Data.R
        }

        showTab(inputId = "innavbar-GS", target = "Report")
        updateTabsetPanel(session, "innavbar", selected = "Report")

        ## This part collect information about all analyzed data and based on that 
        ## will generate plot.
        File_name <<- as.data.frame(ls(pattern = "Data_", envir = .GlobalEnv))
        numfiles <<- readr::parse_number(File_name[nrow(File_name), 1])

        df <- data.frame()

        for (i in 1:nrow(File_name)) {
          name <- as.data.frame(str_split(File_name[i, 1], "_"))
          df[i, 1] <- as.numeric(name[2, 1])
          name <- as.data.frame(str_split(File_name[i, 1], paste("Data_", df[i, 1], "_", sep = "")))
          df[i, 2] <- as.character(name[2, 1])
        }

        File_name <<- na.omit(df)

        rm(df, name)
      })

      # Download data ----------------------------------------------------------
      output$`Home-Download_Button` <- renderUI({
        downloadBttn("downloadData", label = "Download", style = "material-flat", color = "success")
      })

      # Collect information to start a plot after analysis ---------------------
      lapply(1:numfiles, function(i) {
        observeEvent(input[[paste("Data_label", i, sep = "_")]], {
          assign(paste("Data_label", i, sep = "_"),
            input[[paste("Data_label", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })

        observeEvent(input[[paste("Data_color", i, sep = "_")]], {
          assign(paste("Data_color", i, sep = "_"),
            input[[paste("Data_color", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })
      })

      callModule(Report_Plot, "Home") ## see bin/Utility/Report.R
    } else {
      ## Load data for test unit. In the example case excel file has 3 sheets
      Nodes <<- read_excel(
        "tests/ASGA_Test_Data_Set.xlsx",
        sheet = "Nodes"
      )

      Segments <<- read_excel(
        "tests/ASGA_Test_Data_Set.xlsx",
        sheet = "Segments"
      )

      Points <<- read_excel(
        "tests/ASGA_Test_Data_Set.xlsx",
        sheet = "Points"
      )
      
      ## Run test see tests/Test_Output.R
      callModule(Load_Data, "Home")   ## see tests/Test_Output.R
      callModule(Run_Test, "Home")    ## see tests/Test_Output.R
      callModule(Test_Test, "Home")   ## see tests/Test_Output.R

      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
      callModule(Test_Result, "Home") ## see tests/Test_Output.R
      
      ## Clean data/...
      setwd("Data/")
      Files <<- list.files(path = getwd(), pattern = ".xlsx$")
      file.remove(Files)
      setwd("../")
      
      ## if test pass restore environment to the beginning
      if (Test_df == TRUE) {
        rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      }

      Test <<- FALSE
      source("global.r")
    }
  })

  # Report page output ---------------------------------------------------------
  output$`Home-Plot_Settings` <- renderUI({
    Report_Plot_Settings("Report") ## see bin/Utility/Report.R
  })

  output$`Home-Report_Page` <- renderUI({
    tagList(
      if (length(File_name[File_name$V2 == "KMT_No", 2]) >= 1) {
        tagList(
          tags$p(class = "splash-subhead-Report", "KMTs number per kinetochore"),

          Report_Plot_KMT_No("Report")
        )
      }
    )
  })

  # Refresh for the Report page ------------------------------------------------
  observeEvent(input$Refresh, {
    callModule(Report_Plot, "Home") ## see bin/Utility/Report.R
  })
}
