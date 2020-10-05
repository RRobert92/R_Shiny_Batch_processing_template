################################################################################
# Module Test_Result
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################

# Run analysis -----------------------------------------------------------------
Run_Test <- function(input, output, session) {
  withProgress(message = "Analyzing:", value = 1, {
    incProgress(1, detail = "Test in progress...")
    Sys.sleep(0.1)

    Test_value <<- tibble()
    current_data <<- 1

    tryCatch(
      {
        callModule(Pre_Analysis, "Home") ## see/bin/Utility/Pre_Analysis.R
        Test_value[1, 1] <- TRUE
      },
      error = function(e) {
        Test_value[1, 1] <- FALSE
      }
    )
    names(Test_value)[1] <- "Pre_Analsis"

    tryCatch(
      {
        callModule(A_IKD, "Home") ## see/bin/Packages/Analysis/A_IKD.R
        Test_value[1, 2] <- TRUE
      },
      error = function(e) {
        Test_value[1, 2] <- FALSE
      }
    )
    names(Test_value)[2] <- "IKD"
    
    tryCatch(
      {
        callModule(A_MT_Bridging, "Home")
        Test_value[1, 3] <- TRUE
      },
      warning = function(w) {
        Test_value[1, 3] <- TRUE
        },
      error = function(e) {
        Test_value[1, 3] <- FALSE
      }
    )
    names(Test_value)[3] <- "MT Interaction"

    Test_value <<- Test_value
    File_name <<- as_tibble(ls(pattern = "Data_", envir = .GlobalEnv))
    File_name <<- na.omit(File_name)
  })
}

# Test analysis results --------------------------------------------------------
Test_Test <- function(input, output, session) {
  Test_df <<- data.frame()
  ## test what are the expected values for the standard data
  Test_df[1, 1] <<- nrow(File_name) == 3
  Test_df[1, 2] <<- ncol(Test_value) == length(Test_value[Test_value == TRUE])

  if (ncol(Test_df) == length((Test_df[Test_df == TRUE]))) {
    Test_df <<- TRUE
    Bad_funtions <<- 0
  } else {
    Test_df <<- FALSE
    Bad_funtions <<- which(Test_value[1, ] == FALSE, TRUE)
  }
}

# Show results  ----------------------------------------------------------------
Test_Result <- function(input, output, session) {
  ## Pop up messages after the test to confirm the results
  ## An error message is given as a number or a test which failed during the analysis
  if (Test_df == TRUE) {
    confirmSweetAlert(
      session,
      inputId = "Test_data_output",
      title = "Test result",
      text = paste("The test result was positive. All work as intended.", " Error no. ", as.character(Bad_funtions), sep = ""),
      type = "success",
      btn_labels = c("Confirm"),
      btn_colors = "#a5dc86"
    )
  } else {
    confirmSweetAlert(
      session,
      inputId = "Test_data_output",
      title = "Test result",
      text = paste("One of the test went wrong, check more infor in RStudio.
      Test results are included in `Test_value` data.frame.", "Error no. ", as.character(Bad_funtions[, 2]), sep = ""),
      type = "warning",
      btn_labels = c("Confirm"),
      btn_colors = "#f8bb86"
    )
  }
}
