################################################################################
# Module Check_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################

# Example for Checking Data  ---------------------------------------------------
Check_Data <- function(i) {
  tryCatch(
    {
      Test_Segments <- colnames(get(paste("Data", "Segments", i, sep = "_")))[1] == "Segment ID" &&
        colnames(get(paste("Data", "Segments", i, sep = "_")))[ncol(get(paste("Data", "Segments", i, sep = "_")))] == "Point IDs" &&
        colnames(get(paste("Data", "Segments", i, sep = "_")))[ncol(get(paste("Data", "Segments", i, sep = "_"))) - 3] == "length"
    },
    error = function(e) {}
  )

  if (!exists("Test_Segments")) {
    Test_Segments <- FALSE
  } else {}

  tryCatch(
    {
      Test_Pole1 <- colnames(get(paste("Data", "Nodes", i, sep = "_")) %>% select(Pole1)) == "Pole1"
    },
    error = function(e) {}
  )


  if (!exists("Test_Pole1")) {
    Test_Pole1 <- FALSE
  } else {}

  tryCatch(
    {
      Test_Pole2 <- colnames(get(paste("Data", "Nodes", i, sep = "_")) %>% select(Pole2)) == "Pole2"
    },
    error = function(e) {}
  )

  if (!exists("Test_Pole2")) {
    Test_Pole2 <- FALSE
  } else {}

  # State value of data check  --------------------------------------------------
  if (Test_Segments == TRUE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
    DataTest <<- 1
  } else if (Test_Segments == TRUE && which(colnames(get(paste("Data", "Segments", i, sep = "_"))) == "Pole1_00") >
    which(colnames(get(paste("Data", "Segments", i, sep = "_"))) == "Pole2_00")) {
    DataTest <<- 2
  } else if (Test_Segments == FALSE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
    DataTest <<- 3
  } else if (Test_Segments == TRUE && Test_Pole1 == FALSE) {
    DataTest <<- 4
  } else if (Test_Segments == TRUE && Test_Pole1 == FALSE) {
    DataTest <<- 5
  } else if (Test_Segments == TRUE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
    DataTest <<- 6
  } else if (Test_Segments == TRUE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
    DataTest <<- 7
  } else if (!exists(get(paste("Data", "Segments", i, sep = "_")))) {
    DataTest <<- 0
  }
}
