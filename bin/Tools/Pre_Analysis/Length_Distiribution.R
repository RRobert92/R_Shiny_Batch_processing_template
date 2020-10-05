######################################################################################
# Tool Length_Distribution
#
# The analysis tool extract length and (+)/(-) end distance to the Pole
#
# x variable in the function is a number of a KMTs in the fiber (e.g. 1, 2, 3... etc.)
# y variable in the function is the name of the pole to which KMTs is associated
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-03-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
#####################################################################################

# Tool LD  --------------------------------------------------------------------------
Analyse_LD <- function(x, y) {

  # Get position of all (+) ends in the fiber, calculate median position,
  # and take it as a kinetochore position

  Plus_end <- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))) {
    Plus_end[i, 1:3] <- get(paste(colnames(Segments)[x], i, sep = "_"))[1, 2:4]
  }
  
  Plus_end <- data.frame(
    X_Median = c(median(as.matrix(Plus_end[1]))),
    Y_Median = c(median(as.matrix(Plus_end[2]))),
    Z_Median = c(median(as.matrix(Plus_end[3])))
  )

  Bind_Data <- data.frame()
  Plus_Distst_to_kinetochore_core <- sqrt((Plus_end[1, 1] - (Kinetochore_projected[1, 1]))^2 + (Plus_end[1, 3] - (Kinetochore_projected[1, 3]))^2)
  Plus_Distst_to_pole <- sqrt((Plus_end[1, 1] - y[1, 1])^2 + (Plus_end[1, 2] - y[1, 2])^2 + (Plus_end[1, 3] - y[1, 3])^2)

  elipse <- data.frame()

  R25 <- (((Plus_end[1, 1] - Kinetochore_projected[1, 1])^2 / (Rx25^2)) +
    ((Plus_end[1, 3] - Kinetochore_projected[1, 3])^2 / (Rz25^2))) <= 1
  
  R50 <- (((Plus_end[1, 1] - Kinetochore_projected[1, 1])^2 / (Rx50^2)) +
    ((Plus_end[1, 3] - Kinetochore_projected[1, 3])^2 / (Rz50^2))) <= 1
  
  R100 <- (((Plus_end[1, 1] - Kinetochore_projected[1, 1])^2 / (Rx100^2)) +
    ((Plus_end[1, 3] - Kinetochore_projected[1, 3])^2 / (Rz100^2))) <= 1

  if (R25 == TRUE && R50 == TRUE && R100 == TRUE) {
    elipse[1, 1] <- "25%"
  } else if (R50 == TRUE && R100 == TRUE) {
    elipse[1, 1] <- "50%"
  } else if (R100 == TRUE) {
    elipse[1, 1] <- "100%"
  } else if (R25 == FALSE && R50 == FALSE && R100 == FALSE) {
    elipse[1, 1] <- "100%"
  }
  # Get position of the kinetochore on the metaphase plate------------------------------

  for (i in 1:nrow(get(colnames(Segments)[x]))) {
    Minus_end <- paste(colnames(Segments)[x], i, sep = "_"
                       )
    Minus_Distst_to_the_pole <- sqrt((y[1, 1] - (get(Minus_end)[nrow(get(Minus_end)), 2]))^2 +
      (y[1, 2] - (get(Minus_end)[nrow(get(Minus_end)), 3]))^2 +
      (y[1, 3] - (get(Minus_end)[nrow(get(Minus_end)), 4]))^2)

    Bind_Data [i, 1] <- get(colnames(Segments)[x])[i, 1]
    Bind_Data [i, 2] <- get(colnames(Segments)[x])[i, 3] / 10000
    Bind_Data [i, 3] <- Minus_Distst_to_the_pole
    Bind_Data [i, 4] <- Plus_Distst_to_kinetochore_core
    Bind_Data [i, 5] <- Plus_Distst_to_pole
    Bind_Data [i, 6] <- elipse
    Bind_Data [i, 7] <- colnames(Segments)[x]
  }

  names(Bind_Data)[1] <- "Segment ID"
  names(Bind_Data)[2] <- "length"
  names(Bind_Data)[3] <- "minus_dist_to_pole"
  names(Bind_Data)[4] <- "plus_dist_to_kinetochore_core"
  names(Bind_Data)[5] <- "plus_dist_to_pole"
  names(Bind_Data)[6] <- "Elipse_Position"
  names(Bind_Data)[7] <- "Fiber_Name"

  Bind_Data
}
