#####################################################################################################
# Function Select_Points
#
# The core tool to remove ',' and assigned points to each MTs
#
# x variables in the function is a number of the MTs in the fiber (e.g. 8, 10 etc.)
# y variables in the function is a number of a column contain MTs information (e.g. 2, 3, 4 etc.)
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-16
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
#####################################################################################################

# Sort Points of each KMT  --------------------------------------------------------------------------
Select_Points <- function(x, y) {
  selected_points <- data.frame(str_split(gsub("[^[:digit:]]", ",", y[x, 2]), pattern = ","))
  points <- data.frame(Point_ID = selected_points[, 1])
}
