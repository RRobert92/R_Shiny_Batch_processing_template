################################################################################
# Function Find_XYZ
#
# The core tool to assigned MT points with XYZ coordinates
#
# x variables in the function is a DF name with points assigned to the MT
# (e.g. Pole1_01_1)
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-16
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
################################################################################

# Pick single KMT  -------------------------------------------------------------
Find_XYZ <- function(x) {
  points_MT <- as.data.frame(as.numeric(as.character(x$Point_ID)))
  names(points_MT)[1] <- "Point_ID"

  joined_data <- join_all(list(points_MT, Points), by = "Point_ID")

  mutate_all(
    joined_data,
    function(y) as.numeric(as.character(y))
  )
}
