#####################################################################################################
# Function Sort_by_Fiber
#
# The core tool to search for all MTs that belonged to each label
#
# x variables in the function is a number of a column contain KMTs information
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-16
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
#####################################################################################################

# Sort each fiber  ----------------------------------------------------------------------------------
Sort_by_fiber <- function(x) {
  fiber <- Segments %>% filter_at(vars(starts_with(x)), any_vars(. >= 1))

  fiber %>% select(1, which(colnames(Segments) == "Point IDs"), which(colnames(Segments) == "length"))
}
