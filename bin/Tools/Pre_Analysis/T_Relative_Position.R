##########################################################################################################
# Function Relative_Position
#
# The core tool to sort MT points based on their (+) and (-) end
#
# y variables in the function is a DF name with points assigned to the MT (e.g. Pole1_01_1)
#
# Relative position of point between kinetochore and pole ( 1 - 0 ) for Pole_1 and Pole_2
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-16
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
#########################################################################################################

# Relative Position for Pole1 ---------------------------------------------------------------------------
relativ_pos_1 <- function(x, y) {
  relativ_pos_part1 <- lapply(y[3], function(z) {y[3] - Pole1[1, 2]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y Coord"]][["Y Coord"]])

  relativ_pos_part2 <- x[1, 3] - Pole1[1, 2]
  
  relativ_positon <- lapply(
    relativ_pos_part1,
    function(z) {round(relativ_pos_part1[1] / relativ_pos_part2, 2)}
  )

  relat_pos <- data.frame(relativ_positon[["relativ_pos_part1...Y.Coord......Y.Coord..."]][["relativ_pos_part1...Y.Coord......Y.Coord..."]])
  names(relat_pos)[1] <- "Relative_Position"
  cbind(y, relat_pos)
}

# Relative Position for Pole2 ---------------------------------------------------------------------------
relativ_pos_2 <- function(x, y) {
  relativ_pos_part1 <- lapply(y[3], function(z) {y[3] - Pole2[1, 2]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y Coord"]][["Y Coord"]])

  relativ_pos_part2 <- x[1, 3] - Pole2[1, 2]
  
  relativ_positon <- lapply(
    relativ_pos_part1,
    function(z) {round(relativ_pos_part1[1] / relativ_pos_part2, 2)}
  )

  relat_pos <- data.frame(relativ_positon[["relativ_pos_part1...Y.Coord......Y.Coord..."]][["relativ_pos_part1...Y.Coord......Y.Coord..."]])
  names(relat_pos)[1] <- "Relative_Position"
  cbind(y, relat_pos)
}
