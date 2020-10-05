#####################################################################################################
# Function Sort_All_Points_to_Start_From_the_Kinetochore
#
# The core tool to find the mean position of all kinetochores and project it to the spindle pole axis
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-16
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
#####################################################################################################

# Calculate kinetochore position for Pole1  ---------------------------------------------------------
Sort_by_distance_to_pole1 <- function(y) {
  position <- data.frame(
    x = c(Pole1[1, 1], Pole1[1, 1]),
    y = c(Pole1[1, 2], Pole1[1, 2]),
    z = c(Pole1[1, 3], Pole1[1, 3]),
    x1 = c(y[1, 2], y[nrow(y), 2]),
    y1 = c(y[1, 3], y[nrow(y), 3]),
    z1 = c(y[1, 4], y[nrow(y), 4])
  )

  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))

  if (position[1, 7] < position[2, 7]) {
    y %>% arrange(desc(Point_ID))
  } else {
    y
  }
}

# Calculate kinetochore position for Pole2  ---------------------------------------------------------
Sort_by_distance_to_pole2 <- function(y) {
  position <- data.frame(
    x = c(Pole2[1, 1], Pole2[1, 1]),
    y = c(Pole2[1, 2], Pole2[1, 2]),
    z = c(Pole2[1, 3], Pole2[1, 3]),
    x1 = c(y[1, 2], y[nrow(y), 2]),
    y1 = c(y[1, 3], y[nrow(y), 3]),
    z1 = c(y[1, 4], y[nrow(y), 4])
  )

  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))

  if (position[1, 7] < position[2, 7]) {
    y %>% arrange(desc(Point_ID))
  } else {
    y
  }
}
