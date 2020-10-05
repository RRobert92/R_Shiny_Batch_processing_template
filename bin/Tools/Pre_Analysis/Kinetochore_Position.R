#####################################################################################################
# Function Kinetochore_Position
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

# Calculate median kinetochore position  ---------------------------------------
Kinetochore_position <- function() {
  Plus_end <- data.frame()
  Kinetochore_Avg <- data.frame()

  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
    j <- 1

    tryCatch(
      {
        while (j <= as.numeric(nrow(get(paste(colnames(Segments)[i]))))) {
          Plus_end[j, 1:3] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1, 2:4]
          j <- j + 1
        }

        Plus_end <- data.frame(
          X_Median = c(median(as.matrix(Plus_end[1]))),
          Y_Median = c(median(as.matrix(Plus_end[2]))),
          Z_Median = c(median(as.matrix(Plus_end[3])))
        )
        Kinetochore_Avg[i, 1:3] <- Plus_end
      },
      error = function(e) {
        Kinetochore_Avg[i, 1:3] <- NA
      }
    )
  }

  Kinetochore_Avg <- na.omit(Kinetochore_Avg)
  Kinetochore_Avg <- data.frame(
    X_Mean = c(mean(as.matrix(Kinetochore_Avg[1]))),
    Y_Mean = c(mean(as.matrix(Kinetochore_Avg[2]))),
    Z_Mean = c(mean(as.matrix(Kinetochore_Avg[3])))
  )

  Pole_avg <- rbind(Pole1, Pole2)
  Pole_avg <- data.frame(
    X_Mean = c(mean(as.matrix(Pole_avg[1]))),
    Y_Mean = c(mean(as.matrix(Pole_avg[2]))),
    Z_Mean = c(mean(as.matrix(Pole_avg[3])))
  )

  Kinetochore_projected <- data.frame(
    X_Mean = c(mean(as.matrix(Pole_avg[1]))),
    Y_Mean = c(mean(as.matrix(Kinetochore_Avg[2]))),
    Z_Mean = c(mean(as.matrix(Pole_avg[3])))
  )

  Kinetochore_projected
}
