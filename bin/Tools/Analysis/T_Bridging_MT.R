########################################################################################################
# Tool MT_bridging
#
# Function to calculate interaction length for all MT
# The tool is calculating interaction for all points and searching for continues interaction on MT of
# 500 nm (modifiable). Sorted interactions are then fed to the network analysis to determine no. of
# first-degree interaction, and no. of bundled MT.
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz / Gunar Fabig
# Created: 2020-09-01
# Reviewed:
########################################################################################################

# Function:  -------------------------------------------------------------------------------------------
Point_interaction <- function(x) {

  # Calculate distant of the point to other point in the boundary box ~15 min --------------------------

  p_to_P <- Points[with(Points, `X Coord` <= as.numeric(Points[x, 2] + (MT_point_config)) &
    `X Coord` >= as.numeric(Points[x, 2] - (MT_point_config))), ]

  p_to_P <- p_to_P[with(p_to_P, `Y Coord` <= as.numeric(Points[x, 3] + (MT_point_config * 2)) &
    `Y Coord` >= as.numeric(Points[x, 3] - (MT_point_config))), ]

  p_to_P <- p_to_P[with(p_to_P, `Z Coord` <= as.numeric(Points[x, 4] + (MT_point_config * 2)) &
    `Z Coord` >= as.numeric(Points[x, 4] - (MT_point_config))), ]

  p_to_P[5:7] <- Points[x, 2:4]

  p_to_P$dist <- apply(
    p_to_P[2:7],
    1,
    function(y) {
      dist(matrix(y, nrow = 2, byrow = TRUE))
    }
  )

  DF_1 <- data.frame(
    p_to_P[with(p_to_P, dist <= MT_point_config & dist > 0), "Point_ID"],
    p_to_P[with(p_to_P, dist <= MT_point_config & dist > 0), "dist"]
  )
  names(DF_1)[1] <- "Point_ID_2"

  DF_1 <- data.frame(
    Points[x, 1],
    DF_1
  )
  names(DF_1)[1] <- "Point_ID_1"
  names(DF_1)[3] <- "dist"

  DF_1 <- cbind(DF_1, c(DF_1[1] - DF_1[2]))
  names(DF_1)[4] <- "V1"

  DF_1[with(DF_1, `V1` < -1 | `V1` > 1), 1:3]
}

Segment_to_point <- function(x) {
  # Assign segments to the points ----------------------------------------------------------------------
  Length_estiamtion <- tibble(round(Segments$length / 190, 0))
  names(Length_estiamtion)[1] <- "no"

  Length_estiamtion_df <- tibble()
  for (i in 1:nrow(Length_estiamtion)) {
    Length_estiamtion_df[i, 1] <- sum(Length_estiamtion$no[1:i]) - 100
  }

  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  Segment_ID <- foreach(i = 1:nrow(MT_Interaction), .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    j <- which.min(as.matrix(abs(Length_estiamtion_df[1] - MT_Interaction[i, x]))) - 5

    if (j < 1) {
      j <- 1
    }

    test_condition <- TRUE
    while (test_condition == TRUE) {
      if (0 < gregexpr(paste(",", as.character(MT_Interaction[i, x]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] ||
        0 < gregexpr(paste(as.character(MT_Interaction[i, x]), ",", sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1] ||
        0 < gregexpr(paste(",", as.character(MT_Interaction[i, x]), sep = ""), Segments[j, "Point IDs"], fixed = T)[[1]][1]) {
        test_condition <- FALSE
      } else {
        j <- j + 1
      }
    }

    Segment_ID_DF <- j - 1
    Segment_ID_DF
  }
  stopCluster(cl)

  Segment_ID <- tibble(Segments_ID = Segment_ID)
  DF <- cbind(MT_Interaction, Segment_ID)

  if (x == 2) {
    Compare_ID <- tibble(Test = apply(
      DF[4:5],
      1,
      function(y) {
        y[1] == y[2]
      }
    ))

    DF <- cbind(DF, Compare_ID)
    DF <- DF[with(DF, `Test` == FALSE), 1:5]
  }

  DF
}

Remove_interaction_duplicates <- function() {
  # select for each segments pair unique points and remove duplicates based on shorter distance --------
  Unique_value <- unique(MT_Interaction[, 4:5])

  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  Unique_value <- foreach(i = 1:nrow(Unique_value), .combine = rbind, .export = c("MT_Interaction", "Unique_value"), .packages = "tidyr") %dopar% {
    Unique_value_df <- MT_Interaction[with(MT_Interaction, `Segments_ID_1` == as.numeric(Unique_value[i, 1]) &
      `Segments_ID_2` == as.numeric(Unique_value[i, 2])), ]
    Point_ID_2_df <- tibble(unique(Unique_value_df$Point_ID_2))

    Unique_value_df <- apply(
      Point_ID_2_df,
      1,
      function(y) {
        Unique_value_df[with(Unique_value_df, `Point_ID_2` == y), ][which.min(Unique_value_df[with(Unique_value_df, `Point_ID_2` == y), ]$dist), 1:5]
      }
    )

    data.table::rbindlist(Unique_value_df)
  }
  stopCluster(cl)

  Unique_value
}

Unique_interaction <- function() {
  # select for each segments pair unique points and remove duplicates based on shorter distance --------
  Unique_value <- unique(MT_Interaction[, 4])

  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  Unique_value <- foreach(i = 1:nrow(Unique_value), .combine = rbind, .export = c("MT_Interaction", "Unique_value"), .packages = c("tidyr", "dplyr")) %dopar% {
    Unique_value_df <- MT_Interaction[with(MT_Interaction, `Segments_ID_1` == as.numeric(Unique_value[i, 1])), ]
    Unique_segment <- tibble(unique(Unique_value_df$Segments_ID_2))

    Unique_value_df <- apply(
      Unique_segment,
      1,
      function(y) {
        x <- nrow(Unique_value_df[with(Unique_value_df, `Segments_ID_2` == y), 1])
        list(
          as.numeric(Unique_value_df[1, 4]),
          as.numeric(y),
          as.numeric(nrow(Unique_value_df[with(Unique_value_df, `Segments_ID_2` == y), ]) * 20), # Length of interaction [um]
          as.numeric(Unique_value_df[with(Unique_value_df, `Segments_ID_2` == y), 1][1]),
          last(as.matrix(Unique_value_df[with(Unique_value_df, `Segments_ID_2` == y), 1])),
          as.numeric(Unique_value_df[with(Unique_value_df, `Segments_ID_2` == y), 2][1]),
          last(as.matrix(Unique_value_df[with(Unique_value_df, `Segments_ID_2` == y), 2]))
        )
      }
    )

    Unique_value_df <- data.table::rbindlist(Unique_value_df)
    names(Unique_value_df)[1] <- "Segments_ID_1"
    names(Unique_value_df)[2] <- "Segments_ID_2"
    names(Unique_value_df)[3] <- "Length"
    names(Unique_value_df)[4] <- "S_1_Start"
    names(Unique_value_df)[5] <- "S_1_Stop"
    names(Unique_value_df)[6] <- "S_2_Start"
    names(Unique_value_df)[7] <- "S_2_Stop"

    Unique_value_df
  }
  stopCluster(cl)

  Unique_value
}
