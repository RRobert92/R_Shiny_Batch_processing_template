################################################################################################
# Tool Fiber_Area
#
# Set of functions to define fiber area and neighborhood density
#
# Calculate ratio of KMT_length / (-) end distance to the pole. Lower ratio, define leading KMTs
# x is an number of the column with the label name
# Y is name of the pole Pole1 or Pole2
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-17
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
################################################################################################

# Function: select leading KMT ------------------------------------------------------------------
leading_KMTsv2 <- function(x, y) {
  j <- 1
  leading <- data.frame(Leading = as.numeric())

  while (j <= as.numeric(nrow(get(colnames(Segments)[x])))) {
    KMT_lenght <- Segments[as.numeric(get(colnames(Segments)[x])[j, 1] + 1), as.numeric(ncol(Segments) - 3)] / 10000
    
    m_end_to_pole <- data.frame(
      x = c(y[1, 1]),
      y = c(y[1, 2]),
      z = c(y[1, 3]),
      x1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 2]),
      y1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 3]),
      z1 = c(get(paste(colnames(Segments)[x], j, sep = "_"))[as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))), 4])
    )

    m_end_to_pole$distance <- apply(
      m_end_to_pole, 
      1, 
      function(z) {dist(matrix(z,nrow = 2, byrow = TRUE))}
      )
    
    leading[j, ] <- KMT_lenght[1, 1] / m_end_to_pole$distance[1]
    j <- j + 1
  }
  
  bind_cols(
    get(paste(colnames(Segments)[x])), 
    leading)
}

# Function: Find point for lading KMTs, for j = j+5 == 0.5um ------------------------------------
Leadig_Points <- function(x) {
  j <- 5
  
  leading_points <- data.frame(Leading_ID = as.numeric())

  while (j <= nrow(get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_")))) {
    leading_points[j, ] <- get(paste(colnames(Segments)[x], which.max(as.matrix(get(colnames(Segments)[x])["Leading"])), sep = "_"))[j, 1]

    j <- j + Fiber_area_config
  }
  leading_points <- na.omit(leading_points)
}

# Function: Find all points which correspond to the slice of the fiber ---------------------------
find_polygon <- function(x) {
  i <- 1
  lead_points_id <- data.frame(Distance = as.numeric())
  Distance <- data.frame(V1 = as.numeric())

  while (i <= as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    lead_points_id <- data.frame(
      X_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 2],
      Y_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 3],
      Z_lead = Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_")) [i, ] + 1), 4]
    )

    j <- 1
    lead_points_id_full <- data.frame(t = as.numeric())

    for (j in 1:as.numeric(nrow(get(paste(colnames(Segments)[x]))))) {
      lead_points_id_full <- apply(
        lead_points_id,
        MARGIN = 2,
        function(y) {rep(y, as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))))}
      )

      lead_points_id_full <- cbind(
        get(paste(colnames(Segments)[x], j, sep = "_"))[1],
        lead_points_id,
        get(paste(colnames(Segments)[x], j, sep = "_"))[2:4]
      )

      lead_points_id_full$distance <- apply(
        lead_points_id_full[2:7],
        1,
        function(x) {dist(matrix(x, nrow = 2, byrow = TRUE))}
      )

      Distance[i, j] <- lead_points_id_full[as.numeric(which.min(lead_points_id_full$distance)), 1]
      lead_points_id_full <- data.frame(t = as.numeric())
    }
    
    i <- i + 1
  }
  bind_cols(
    get(paste(colnames(Segments)[x], "fiber", sep = "_")),
    Distance
  )
}
# Function: Remove duplicated points -------------------------------------------------------------
duplicated_points <- function(x) {
  DF <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))

  for (i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))) {
    DF[, i][duplicated(DF[, i])] <- NA
  }
  
  # Check if there is no hole in data-set if yes remove
  for (i in 1:ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_")))) {
    for (j in 2:nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_")))) {
      if (is.na(DF[j - 1, i]) && is.na(DF[j + 2, i])) {
        DF[j:nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))), i] <- NA
      }
      else {}
    }
  }
  
  DF
}

# Function: Get a median point for each position and put cbind in first col ---------------------
median_point <- function(x) {

  # for loop to create df of x y z coord for each position
  # median of x y z coord
  # writ it in a table
  # cbind with Pole1_00_fiber

  Median_id <- data.frame(
    X_Coord = as.numeric(),
    Y_Coord = as.numeric(),
    Z_Coord = as.numeric()
  )

  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    DF <- data.frame(
      X_Coord = as.numeric(),
      Y_Coord = as.numeric(),
      Z_Coord = as.numeric()
    )

    for (j in 1:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j, 1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, j] + 1), 2]
      DF[j, 2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, j] + 1), 3]
      DF[j, 3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, j] + 1), 4]
    }

    Median_id[i, 1] <- median(na.omit(DF$X_Coord))
    Median_id[i, 2] <- median(na.omit(DF$Y_Coord))
    Median_id[i, 3] <- median(na.omit(DF$Z_Coord))
  }

  DF <- cbind(Median_id, get(paste(colnames(Segments)[x], "fiber", sep = "_")))
  
  for (i in 1:nrow(DF)) {
    if (sum(na.omit(colSums(DF[i, which(colnames(DF) == "V1"):ncol(DF)] != 0))) < 3) {
      DF[i, 1:12] <- NA
    } else {}
  }
  
  if (sum(which(is.na(DF[1]))) != 0) {
    DF[-c(which(is.na(DF[1]))), ]
  } else {
    DF
  }
}

# Function: Find polygon for selected areas ---------------------------------------------------------
find_polygon_for_all <- function(x) {
  i <- 1
  
  lead_points_id <- data.frame(Distance = as.numeric())
  Distance <- data.frame(V1 = as.numeric())

  while (i <= as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    lead_points_id <- data.frame(
      X_lead = get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 1],
      Y_lead = get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 2],
      Z_lead = get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 3]
    )
    j <- 1
    lead_points_id_full <- data.frame(t = as.numeric())

    for (j in 1:as.numeric(nrow(get(paste(colnames(Segments)[x]))))) {
      lead_points_id_full <- apply(
        lead_points_id,
        MARGIN = 2,
        function(y) {rep( y, as.numeric(nrow(get(paste(colnames(Segments)[x], j, sep = "_")))))}
      )

      lead_points_id_full <- cbind(
        get(paste(colnames(Segments)[x], j, sep = "_"))[1],
        lead_points_id,
        get(paste(colnames(Segments)[x], j, sep = "_"))[2:4]
      )

      lead_points_id_full$distance <- apply(
        lead_points_id_full[2:7],
        1,
        function(x) {dist(matrix(x, nrow = 2, byrow = TRUE))}
      )

      Distance[i, j] <- lead_points_id_full[as.numeric(which.min(lead_points_id_full$distance)), 1]
      lead_points_id_full <- data.frame(t = as.numeric())
    }
    
    i <- i + 1
  }
  
  bind_cols(Distance)
}

# Function: Calculate a polygon for selected areas -------------------------------------------------
polygon_area <- function(x) {
  area <- data.frame(
    Alpha_area = as.numeric(),
    Roundness = as.numeric()
  )
  MT_no <- data.frame(KMT_no = as.numeric())
  i <- 1
  for (i in 1:as.numeric(nrow(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
    MT_no[i, 1] <- ncol(select(
      get(paste(colnames(Segments)[x], "fiber", sep = "_")),
      starts_with("V")
    )) - sum(is.na(select(
      get(paste(colnames(Segments)[x], "fiber", sep = "_")),
      starts_with("V")
    )[i, ]))
    
    DF <- data.frame(
      X_Coord = as.numeric(),
      Y_Coord = as.numeric(),
      Z_Coord = as.numeric()
    )
    
    j <- 4
    
    for (j in 4:as.numeric(ncol(get(paste(colnames(Segments)[x], "fiber", sep = "_"))))) {
      DF[j, 1] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, j] + 1), 2]
      DF[j, 2] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, j] + 1), 3]
      DF[j, 3] <- Points[as.numeric(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, j] + 1), 4]
    }

    DF1 <- as.matrix(na.omit(DF))
    DF2 <- as.matrix(DF1 + 1)
    DF3 <- as.matrix(merge(DF1, DF2, all = TRUE))
    
    DF4 <- cbind(DF1, get(paste(colnames(Segments)[x], "fiber", sep = "_"))[i, 1:3], row.names = NULL)
    DF4$distance <- apply(
      DF4[1:6],
      1,
      function(x) {dist(matrix(x, nrow = 2, byrow = TRUE))}
    )
    
    if (as.numeric(nrow(DF1)) < 3) {
      area[i, 1] <- 0
      area[i, 2] <- 0
    } else {
      alphashape.obj <- ashape3d(
        DF3,
        pert = TRUE,
        alpha = 10
      )
      
      area[i, 1] <- round(volume_ashape3d(alphashape.obj) / 1, 3)
      area[i, 2] <- area[i, 1] / (pi * max(DF4$distance)^2)
    }
  }
  cbind(
    area,
    MT_no,
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))
  )
}

# Function: Find neighborhood density for selected areas ------------------------------------------
Neighorhood_densit <- function(x) {
  DF <- select(
    get(paste(colnames(Segments)[x], "fiber", sep = "_")),
    starts_with("V")
  )

  Mean_DF <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[4:6]

  dist <- data.frame()
  for (j in 1:ncol(DF)) {
    dist[j, 1] <- Points[as.numeric(DF[1, j] + 1), "X Coord"]
    dist[j, 2] <- Points[as.numeric(DF[1, j] + 1), "Y Coord"]
    dist[j, 3] <- Points[as.numeric(DF[1, j] + 1), "Z Coord"]
  }
  dist <- na.omit(dist)
  dist[4:6] <- Mean_DF[1, 1:3]
  
  dist$distance <- apply(
    dist,
    1,
    function(y) {dist(matrix(y, nrow = 2, byrow = TRUE))}
  )
  
  fiber_radius <- round(as.numeric(max(dist$distance) * 2), 2)

  DF_full <- data.frame()

  for (i in 1:nrow(DF)) {
    dist <- data.frame()

    for (j in 1:ncol(DF)) {
      dist[j, 1] <- Points[as.numeric(DF[i, j] + 1), "X Coord"]
      dist[j, 2] <- Points[as.numeric(DF[i, j] + 1), "Y Coord"]
      dist[j, 3] <- Points[as.numeric(DF[i, j] + 1), "Z Coord"]
    }
    
    dist <- na.omit(dist)
    dist[4:6] <- Mean_DF[i, 1:3]

    dist$distance <- apply(
      dist,
      1,
      function(y) {dist(matrix(y, nrow = 2, byrow = TRUE))}
    )

    DF_full[i, 1] <- round(mean(dist$distance), 3)
    DF_full[i, 2] <- round(sd(dist$distance), 3)
    DF_full[i, 3] <- round((length(which(dist$distance <= fiber_radius)) * 100) / nrow(dist), 0)
    DF_full[i, 4] <- get(paste(colnames(Segments)[x]))[1, 5]
    DF_full[i, 5] <- get(paste(colnames(Segments)[x]))[1, 6]
  }

  names(DF_full)[1] <- "Mean"
  names(DF_full)[2] <- "SD"
  names(DF_full)[3] <- "Focused KMTs %"
  names(DF_full)[4] <- "plus_dist_to_pole"
  names(DF_full)[5] <- "Elipse_Position"
  
  DF_full
}

# Function: Relative position of points between kinetochore and the pole1 ----------------------
relativ_pos_1_fiber <- function(x) {
  relativ_pos_part1 <- lapply(
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[5],
    function(y) {get(paste(colnames(Segments)[x], "fiber", sep = "_"))[5] - Pole1[1, 2]}
  )
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])

  relativ_pos_part2 <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[which.min(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1, 5]), 5] - Pole1[1, 2]

  relativ_positon <- lapply(
    relativ_pos_part1,
    function(y) {round(relativ_pos_part1[1] / relativ_pos_part2, 2)}
  )

  relat_pos <- data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])

  names(relat_pos)[1] <- "Relative_position"

  cbind(
    relat_pos,
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1],
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[2],
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[3]
  )
}

# Function: Relative position of points between kinetochore and the pole2 ----------------------
relativ_pos_2_fiber <- function(x) {
  relativ_pos_part1 <- lapply(
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[5],
    function(y) {get(paste(colnames(Segments)[x], "fiber", sep = "_"))[5] - Pole2[1, 2]}
  )
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y_Coord"]][["Y_Coord"]])

  relativ_pos_part2 <- get(paste(colnames(Segments)[x], "fiber", sep = "_"))[which.max(get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1, 5]), 5] - Pole2[1, 2]

  relativ_positon <- lapply(
    relativ_pos_part1,
    function(y) {round(relativ_pos_part1[1] / relativ_pos_part2, 2)}
  )

  relat_pos <- data.frame(Relative_Position = relativ_positon[["relativ_pos_part1...Y_Coord......Y_Coord..."]])

  names(relat_pos)[1] <- "Relative_position"
  cbind(
    relat_pos,
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[1],
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[2],
    get(paste(colnames(Segments)[x], "fiber", sep = "_"))[3]
  )
}
