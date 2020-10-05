################################################################################
# Module Pre_Analysis
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-18
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
################################################################################

Pre_Analysis <- function(input, output, session) {
  # Sort single KMT --------------------------------------------------------------
  progressSweetAlert(
    session = session, 
    id = "SingleKMT",
    title = "Sorting data for single KMT",
    display_pct = TRUE, 
    value = 0
  )

  total <- as.numeric(ncol(Segments %>% select(starts_with("Pole"))))

  for (i in which(colnames(Segments) == "Pole1_00"):which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole")))[ncol(Segments %>% select(starts_with("Pole")))])) {
    assign(paste(colnames(Segments)[i]),
      Sort_by_fiber(paste(colnames(Segments)[i])),
      envir = .GlobalEnv
    )

    j <- 1

    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], j, sep = "_"),
        Select_Points(j, get(colnames(Segments)[i])),
        envir = .GlobalEnv
      )

      j <- j + 1
    }

    j <- 1
    while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
      assign(paste(colnames(Segments)[i], j, sep = "_"),
        Find_XYZ(get(paste(colnames(Segments)[i], j, sep = "_"))),
        envir = .GlobalEnv
      )

      j <- j + 1
    }

    updateProgressBar(
      session = session,
      id = "SingleKMT",
      value = i / total * 100
    )
    Sys.sleep(0.1)
  }
  closeSweetAlert(session = session)

  # Sort Points in KMT at a Pole1 ------------------------------------------------
  progressSweetAlert(
    session = session, 
    id = "Pre_Analysis",
    title = "Sorting points based on (+) and (-) ends for the Pole_1...",
    display_pct = TRUE, 
    value = 1
  )

  total <- 6

  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch(
      {
        j <- 1

        while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
          assign(paste(colnames(Segments)[i], j, sep = "_"),
            Sort_by_distance_to_pole1(get(paste(colnames(Segments)[i], j, sep = "_"))),
            envir = .GlobalEnv
          )

          j <- j + 1
        }
      },
      error = function(e) {}
    )
  }

  # Sort Points in KMT at a Pole2 ------------------------------------------------
  z <- 2

  updateProgressBar(
    session = session,
    id = "Pre_Analysis",
    title = "Sorting points based on (+) and (-) ends for the Pole_2...",
    value = round((z - 1) / total * 100, 0)
  )
  Sys.sleep(0.1)


  for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
    j <- 1

    tryCatch(
      {
        while (j <= as.numeric(nrow(get(colnames(Segments)[i])))) {
          assign(paste(colnames(Segments)[i], j, sep = "_"),
            Sort_by_distance_to_pole2(get(paste(colnames(Segments)[i], j, sep = "_"))),
            envir = .GlobalEnv
          )

          j <- j + 1
        }
      },
      error = function(e) {}
    )
  }

  # Get ID for the ellipse Rx and Rz for 100%, 50% and 25% -----------------------
  Plus_end <<- data.frame()
  Kinetochore_Avg <<- data.frame()

  assign("Kinetochore_projected",
    Kinetochore_position(),
    envir = .GlobalEnv
  )

  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(ncol(Segments) - 4)) {
    j <- 1

    tryCatch(
      {
        while (j <= as.numeric(nrow(get(paste(colnames(Segments)[i]))))) {
          Plus_end[j, 1:3] <<- get(paste(colnames(Segments)[i], j, sep = "_"))[1, 2:4]
          j <- j + 1
        }

        Plus_end <<- data.frame(
          X_Median = c(median(as.matrix(na.omit(Plus_end[1])))),
          Y_Median = c(median(as.matrix(na.omit(Plus_end[2])))),
          Z_Median = c(median(as.matrix(na.omit(Plus_end[3]))))
        )

        Kinetochore_Avg[i, 1:3] <<- Plus_end
        Plus_end <- data.frame()
      },
      error = function(e) {
        Kinetochore_Avg[i, 1:3] <<- NA
      }
    )
  }

  Kinetochore_Avg <<- na.omit(Kinetochore_Avg)

  Pole_avg <<- rbind(Pole1, Pole2)
  Pole_avg <<- data.frame(
    X_Mean = c(mean(as.matrix(Pole_avg[1]))),
    Y_Mean = c(mean(as.matrix(Pole_avg[2]))),
    Z_Mean = c(mean(as.matrix(Pole_avg[3])))
  )

  Rx100 <<- data.frame()
  Rx100[1, 1] <<- max(Kinetochore_Avg[1])
  Rx100[1, 1] <<- abs(Rx100[1, 1] - Pole_avg[1])
  Rx100[1, 2] <<- min(Kinetochore_Avg[1])
  Rx100[1, 2] <<- abs(Rx100[1, 2] - Pole_avg[1])

  Rx100 <<- max(Rx100)
  Rx50 <<- Rx100 * 0.75
  Rx25 <<- Rx100 * 0.50

  Rz100 <<- data.frame()
  Rz100[1, 1] <<- max(Kinetochore_Avg[3])
  Rz100[1, 1] <<- abs(Rz100[1, 1] - Pole_avg[3])
  Rz100[1, 2] <<- min(Kinetochore_Avg[3])
  Rz100[1, 2] <<- abs(Rz100[1, 2] - Pole_avg[3])

  Rz100 <<- max(Rz100)
  Rz50 <<- Rz100 * 0.75
  Rz25 <<- Rz100 * 0.50

  # Analyze Length Distribution for Pole1 --------------------------------------------------
  z <- 3

  updateProgressBar(
    session = session,
    id = "Pre_Analysis",
    title = "Calcualting legnth and ends positions for Pole_1...",
    value = round((z - 1) / total * 100, 0)
  )
  Sys.sleep(0.1)

  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch(
      {
        assign(paste(colnames(Segments)[i]),
          Analyse_LD(i, Pole1),
          envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
  }

  # Analyze Length Distribution for Pole2 --------------------------------------------------
  z <- 4

  updateProgressBar(
    session = session,
    id = "Pre_Analysis",
    title = "Calcualting legnth and ends positions for Pole_2...",
    value = round((z - 1) / total * 100, 0)
  )
  Sys.sleep(0.1)

  for (i in as.numeric(which(colnames(Segments) == "Pole2_00")):as.numeric(ncol(Segments) - 4)) {
    j <- 1

    tryCatch(
      {
        assign(paste(colnames(Segments)[i]),
          Analyse_LD(i, Pole2),
          envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
  }


  # Collect Length Distribution Data for Pole1 --------------------------------------------------
  assign("LD_P1",
    get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["length"],
    envir = .GlobalEnv
  )
  assign("Plus_end_pos",
    get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_kinetochore_core"],
    envir = .GlobalEnv
  )
  assign("Dist_pole",
    get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["plus_dist_to_pole"],
    envir = .GlobalEnv
  )
  assign("Elips",
    get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["Elipse_Position"],
    envir = .GlobalEnv
  )
  assign("Minus_dist",
    get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["minus_dist_to_pole"],
    envir = .GlobalEnv
  )
  assign("k_fiber",
    get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["Fiber_Name"],
    envir = .GlobalEnv
  )

  assign("LD_P1",
    cbind(LD_P1, Plus_end_pos, Dist_pole, Elips, Minus_dist, k_fiber),
    envir = .GlobalEnv
  )

  for (i in as.numeric(which(colnames(Segments) == "Pole1_00") + 1):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch(
      {
        assign("DF_LD",
          get(paste(colnames(Segments)[i]))["length"],
          envir = .GlobalEnv
        )
        assign("DF_Plus_end",
          get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"],
          envir = .GlobalEnv
        )
        assign("DF_Dist_pole",
          get(paste(colnames(Segments)[i]))["plus_dist_to_pole"],
          envir = .GlobalEnv
        )
        assign("DF_Elips",
          get(paste(colnames(Segments)[i]))["Elipse_Position"],
          envir = .GlobalEnv
        )
        assign("Minus_dist",
          get(paste(colnames(Segments)[i]))["minus_dist_to_pole"],
          envir = .GlobalEnv
        )
        assign("k_fiber",
          get(paste(colnames(Segments)[i]))["Fiber_Name"],
          envir = .GlobalEnv
        )

        assign("DF_LD_P1",
          cbind(DF_LD, DF_Plus_end, DF_Dist_pole, DF_Elips, Minus_dist, k_fiber),
          envir = .GlobalEnv
        )

        assign("LD_P1",
          rbind(LD_P1, DF_LD_P1),
          envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
  }

  # Collect Length Distribution Data for Pole2 --------------------------------------------------
  tryCatch(
    {
      assign("LD_P2",
        get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["length"],
        envir = .GlobalEnv
      )
      assign("Plus_end_pos",
        get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["plus_dist_to_kinetochore_core"],
        envir = .GlobalEnv
      )
      assign("Dist_pole",
        get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["plus_dist_to_pole"],
        envir = .GlobalEnv
      )
      assign("Elips",
        get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["Elipse_Position"],
        envir = .GlobalEnv
      )
      assign("Minus_dist",
        get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["minus_dist_to_pole"],
        envir = .GlobalEnv
      )
      assign("k_fiber",
        get(paste(colnames(Segments)[which(colnames(Segments) == "Pole2_00")]))["Fiber_Name"],
        envir = .GlobalEnv
      )

      assign("LD_P2",
        cbind(LD_P2, Plus_end_pos, Dist_pole, Elips, Minus_dist, k_fiber),
        envir = .GlobalEnv
      )
    },
    error = function(e) {}
  )

  for (i in as.numeric(which(colnames(Segments) == "Pole2_00") + 1):as.numeric(ncol(Segments) - 4)) {
    tryCatch(
      {
        assign("DF_LD",
          get(paste(colnames(Segments)[i]))["length"],
          envir = .GlobalEnv
        )
        assign("DF_Plus_end",
          get(paste(colnames(Segments)[i]))["plus_dist_to_kinetochore_core"],
          envir = .GlobalEnv
        )
        assign("DF_Dist_pole",
          get(paste(colnames(Segments)[i]))["plus_dist_to_pole"],
          envir = .GlobalEnv
        )
        assign("DF_Elips",
          get(paste(colnames(Segments)[i]))["Elipse_Position"],
          envir = .GlobalEnv
        )
        assign("Minus_dist",
          get(paste(colnames(Segments)[i]))["minus_dist_to_pole"],
          envir = .GlobalEnv
        )
        assign("k_fiber",
          get(paste(colnames(Segments)[i]))["Fiber_Name"],
          envir = .GlobalEnv
        )

        assign("DF_LD_P2",
          cbind(DF_LD, DF_Plus_end, DF_Dist_pole, DF_Elips, Minus_dist, k_fiber),
          envir = .GlobalEnv
        )

        assign("LD_P2",
          rbind(LD_P2, DF_LD_P2),
          envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
  }

  # Analyze Relative Position for Pole1 --------------------------------------------------------
  z <- 5
  updateProgressBar(
    session = session,
    id = "Pre_Analysis",
    title = "Calcualting relative position for Pole_1...",
    value = round((z - 1) / total * 100, 0)
  )
  Sys.sleep(0.1)

  for (i in which(colnames(Segments) == "Pole1_00"):as.numeric(which(colnames(Segments) == "Pole2_00") - 1)) {
    tryCatch(
      {
        Point_KMT <- data.frame()
        for (j in 1:nrow(get(colnames(Segments)[i]))) {
          Point_KMT[j, 1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1, 3]
        }
        Point_KMT <- which(Point_KMT[1] == min(Point_KMT))
        longest <- get(paste(colnames(Segments)[i], Point_KMT, sep = "_"))

        for (j in 1:nrow(get(paste(colnames(Segments)[i])))) {
          assign(paste(colnames(Segments)[i], j, sep = "_"),
            relativ_pos_1(longest, get(paste(colnames(Segments)[i], j, sep = "_"))),
            envir = .GlobalEnv
          )
        }
        
        Point_minus <- data.frame()
        for (j in 1:nrow(get(colnames(Segments)[i]))) {
          Point_minus[j, 1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[nrow(get(paste(colnames(Segments)[i], j, sep = "_"))), 5]
        }
        names(Point_minus)[1] <- "Relative_minus_position"

        Point_plus <- data.frame()
        for (j in 1:nrow(get(colnames(Segments)[i]))) {
          Point_plus[j, 1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1, 5]
        }
        names(Point_plus)[1] <- "Relative_plus_position"

        assign(paste(colnames(Segments)[i]),
          cbind(get(paste(colnames(Segments)[i])), Point_plus, Point_minus),
          envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
  }

  #  Analyze Relative Position for Pole2 --------------------------------------------------------
  z <- 6
  
  updateProgressBar(
    session = session,
    id = "Pre_Analysis",
    title = "Calcualting relative position for Pole_2...",
    value = round((z - 1) / total * 100,0)
  )
  Sys.sleep(0.1)
  
  for (i in which(colnames(Segments) == "Pole2_00"):as.numeric(ncol(Segments) - 4)) {
    tryCatch(
      {
        Point_KMT <- data.frame()
        for (j in 1:nrow(get(colnames(Segments)[i]))) {
          Point_KMT[j, 1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1, 3]
        }
        
        Point_KMT <- which(Point_KMT[1] == max(Point_KMT))
        longest <- get(paste(colnames(Segments)[i], Point_KMT, sep = "_"))

        for (j in 1:nrow(get(paste(colnames(Segments)[i])))) {
          assign(paste(colnames(Segments)[i], j, sep = "_"),
          relativ_pos_2(longest, get(paste(colnames(Segments)[i], j, sep = "_"))),
          envir = .GlobalEnv
          )
        }
        
        Point_minus <- data.frame()
        for (j in 1:nrow(get(colnames(Segments)[i]))) {
          Point_minus[j, 1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[nrow(get(paste(colnames(Segments)[i], j, sep = "_"))), 5]
        }
        names(Point_minus)[1] <- "Relative_minus_position"

        Point_plus <- data.frame()
        for (j in 1:nrow(get(colnames(Segments)[i]))) {
          Point_plus[j, 1] <- get(paste(colnames(Segments)[i], j, sep = "_"))[1, 5]
        }
        names(Point_plus)[1] <- "Relative_plus_position"

        assign(paste(colnames(Segments)[i]),
          cbind(get(paste(colnames(Segments)[i])), Point_plus, Point_minus),
          envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
  }
  closeSweetAlert(session = session)

  Minus_end_position <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["minus_dist_to_pole"]
  Relative_position <<- get(paste(colnames(Segments)[which(colnames(Segments) == "Pole1_00")]))["Relative_minus_position"]
  Minus_end_position <<- cbind(Minus_end_position, Relative_position)

  for (i in as.numeric(which(colnames(Segments) == "Pole1_00") + 1):as.numeric(ncol(Segments) - 4)) {
    tryCatch(
      {
        DF_Minus_end_position <<- get(paste(colnames(Segments)[i]))["minus_dist_to_pole"]
        DF_Relative_position <<- get(paste(colnames(Segments)[i]))["Relative_minus_position"]
        DF <<- cbind(DF_Minus_end_position, DF_Relative_position)
        Minus_end_position <<- rbind(Minus_end_position, DF)
      },
      error = function(e) {}
    )
  }
}
