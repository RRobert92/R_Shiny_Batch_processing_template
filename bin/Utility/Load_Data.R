################################################################################
# Module Load_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-22
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

Load_Data <- function(input, output, session) {
  # Set-up ----------------------------------------------------------------------
  if (Test == FALSE) {
    Segments <<- get(paste("Data", "Segments", current_data, sep = "_"))
    Points <<- get(paste("Data", "Points", current_data, sep = "_"))
    Nodes <<- get(paste("Data", "Nodes", current_data, sep = "_"))
  }

  Pole1 <<- "Pole1"
  Pole2 <<- "Pole2"

  # Load Segments ----------------------------------------------------------------
  NColumn <<- ncol(Segments)
  Segments_1_KMT <<- Segments %>% filter_at(vars(starts_with(Pole1)), any_vars(. >= 1))
  Segments_1_KMT <<- Segments_1_KMT %>% select(
    "Segment ID",
    "length",
    "Node ID #1",
    "Node ID #2",
    "Point IDs"
  )

  Segments_2_KMT <<- Segments %>% filter_at(vars(starts_with(Pole2)), any_vars(. >= 1))
  Segments_2_KMT <<- Segments_2_KMT %>% select(
    "Segment ID",
    "length",
    "Node ID #1",
    "Node ID #2",
    "Point IDs"
  )

  Segments_KMT <<- Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
  Segments_KMT <<- Segments_KMT %>% select(
    "Segment ID",
    "length",
    "Node ID #1",
    "Node ID #2",
    "Point IDs"
  )

  Segments_SMT <<- Segments %>% filter_at(vars(starts_with("Pole")), all_vars(. < 1))
  Segments_SMT <<- Segments_SMT %>% select(
    "Segment ID",
    "length",
    "Node ID #1",
    "Node ID #2",
    "Point IDs"
  )

  # Load Poles ------------------------------------------------------------------
  Pole1 <<- Nodes %>% filter_at(vars(Pole1), any_vars(. >= 1))
  Pole1 <<- data.frame(
    X = c(Pole1 %>% select("X Coord") / 10000),
    Y = c(Pole1 %>% select("Y Coord") / 10000),
    Z = c(Pole1 %>% select("Z Coord") / 10000)
  )

  Pole2 <<- Nodes %>% filter_at(vars(Pole2), any_vars(. >= 1))
  Pole2 <<- data.frame(
    X = c(Pole2 %>% select("X Coord") / 10000),
    Y = c(Pole2 %>% select("Y Coord") / 10000),
    Z = c(Pole2 %>% select("Z Coord") / 10000)
  )

  # Load Nodes ------------------------------------------------------------------
  if (ncol(Nodes %>% select(starts_with("EndType"))) == 1) {
    Nodes <<- Nodes %>% select(
      "Node ID",
      "X Coord",
      "Y Coord",
      "Z Coord",
      starts_with("EndType")
    )
  } else if (ncol(Nodes %>% select(starts_with("EndType"))) == 2) {
    Nodes <<- Nodes %>% select(
      "Node ID",
      "X Coord",
      "Y Coord",
      "Z Coord",
      starts_with("EndType")
    )

    Compare_module <<- tibble()

    for (i in 1:nrow(Nodes %>% select(starts_with("EndType")))) {
      Compare_module[i, 1] <- Nodes[i, 5] == Nodes[i, 6]
    }

    Nodes <<- cbind(Nodes, Compare_module[1])

    names(Nodes)[7] <<- "Entype_Different"
    rm(Compare_module)
  } else {
    Nodes <<- Nodes %>% select(
      "Node ID",
      "X Coord",
      "Y Coord",
      "Z Coord"
    )
  }

  Nodes[2:4] <<- Nodes[2:4] / 10000

  # Load Points -----------------------------------------------------------------
  Points <<- Points %>% select(
    "Point ID",
    "X Coord",
    "Y Coord",
    "Z Coord"
  )

  Points[2:4] <<- Points[2:4] / 10000
  names(Points)[1] <<- "Point_ID"
}
