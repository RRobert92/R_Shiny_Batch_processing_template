################################################################################
# Packages MT_Bridging
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz / Gunar Fabig
# Created: 2020-09-01
# Reviewed:
################################################################################

# Set-up analysis --------------------------------------------------------------

A_MT_Bridging <- function(input, output, session) {
  progressSweetAlert(
    session = session,
    id = "P_MT_Bridginig",
    title = "Calculating microtubule interaction...",
    display_pct = TRUE,
    value = round(0 / 4 * 100, 0)
  )

  # Calculate all interaction --------------------------------------------------
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  MT_Interaction <<- foreach(i = 1:nrow(Points), .combine = rbind, .inorder = FALSE, .export = ls(.GlobalEnv)) %dopar% {
    Point_interaction(i)
  }
  stopCluster(cl)

  updateProgressBar(
    session = session,
    id = "P_MT_Bridginig",
    title = "Pre-sorting data of MT interaction...",
    value = round(1 / 4 * 100, 0)
  )
  Sys.sleep(0.1)

  assign("MT_Interaction",
    Segment_to_point(1),
    envir = .GlobalEnv
  )
  names(MT_Interaction)[4] <<- "Segments_ID_1"

  updateProgressBar(
    session = session,
    id = "P_MT_Bridginig",
    value = round(2 / 4 * 100, 0)
  )
  Sys.sleep(0.1)

  assign("MT_Interaction",
    Segment_to_point(2),
    envir = .GlobalEnv
  )
  names(MT_Interaction)[5] <<- "Segments_ID_2"

  updateProgressBar(
    session = session,
    title = "Searching for unique interacting points...",
    id = "P_MT_Bridginig",
    value = round(3 / 4 * 100, 0)
  )
  Sys.sleep(0.1)

  assign("MT_Interaction",
    Remove_interaction_duplicates(),
    envir = .GlobalEnv
  )

  updateProgressBar(
    session = session,
    title = "Searching for unique interacting points...",
    id = "P_MT_Bridginig",
    value = round(4 / 4 * 100, 0)
  )
  Sys.sleep(0.1)

  assign("MT_Interaction",
    Unique_interaction(),
    envir = .GlobalEnv
  )

  closeSweetAlert(session = session)
}
