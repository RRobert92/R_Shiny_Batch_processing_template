################################################################################
# Module Save_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
################################################################################

Save_Data <- function(input, output, session) {

  # Save Data for IKD -----------------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "IKD", sep = "_"),
        Inter_Kinetochore_Distance,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "IKD_KMT_Delta", sep = "_"),
        Inter_Kinetochore_Distance_KMTs_delta,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "IKD_KMT_No", sep = "_"),
        Inter_Kinetochore_Distance_KMTs_no,
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "IKD", sep = "_")),
        paste("Data/", "Data_", current_data, "_IKD.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "IKD_KMT_Delta", sep = "_")),
        paste("Data/", "Data_", current_data, "_IKD_KMT_Delta.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "IKD_KMT_No", sep = "_")),
        paste("Data/", "Data_", current_data, "_IKD_KMT_No.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Save Data for Fiber area ----------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "Fiber_Area_P1", sep = "_"),
        Fiber_area_P1,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Area_P2", sep = "_"),
        Fiber_area_P2,
        envir = .GlobalEnv
      )
      assign(paste("Data", current_data, "Fiber_Area", sep = "_"),
        rbind(Fiber_area_P1, Fiber_area_P2),
        envir = .GlobalEnv
      )

      write.xlsx(
        get(paste("Data", current_data, "Fiber_Area_P1", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Area_P1.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Area_P2", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Area_P2.xlsx", sep = "")
      )
      write.xlsx(
        get(paste("Data", current_data, "Fiber_Area", sep = "_")),
        paste("Data/", "Data_", current_data, "_Fiber_Area.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

  # Clean Environment -----------------------------------------------------------
  rm(list = ls(pattern = "Pole"))
  rm(list = ls(pattern = "DF"))
}
