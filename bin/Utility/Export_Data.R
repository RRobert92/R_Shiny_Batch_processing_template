################################################################################
# Module Export_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-10-05
################################################################################

ExportData <- function(input, output, session) {
  # Set Data dir -----------------------------------------------------------------
  setwd("Data/")

  # Zip all file and cleaned dir --------------------------------------------------
  ZipFiles <- list.files(path = getwd(), pattern = ".xlsx$")
  zip::zipr(zipfile = "Shiny_batch_Data.zip", files = Zip_Files)

  file.remove(ZipFiles)
  # Back to main dir -------------------------------------------------------------
  setwd("../")
}
