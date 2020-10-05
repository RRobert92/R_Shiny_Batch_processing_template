################################################################################
# Module Report
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (ASGA v0.31.1)
################################################################################

# Style of the setting for the plot --------------------------------------------
Report_Plot_Settings <- function(input, output, session) {
  tags$div(class = "splash-report-code",
    fluidRow(
      column(
        4,
        lapply(1:numfiles, function(i) {
          textInput(
            inputId = paste("Data_label", i, sep = "_"),
            label = paste("Data", i, sep = "_"),
            value = paste("Data", i, sep = "_")
          )
        })
      ),

      column(
        4,
        lapply(1:numfiles, function(i) {
          colourInput(
            inputId = paste("Data_color", i, sep = "_"),
            label = paste("Select Colour for Data_", i, sep = ""),
            value = paste("gray")
          )
        })
      )
    ),

    fluidRow(column(
      12,
      tags$div(class = "table-GS-Center",
        actionBttn(
          inputId = "Refresh",
          label = "Refresh",
          style = "material-flat",
          color = "primary",
          size = "lg"
        )
      )
    ))
  )
}

# Render of a plots ------------------------------------------------------------
Report_Plot_KMT_No <- function(input, output, session) {
  plotOutput("Home-plot_KMT_No")
}

Report_Plot_IKD <- function(input, output, session) {
  plotOutput("Home-plot_IKD")
}

# Set uo of a plot to render ---------------------------------------------------
Report_Plot <- function(input, output, session) {

  # Plot for KMT no per kinetochore --------------------------------------------
  output$`plot_KMT_No` <- renderPlot({
    tryCatch(
      {
        Plot_Data <<- File_name[File_name$V2 == "KMT_No", ]

        for (i in 1:nrow(Plot_Data)) {

            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], sep = ""))["KMTs_per_kinetochore"]
              ),
              envir = .GlobalEnv
            )

        }

        P1 <<- ggplot(
          get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = "")),
          aes_string("Data", "KMTs_per_kinetochore")
        ) +
          geom_boxplot(fill = get(paste("Data_", "color_", 1, sep = "")), color = "black") +
          theme_classic() +
          xlab("Data-set names") +
          ylab("Number of KMTs per kinetochore")

        tryCatch(
          {
            for (i in 2:nrow(Plot_Data)) {
              P1 <<- P1 + geom_boxplot(
                data = get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")), aes_string("Data", "KMTs_per_kinetochore"),
                fill = get(paste("Data_", "color_", i, sep = "")), color = "black"
              )
            }

            All_KMT_No <<- get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = ""))

            for (i in 2:nrow(Plot_Data)) {
              assign("All_KMT_No",
                rbind(
                  All_KMT_No["KMTs_per_kinetochore"],
                  get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""))["KMTs_per_kinetochore"]
                ),
                envir = .GlobalEnv
              )
            }

            All_KMT_No <<- data.frame(
              Data = "Average",
              All_KMT_No["KMTs_per_kinetochore"]
            )

            P1 <<- P1 + geom_boxplot(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), fill = "darkred", color = "black", outlier.alpha = 0) +
              geom_jitter(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)
          },
          error = function(e) {}
        )

        print(P1)
      },
      error = function(e) {}
    )
  })

  # Plot for the inter-kinetochore distance ------------------------------------
  output$`plot_IKD` <- renderPlot({
    tryCatch(
      {
        Plot_Data <<- File_name[File_name$V2 == "IKD", ]

        for (i in 1:nrow(Plot_Data)) {
          assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
            data.frame(
              Data = get(paste("Data_", "label_", i, sep = "")),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], sep = ""))["Inter-kinetochore distance"]
            ),
            envir = .GlobalEnv
          )
        }

        P3 <<- ggplot(
          get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = "")),
          aes_string("Data", "Inter.kinetochore.distance")
        ) +
          geom_boxplot(fill = get(paste("Data_", "color_", 1, sep = "")), color = "black") +
          theme_classic() +
          xlab("Data-set names") +
          ylab("Inter-kinetochore distance [um]")

        tryCatch(
          {
            for (i in 2:nrow(Plot_Data)) {
              P3 <<- P3 + geom_boxplot(
                data = get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")), aes_string("Data", "Inter.kinetochore.distance"),
                fill = get(paste("Data_", "color_", i, sep = "")), color = "black"
              )
            }
          },
          error = function(e) {}
        )

        print(P3)
      },
      error = function(e) {}
    )
  })
}
