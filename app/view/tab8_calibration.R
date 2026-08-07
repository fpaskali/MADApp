box::use(
  shiny,
  shinyjs[disable, enable],
  mgcv[gam],
  grDevices[colorRamp, rgb],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  plotly[ggplotly, plotlyOutput, renderPlotly],
  utils[read.csv],
  ggpubr[ggscatter, stat_cor],
  ggplot2[geom_smooth],
  dplyr[case_when, group_by, left_join, mutate, n_distinct, rename, relocate, select, summarise],
  stats[complete.cases, lm, loess, predict],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel(
    "Calibration", 
    value = "tab8",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 2,
        shiny$selectInput(ns("date"), "Select Date", choices = ""),
        shiny$selectInput(ns("id"), "Select ID", choices = ""),
        shiny$selectInput(ns("param"), "Select variable", choices = c("Mean", "Median")),
        shiny$radioButtons(ns("menuChoice"),
                           "Use a saved model or fit a new one", 
                           choices = list(
                             "Fit Model" = 1,
                             "Calibrate" = 2),
                           inline = TRUE),
        shiny$conditionalPanel(
          ns = ns,
          condition = "input.menuChoice == 1",
          shiny$fileInput(ns("refDataFile"),
                          label = "Load Reference Data File",
                          accept = c(".csv")),
          shiny$numericInput(
            ns("filterLow"),
            "Low Intensities Filter",
            value = 0.05, min = 0, max = 1, step = 0.01
          ),
          shiny$hr(),
          shiny$radioButtons(ns("calModelType"),
                             label = "Choose model:",
                             choices = list(
                               "Linear Model (lm)" = "reg.line",
                               "Generalized Additive Model (gam)" = "gam",
                               "Local Polynomial Model (loess)" = "loess"
                             )
          ),
          shiny$hr(),
          shiny$fluidRow(
            shiny$column(
              shiny$actionButton(ns("runCali"), label = "Run Calibration"),
              width = 6
            ),
            shiny$column(
              shiny$downloadButton(ns("saveModel"), label = "Save Model"),
              width = 6
            )
          )
        ),
        shiny$conditionalPanel(
          ns = ns,
          condition = "input.menuChoice == 2",
          shiny$fileInput(ns("calModel"),
                          label = "Load Calibration Model",
                          accept = c(".rds")),
          shiny$checkboxInput(ns("applyAll"), "Apply calibration to all samples", value = FALSE),
          shiny$actionButton(ns("transform"), label = "Apply Calibration"),
          shiny$hr(),
          shiny$actionButton(ns("showIntensData"), "Show Intensity Data")
        )
      ),
      shiny$mainPanel(
        width = 10,
        shiny$column(
          width = 4,
          shiny$h3("Reference Intensities", align = "center"),
          reactableOutput(ns("refDT"), height = "80vh")
        ),
        shiny$column(
          width = 8,
          shiny$h3("Calibration Curve", align = "center"),
          plotlyOutput(ns("calPlot"), height = "80vh", inline=TRUE)
        )
      )
    )
  )
}

#' @export
server <- function(id, parent_session, intensity_data) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refIntensities <- shiny$reactiveVal()
    calPlot <- shiny$reactiveVal()
    calModel <- shiny$reactiveVal()
    
    shiny$observe({
      if (is.null(calModel())) {
        disable(session$ns("saveModel"), asis = TRUE)
      } else {
        enable(session$ns("saveModel"), asis = TRUE)
      }
    })
    
    shiny$observe({
      shiny$updateSelectInput(
        session, "date", choices = unique(intensity_data$df$Date), selected = input$date
      )

      shiny$updateSelectInput(
        session, "id", choices = unique(intensity_data$df$ID[intensity_data$df$Date == input$date]),
        selected = input$id
      )
    })
      
    shiny$observeEvent(input$refDataFile, {
      if (!is.null(input$refDataFile$datapath)) {
        tmp_df <- read.csv(input$refDataFile$datapath) |> 
          mutate(Analyte = gsub("([0-9]),([0-9])", "\\1.\\2", Analyte))
        
        refIntensities(tmp_df)
        shiny$showNotification("Reference intensities loaded", type = "message")
      }
    })
    
    output$refDT <- renderReactable({
      shiny$validate(shiny$need(refIntensities(), "No reference intensities found!"))
      reactable(
        refIntensities(),
        defaultColDef = colDef(
          format = colFormat(digits = 4)
        ),
        bordered = TRUE,
        defaultPageSize = 20,
        filterable = TRUE
      )
    })
    
    shiny$observeEvent(input$runCali, {
      shiny$isolate({
        if (is.null(intensity_data$df)) {
          shiny$showNotification("Intensity Data not loaded.", type = "error")
        } else if (is.null(refIntensities())) {
          shiny$showNotification("Reference intensities not available", type = "error")
        } else if (input$date == "") {
          shiny$showNotification("Date not selected.", type = "error")
        } else if (input$id == "") {
          shiny$showNotification("ID not selected", type = "error")
        } else {
          tmp_ref <- refIntensities() |>
            group_by(Analyte) |>
            summarise(Mean = mean(Intensity), .groups = "drop")
          tmp_df <- intensity_data$df[intensity_data$df$Date == input$date &
                                        intensity_data$df$ID == input$id,] |>
            select(Analyte, Mean) |>
            group_by(Analyte) |>
            summarise(Mean = mean(Mean, na.rm = TRUE), .groups = "drop")
          tmp_df <- left_join(tmp_ref, tmp_df, by = "Analyte", suffix = c(".ref", ".app"))
          
          # Check if analyte lists match
          n_ref_analytes <- n_distinct(tmp_ref$Analyte)
          n_app_analytes <- n_distinct(tmp_df$Analyte[!is.na(tmp_df$Mean.app)])
          n_missing <- n_ref_analytes - n_app_analytes
          
          if (n_app_analytes == 0) {
            shiny$showNotification(
            "No matching analytes found between reference and app data. 
            Check that analyte data is loaded and analyte names match.",
            type = "error")
            shiny$req(FALSE)
          }
          
          tmp_df <- tmp_df[tmp_df$Mean.ref > input$filterLow,]
          
          # Check if enough measurements for fitting a valid model
          n_total <- nrow(tmp_df)
          n_valid <- sum(complete.cases(tmp_df[,c("Mean.ref", "Mean.app")]))
          n_na_only <- sum(is.na(tmp_df$Mean.ref) | is.na(tmp_df$Mean.app))
          
          if (n_valid == 0) {
            shiny$showNotification("No analytes with valid matched values. Cannot fit a model.",
                                   type = "error")
            shiny$req(FALSE)
          } else if (n_valid < 4) {
            shiny$showNotification(
              sprintf(
                "Only %d analytes have usable values (%d total rows, %d with missing values).
                Too few to fit a model.", n_valid, n_total, n_na_only
              ), type = "error"
            )
            shiny$req(FALSE)
          } else if (n_valid < 5) {
            shiny$showNotification(
              sprintf(
                "Only %d analytes have usable values (%d excluded due to missing values).
                Model fit may be unreliable.", n_valid, n_na_only
              ), type = "warning"
            )
          }
          
          if (input$calModelType == "gam") {
            p <- ggscatter(
              tmp_df,
              x = "Mean.app",
              y = "Mean.ref",
              xlab = "Intensities",
              ylab = "Reference",
              add = "none",
              color = "#384860",
              conf.int = TRUE
            ) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE,
                            color = "#2b3444", fill  = "#97a7c4") 
          } else {
            p <- ggscatter(
              tmp_df,
              x = "Mean.app",
              y = "Mean.ref",
              xlab = "Intensities",
              ylab = "Reference",
              add = input$calModelType,
              color = "#384860",
              add.params = list(color = "#2b3444", fill = "#97a7c4"),
              conf.int = TRUE
            )
            
          }
          p <- p + stat_cor(method = "spearman", output.type = "latex", label.x.npc = 0.10)
          calPlot(p)
          if (input$calModelType == "loess") {
            calModel(loess(Mean.ref ~ Mean.app, data = tmp_df))
          } else if (input$calModelType == "gam") {
            calModel(gam(Mean.ref ~ s(Mean.app), data = tmp_df))
          } else {
            calModel(lm(Mean.ref ~ Mean.app, data = tmp_df))
          }
          shiny$showNotification("Calibration model fitted and ready to apply.", type = ("message"))
        }
      })
    }, ignoreInit = TRUE)
    
    shiny$observeEvent(calPlot(), {
      output$calPlot <- renderPlotly({
        ggplotly(calPlot())
      })
    })
    
    output$saveModel <- shiny$downloadHandler(
      filename = function() {
        "Model.rds"
      },
      content = function(file) {
        saveRDS(calModel(), file = file)
      }
    )
    
    shiny$observeEvent(input$calModel, {
      if (!is.null(input$calModel$datapath)) {
        calModel(readRDS(input$calModel$datapath))
        shiny$showNotification("Calibration model loaded.", type = "message")
      }
    })
    
    shiny$observeEvent(input$transform, {
      if (is.null(intensity_data$df)) {
        shiny$showNotification("No intensity data found!", type = "error")
      } else if (is.null(calModel())) {
        shiny$showNotification("No calibration model found!", type = "error")
      } else {
        if (!"Adj.Mean" %in% names(intensity_data$df)) {
          tmp_df <- intensity_data$df |>
            mutate(Adj.Mean = NA)
        } else {
          tmp_df <- intensity_data$df
        }
        
        if (input$applyAll) {
          tmp_df <- tmp_df |>
            mutate(
              Adj.Mean = predict(calModel(), newdata = data.frame(Mean.app = Mean))
            )
        } else {
          idx <- tmp_df$Date == input$date & tmp_df$ID == input$id
          
          tmp_df$Adj.Mean[idx] <- predict(
            calModel(),
            newdata = data.frame(
              Mean.app = tmp_df$Mean[idx]
            )
          )
        }

        intensity_data$df <- tmp_df |> 
          relocate(Adj.Mean, .after = Mean)
        
        shiny$showNotification("Calibration applied. Adjusted mean column added to intensity data.", type = "message")
      }
    })
    
    shiny$observeEvent(input$showIntensData, {
      shiny$updateTabsetPanel(parent_session, "tabs", selected = "tab5")
    })
  })
}
