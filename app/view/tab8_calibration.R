box::use(
  shiny,
  shinyjs[hide, hidden, show, toggle],
  grDevices[colorRamp, rgb],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  plotly[ggplotly, plotlyOutput, renderPlotly],
  utils[read.table],
  ggpubr[ggscatter, stat_cor],
  dplyr[group_by, left_join, mutate, rename, select, summarise],
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
        shiny$hr(),
        shiny$fileInput(ns("refDataFile"),
                        label = "Load Reference Data File",
                        accept = c(".csv", ".tsv")),
        shiny$selectInput(ns("analyte"), "Select Analyte column:", choices = ""),
        shiny$selectInput(ns("variable"), "Select response column:", choices = ""),
        shiny$hr(),
        shiny$radioButtons(ns("calModel"),
         label = "Choose model:",
         choices = list(
           "Linear Model (lm)" = 1,
           "Local Polynomial Model (loess)" = 2
         )
        ),
        shiny$hr(),
        shiny$actionButton(ns("runCali"), label = "Run Calibration", width = "100%")
      ),
      shiny$mainPanel(
        width = 10,
        shiny$uiOutput(ns("calMainPanel"))
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
    
    shiny$observe({
      shiny$updateSelectInput(
        session, "date", choices = unique(intensity_data$df$Date), selected = input$date
      )

      shiny$updateSelectInput(
        session, "id", choices = unique(intensity_data$df$ID[intensity_data$df$Date == input$date]),
        selected = input$id
      )
      
      shiny$observeEvent(input$refDataFile, {
        if (!is.null(input$refDataFile$datapath)) {
          tmp_df <- read.table(input$refDataFile$datapath, header = TRUE, sep = "\t")
          refIntensities(tmp_df)
          shiny$updateSelectInput(session, "analyte", choices = colnames(tmp_df))
          shiny$updateSelectInput(session, "variable", choices = colnames(tmp_df))
        }
      })
      
      output$calMainPanel <- shiny$renderUI({
        if (is.null(calPlot())) {
          shiny$tagList(
            shiny$h3("Reference Intensities", align = "center"),
            reactableOutput(ns("refDT"))
          )
        } else {
          shiny$tagList(
            shiny$h3("Calibration Curve", align = "center"),
            plotlyOutput(ns("calPlot"), height = "80vh", inline=TRUE)
          )
        }
      })
      
      output$refDT <- renderReactable({
        shiny$validate(shiny$need(intensity_data$df, "No intensity data found!"))
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
        shiny$validate(shiny$need(intensity_data$df, "Please load intensity data."))
        shiny$validate(shiny$need(input$date, "Please select column with date and experiment ID!"))
        shiny$validate(shiny$need(input$id, "Please select column with date and experiment ID!"))
        print("Starting with Plot Generation")
        tmp_ref <- refIntensities() |> 
          mutate(Substance = gsub("([0-9]),([0-9])", "\\1.\\2", Substance)) |> 
          rename(Analyte = Substance) |> 
          select(Analyte, Signal) |> 
          group_by(Analyte) |> 
          summarise(Mean = mean(Signal))
        tmp_df <- intensity_data$df[intensity_data$df$Date == input$date &
                                      intensity_data$df$ID == input$id,] |>
          select(Analyte, Mean) |> 
          group_by(Analyte) |> 
          summarise(
            Mean = mean(Mean, na.rm = TRUE),
            .groups = "drop"
          )
        tmp_df <- left_join(tmp_ref, tmp_df, by = "Analyte", suffix = c(".ref", ".smart"))
        # TODO Add low intensities filter
        p <- ggscatter(
          tmp_df,
          x = "Mean.smart",
          y = "Mean.ref",
          add = "loess",
          add.params = list(color = "blue", fill = "red"),
          conf.int = TRUE
        ) +
          stat_cor(
            method = "spearman",
            output.type = "latex",
            label.x.npc = 0.10
          )
        calPlot(p)
        print("I have finished")
      })
      
      shiny$observeEvent(calPlot(), {
        output$calPlot <- renderPlotly({
          ggplotly(calPlot())
        })
      })
    })
  })
}
