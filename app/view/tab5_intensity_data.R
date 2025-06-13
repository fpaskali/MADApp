box::use(
  shiny,
  utils[read.csv, write.csv],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel("Intensity Table", value = "tab5",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        shiny$fileInput(ns("intensDataFile"),
                  label = "Load Intensity Data File",
                  accept = c("text/csv")),
        shiny$hr(),
        shiny$fluidRow(
          shiny$column(
            shiny$actionButton(ns("deleteData"), label = "Delete Intensity Data", width = "100%"), 
            width = 6),
          shiny$column(
            shiny$downloadButton(ns("downloadData"), "Save Intensity Data", width = "100%"),
            width = 6)
        ),
        shiny$hr(),
        shiny$fluidRow(
          shiny$column(
            shiny$downloadButton(ns("report"), "Save Report"),
            width = 6
          )
        )
      ),
      shiny$mainPanel(
        width = 9,
        shiny$h3("Intensity Data Tables", align = "center"),
        reactableOutput(ns("intensityDT"))
      )
    )
  )
}

#' @export
server <- function(id, parent_session, intensity_data) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observe({
      output$intensityDT <- renderReactable({
        shiny$validate(shiny$need(intensity_data$df, "No intensity data found!"))
        reactable(intensity_data$df,
          defaultColDef = colDef(
            format = colFormat(digits = 4)
          ),
          columns = list(
            ID = colDef(cell = function(x) sprintf("%03d", x), minWidth = 50),
            File = colDef(minWidth = 250),
            Analyte = colDef(minWidth = 200),
            Mode = colDef(minWidth = 50),
            Cell = colDef(minWidth = 50)
          ),
          bordered = TRUE,
          defaultPageSize = 20,
          filterable = TRUE
        )
      })
    })

    shiny$observeEvent(input$intensDataFile, {
      #TODO Add modal asking if you want to overwrite data...
      if (!is.null(input$intensDataFile$datapath)) {
        intensity_data$df <- read.csv(input$intensDataFile$datapath)
      }
    })

    shiny$observeEvent(input$deleteData, {
      intensity_data$df <- NULL
    })

    output$downloadData <- shiny$downloadHandler(
      filename = "IntensityData.csv",
      content = function(file) {
        write.csv(intensity_data$df, file, na = "", row.names = FALSE)
      }
    )
  })
}
