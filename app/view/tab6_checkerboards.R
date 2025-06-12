box::use(
  shiny,
  grDevices[colorRamp, rgb],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable]
)

box::use(
  app/view/analyte_selector,
  app/logic/const[LETTERS_EXT],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tabPanel(
    "Checkerboards", 
    value = "tab6",
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        shiny$selectInput(ns("date"), "Select Date", choices = ""),
        shiny$selectInput(ns("id"), "Select ID", choices = ""),
        shiny$selectInput(ns("param"), "Select Parameter", choices = c("Mean", "Median", "Threshold")),
        analyte_selector$ui(ns("analyteSelector")),
        shiny$checkboxInput(ns("heatmap"), "Color coded matrix", value = FALSE)
        ),
      shiny$mainPanel(
        shiny$h3("Intensity Matrices", align = "center"),
        reactableOutput(ns("checkerboard"))
      )
    )
  )
}

#' @export
server <- function(id, parent_session, intensity_data) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observe({
      shiny$updateSelectInput(
        session, "date", choices = unique(intensity_data$df$Date), selected = input$date
      )
      shiny$updateSelectInput(
        session, "id", choices = unique(intensity_data$df$ID[intensity_data$df$Date == input$date]),
        selected = input$id
      )

      output$checkerboard <- renderReactable({
        shiny$validate(shiny$need(intensity_data$df, "Intensity data not found!"))
        shiny$validate(shiny$need(input$date, ""))
        shiny$validate(shiny$need(input$id, ""))
  
        tmp_df <- intensity_data$df[intensity_data$df$Date == input$date &
                                      intensity_data$df$ID == input$id,]
        selected <- analyte_selector$server("analyteSelector", unique(tmp_df$Analyte), TRUE)

        cells <- strsplit(tmp_df$Cell, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
  
        shiny$validate(shiny$need(length(cells) != 0, ""))
        cols <- as.integer(match(cells[[length(cells)]][1], LETTERS_EXT))
        rows <- as.integer(cells[[length(cells)]][2])
        tmp_table <- data.frame(matrix(tmp_df[[input$param]], nrow=rows, ncol=cols))
        colnames(tmp_table) <- LETTERS_EXT[1:cols]
        rownames(tmp_table) <- 1:rows

        colPalette <- function(x) rgb(colorRamp(c("#fc8d59", "#ffffbf", "#91cf60"))(x),
                                      maxColorValue = 255)

        reactable(
          tmp_table,
          defaultColDef = colDef(
            style = function(value, index, col) {
              if (!is.numeric(value) || col == ".rownames") return()
              if (is.na(value)) {
                color <- colPalette(0)
              } else {
                color <- colPalette((value - min(tmp_table, na.rm = TRUE)) / 
                                      (max(tmp_table, na.rm = TRUE) - min(tmp_table, na.rm = TRUE)))
              }
              if (input$heatmap) cell_style <- list(background = color) else cell_style <- list()
              if (selected != "" && tmp_df[tmp_df$Cell == paste0(col, index),]$Analyte == selected) {
                cell_style$border <- "1px solid red"
              }
              cell_style
            },
            format = colFormat(digits = 4),
            minWidth = 50
          ),
          columns = list(
            .rownames = colDef(format = colFormat(digits = 0), minWidth = 30)
          ),
          defaultPageSize = nrow(tmp_table),
          bordered = TRUE,
          rownames = TRUE)
      })
    })
  })
}
