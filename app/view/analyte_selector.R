box::use(
  shiny[fluidPage, moduleServer, NS, selectInput, shinyApp, updateSelectInput],
)

#' @export
ui <- function(id, label = "Select Analyte", multiple = FALSE) {
  ns <- NS(id)
  selectInput(ns("analyte_selector"),
              label = label,
              choices = "",
              selected = 1,
              multiple = multiple)
}

#' @export
server <- function(id, analytes, sort = TRUE) {
  moduleServer(id, function(input, output, session) {
    if (is.null(analytes)) {
      updateSelectInput(session, "analyte_selector", choices = c("No Analytes Found" = ""))
    } else {
      if (sort) analytes <- sort(analytes)
      updateSelectInput(session, "analyte_selector",
                        choices = c("Please select Analyte" = "", analytes),
                        selected = input$analyte_selector)
    }
    input$analyte_selector
  })
}

selector_demo <- function() {
  analytes <- NULL
  analytes1 <- c("Analyte 1", "Analyte 2", "Analyte 3")
  demo_ui <- fluidPage(ui("demo"))
  demo_server <- function(input, output, session) {
    server("demo", analytes1, analytes1[length(analytes1)])
  }
  shinyApp(demo_ui, demo_server)
}
