box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$actionButton(ns("settings"), "", icon = shiny$icon("gear"))
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    settings <- shiny$reactiveValues(showAdvanced = FALSE, bigGrid = FALSE)

    shiny$observeEvent(input$settings, {
      shiny$showModal(shiny$modalDialog(
        title = "MADApp v1.0.1",
        shiny$h4("Settings"),
        shiny$checkboxInput(session$ns("advanced"), "Show advanced options", settings$showAdvanced),
        shiny$checkboxInput(session$ns("bigGrid"), "Increase the grid size to 50x50", settings$bigGrid),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    shiny$observeEvent(input$advanced, {
      settings$showAdvanced <- input$advanced
    })

    shiny$observeEvent(input$bigGrid, {
      settings$bigGrid <- input$bigGrid
    })

    settings
  })
}
