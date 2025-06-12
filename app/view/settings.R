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
    settings <- shiny$reactiveValues(showAdvanced = FALSE)

    shiny$observeEvent(input$settings, {
      shiny$showModal(shiny$modalDialog(
        title = "MADApp v0.3 (rhino)",
        shiny$h4("Settings"),
        shiny$checkboxInput(session$ns("advanced"), "Show advanced options", settings$showAdvanced),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    shiny$observeEvent(input$advanced, {
      settings$showAdvanced <- input$advanced
    })

    settings
  })
}
