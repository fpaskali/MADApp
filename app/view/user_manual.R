box::use(
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$actionButton(ns("userManual"), "", icon = shiny$icon("circle-question"))
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny::addResourcePath("docs", "vignettes")
    shiny$observeEvent(input$userManual, {
      shiny$showModal(shiny$modalDialog(
        shiny$tags$iframe(
          id = "user-manual-frame",
          src = "docs/MADApp.html"
        ),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
}