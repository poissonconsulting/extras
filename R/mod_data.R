# Module UI

#' @title   mod_data_ui and mod_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data
#' @keywords internal
mod_data_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(),
    mainPanel()
  )
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
}
