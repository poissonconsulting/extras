# Module UI

#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#' @keywords internal
mod_about_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      1,
      HTML("")
    ),
    column(
      11,
      h4(paste("Welcome!")),
      br(),
      h5(
        "For more information about this app, see the ",
        a("GitHub page.", href = "https://github.com/pkgtemplate/rems")
      ),
      h6("Developed by Poisson Consulting.")
    ),
    tags$footer(actionLink(
      inputId = "poisson",
      label = img(
        src = "https://www.poissonconsulting.ca/assets/logos/poisson.png",
        height = 177 / 5,
        width = 739 / 5,
        onclick = "window.open('http://www.poissonconsulting.ca', '_blank')"
      )
    ),
    align = "center",
    style = "position: relative;
                bottom:1;
                width:100%;
                height:50px; /* Height of the footer */
                color: #2f4f4f;
                padding: 10px;
                background-color: white;
                z-index: -1000;
                font-size: 12px"
    )
  )
}

# Module Server

#' @rdname mod_about
#' @keywords internal

mod_about_server <- function(input, output, session) {
  ns <- session$ns
}
