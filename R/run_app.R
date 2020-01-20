#' Run Shiny Application
#'
#' @export
run_app <- function() {
  shiny::shinyAppDir(system.file("app", package = "pkgtemplate"))
}
