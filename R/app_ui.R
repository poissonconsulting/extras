app_ui <- function() {
  tagList(
    css_hide_errors(),
    css_navbar(),
    navbarPage(
      title = "Pkgtemplate",
      selected = "Data",
      tabPanel(
        title = "Data",
        br(),
        mod_data_ui("data_ui_1")
      ),
      tabPanel(
        title = "About",
        br(),
        mod_about_ui("about_ui_1")
      )
    )
  )
}
