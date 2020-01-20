app_server <- function(input, output, session) {
  callModule(mod_data_server, "data_ui_1")

  callModule(mod_about_server, "about_ui_1")
}
