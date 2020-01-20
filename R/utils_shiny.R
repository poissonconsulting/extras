alter_file <- function(file, pattern) {
  lines <- readLines(file)
  lines <- lines[!lines == pattern]
  writeLines(lines, file)
  invisible(TRUE)
}

cannibalise_shiny <- function(root = getwd()) {
  unlink(file.path(root, "R/app_server.R"))
  unlink(file.path(root, "R/app_ui.R"))
  unlink(file.path(root, "R/run_app.R"))
  unlink(file.path(root, "R/utils_shiny.R"))
  unlink(file.path(root, "R/mod_about.R"))
  unlink(file.path(root, "R/mod_data.R"))
  unlink(file.path(root, "R/addin.R"))
  unlink(file.path(root, "inst"))
  unlink(file.path(root, "tests/testthat/test-shiny.R"))
  alter_file(file.path(root, "R/namespace.R"), "#' @rawNamespace import(shiny, except = p)")
  alter_file(file.path(root, "DESCRIPTION"), "    shiny")
}

txt_input <- function(..., width = "100%") shiny::textInput(..., width = width)

button <- function(..., class = "btn-primary") actionButton(..., class = class)

br2 <- function() tagList(br(), br())
br3 <- function() tagList(br(), br(), br())

help_text <- function(x) {
  p(x, style = "font-size: 11px; color: grey;")
}

select_input_x <- function(..., label = "Select sites:", choices, selected = choices[1]) {
  selectizeInput(...,
    multiple = TRUE, label = label,
    choices = choices,
    selected = selected,
    options = list(
      "plugins" = list("remove_button"),
      "create" = TRUE,
      "persist" = FALSE
    )
  )
}

css_add <- function(x) {
  shiny::tags$head(shiny::tags$style(shiny::HTML(x)))
}

css_hide_errors <- function() {
  css_add("
.shiny-output-error {visibility: hidden;}
.shiny-output-error:before {visibility: hidden;}
")
}

css_navbar <- function(text_color = "#E0E0E0",
                       text_selected_color = "#5bc0de",
                       text_size = "15px",
                       background_color = "#010101",
                       float = "right") {
  css_add(paste0("
.navbar .navbar-nav {float: ", float, ";}

.navbar-header {padding: 0px 20px 0px 30px;}

.navbar.navbar-default.navbar-static-top{
  padding:0;
  margin-bottom: 0px;
  background: rgba(16,16,16, 0.85);
}

.navbar-default {
    box-shadow: 0 4px 4px -2px rgba(0,0,0,.2);
}

.navbar-default {
  background-color: ", background_color, ";
  border-color: transparent;
  font-size: ", text_size, ";
}
.navbar-default .navbar-brand {
  color: ", text_color, ";
}
.navbar-default .navbar-brand:hover,
.navbar-default .navbar-brand:focus {
  color: ", text_color, ";
  background-color: transparent;
}
.navbar-default .navbar-text {
  color: ", text_color, ";
  font-size: 20px !important;
}
.navbar-default .navbar-nav > li > a {
  color:  ", text_color, ";
}
.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:focus {
  color: ", text_selected_color, ";
  background-color: transparent;
}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
  color: ", text_selected_color, ";
  background-color: transparent;
}
.navbar-default .navbar-nav > .disabled > a,
.navbar-default .navbar-nav > .disabled > a:hover,
.navbar-default .navbar-nav > .disabled > a:focus {
  color: ", background_color, ";
  background-color: transparent;
}
.navbar-default .navbar-toggle {
  border-color: ", background_color, ";
}
.navbar-default .navbar-toggle:hover,
.navbar-default .navbar-toggle:focus {
  background-color: ", text_selected_color, ";
}
.navbar-default .navbar-toggle .icon-bar {
  background-color: ", text_color, ";
}
.navbar-default .navbar-collapse,
.navbar-default .navbar-form {
  border-color: transparent;
}
.navbar-default .navbar-nav > .open > a,
.navbar-default .navbar-nav > .open > a:hover,
.navbar-default .navbar-nav > .open > a:focus {
  background-color: ", background_color, ";
  color:", text_selected_color, ";
}
@media (max-width: 767px) {
  .navbar-default .navbar-nav .open .dropdown-menu > li > a {
    color: ", text_color, ";
  }
  .navbar-default .navbar-nav .open .dropdown-menu > li > a:hover,
  .navbar-default .navbar-nav .open .dropdown-menu > li > a:focus {
    color: ", background_color, ";
    background-color: transparent;
  }
  .navbar-default .navbar-nav .open .dropdown-menu > .active > a,
  .navbar-default .navbar-nav .open .dropdown-menu > .active > a:hover,
  .navbar-default .navbar-nav .open .dropdown-menu > .active > a:focus {
    color: ", text_color, ";
    background-color: ", background_color, ";
  }
  .navbar-default .navbar-nav .open .dropdown-menu > .disabled > a,
  .navbar-default .navbar-nav .open .dropdown-menu > .disabled > a:hover,
  .navbar-default .navbar-nav .open .dropdown-menu > .disabled > a:focus {
    color: #cccccc;
    background-color: transparent;
  }
}
.navbar-default .btn-link {
  color: ", text_color, ";
}"))
}
