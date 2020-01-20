context("shiny tests")

test_that("app ui", {
  ui <- app_ui()
  expect_is(ui, "shiny.tag.list")
})

test_that("app server", {
  server <- app_server
  expect_is(server, "function")
})

test_that("css functions work", {
  expect_is(css_add("test"), "shiny.tag")
  expect_is(css_navbar(), "shiny.tag")
  expect_is(css_hide_errors(), "shiny.tag")
})
