test_that("app ui", {
  ui <- mod_main_ui("ui")
  # golem::expect_shinytaglist(ui)
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_main_ui)
  for (i in c("request")){
    expect_true(i %in% names(fmls))
  }
})

test_that("app server", {
  server <- mod_main_server
  expect_type(server, "function")
  # Check that formals have not been removed
  fmls <- formals(mod_main_server)
  for (i in c("input", "output", "session")){
    expect_true(i %in% names(fmls))
  }
})

# Configure this test to fit your need
test_that(
  "app launches",{
    golem::expect_running(sleep = 5)
  }
)
