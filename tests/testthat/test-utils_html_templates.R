test_that("Card works", {
  my_card <- customCard("Random title", tags$span("Some content"))
  expect_equal(class(my_card), "shiny.tag")
})

# TODO: ADD more, more precise test


test_that("Nav item works", {
  my_card <- sideItem("Random nav", "wallet")
  expect_equal(class(my_card), "shiny.tag")
})
