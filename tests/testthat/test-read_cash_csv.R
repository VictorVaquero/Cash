
test_that("Read csv cash file works", {
  file_list <- list_files() %>%
    filter(type == "csv")

  expect_equal(class(read_cash_csv(file_list)), c("tbl_df", "tbl", "data.frame"))
})
