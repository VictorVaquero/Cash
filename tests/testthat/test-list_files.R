test_that("Reading file lists works", {
  expect_equal(class(list_files()), c("tbl_df", "tbl", "data.frame"))
})
