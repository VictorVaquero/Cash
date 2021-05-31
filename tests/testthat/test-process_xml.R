setup_books <- function() {
  latest_file <- list_files() %>%
    filter(
      type == "gnca",
      last_date == max(last_date)
    )
  xml_root_node <- read_cash_xml(latest_file)
  books <- get_books(xml_root_node)
  return(books)
}


test_that("Read xml files", {
  latest_file <- list_files() %>%
    filter(
      type == "gnca",
      last_date == max(last_date)
    )
  expect_equal(class(read_cash_xml(latest_file)), c("xml_document", "xml_node"))
})

test_that("Get books from xml", {
  books <- setup_books()

  expect_equal(class(books), c("xml_nodeset"))
  expect_gte(length(books), 1)
})

test_that("Get book metadata", {
  book <- setup_books()[1]

  metadata <- get_book_metadata(book)
  expect_equal(class(metadata), c("tbl_df", "tbl", "data.frame"))
})

test_that("Get currencies", {
  book <- setup_books()[1]

  currencies <- get_book_currencies(book)
  expect_equal(class(currencies), c("tbl_df", "tbl", "data.frame"))
  expect_gte(length(currencies), 1)
})

test_that("Get accounts", {
  book <- setup_books()[1]

  data <- get_book_accounts(book)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_gte(length(data), 1)
})

test_that("Get parents", {
  book <- setup_books()[1]
  accounts <- get_book_accounts(book)

  data <-  find_parents("6d3b0ca0b64031f4839cea47585cac5e", accounts)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(data), 4)
})

test_that("Get transactions", {
  book <- setup_books()[1]
  accounts <- get_book_accounts(book)

  data <- get_book_transactions(book, accounts)
  expect_equal(class(data), c("tbl_df", "tbl", "data.frame"))
  expect_gte(length(data), 1)
})
