#' Listado y metadatos
#'
#' @return tibble
#' @export
#' @examples list_files()
list_files <- function() {
  files_directory <- fs::path_package("Cash", "/extdata")

  tibble(path = fs::dir_ls(files_directory)) %>%
  # tibble(path = list.files(files_directory, full.names = TRUE)) %>%
    mutate(
      name = basename(path),
      parts = purrr::map(name, ~ {
        capture <-
          stringr::str_match(
            .x,
            "([0-9]{8})_([0-9]{6})_[^_]+_[^_]+_([^_]+)(?:_[^_]+)?\\.(\\w+)"
          )
        tibble(
          last_date = capture[1, 2],
          other = capture[1, 3],
          account = capture[1, 4],
          type = capture[1, 5]
        )
      })
    ) %>%
    unnest(parts) %>%
    mutate(
      last_date = lubridate::ymd(last_date),
      account = factor(account)
    )
}

