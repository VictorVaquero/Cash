#' @include utils_general.R


#' Listado y metadatos
#'
#' @return tibble
#' @export
#' @examples list_files()
list_files <- log("list_files") %decor% function() {
  files_directory <- fs::path_package("Cash", "/extdata")

  output <- tibble(path = fs::dir_ls(files_directory)) %>%
  # tibble(path = list.files(files_directory, full.names = TRUE)) %>%
    mutate(
      name = basename(path),
      parts = purrr::map(name, ~ {
        capture <-
          stringr::str_match(
            .x,
            "([0-9]{8})_([0-9]{6})_[^_]+_[^_]+_([^_]+)(?:_[^_]+)?\\.(\\w+)"
          )
        if(any(is.na(capture)))
          logger::log_warn("Invalid gnu file format in {files_directory}\\{.x}")

        tibble(
          last_date = capture[1, 2],
          hour = capture[1, 3],
          account = capture[1, 4],
          type = capture[1, 5],
          is_valid = !any(is.na(capture))
        )
      })
    ) %>%
    unnest(parts) %>%
    mutate(
      last_date = lubridate::ymd_hms(glue::glue("{last_date}{hour}")),
      account = factor(account)
    )

  if(any(is.na(output)))
    warning("File names with bad format")

  return(output)
}

