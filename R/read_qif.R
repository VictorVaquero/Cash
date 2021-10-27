
# TODO: FIX
#' Read  cash qif file
#'
#' @param file
#'
#' @return tibble
#' @export
#'
#' @examples
read_cash_qif <- function(file) {
  file %>%
    read.table(fileEncoding = "utf-8", sep = "\n") %>%
    # First line of a qif file indicates the origin
    filter(row_number() != 1) %>%
    # Each operation is separated by an empty line
    mutate(newline = str_count(V1, "") == 1) %>%
    mutate(operation = cumsum(newline)) %>%
    filter(!newline) %>%
    # The type of the variable is define by the first letter
    mutate(col = str_extract(V1, "^.?")) %>%
    spread(col, V1) %>%
    select(-newline, -operation) %>%
    mutate(Date = mdy(str_replace(str_replace(D, "^D", ""), "'", "/")),
           Description = str_replace(P, "^P", ""),
           Amount = as.numeric(str_replace(str_replace(T, "^T", ""), ",", "")))
}

