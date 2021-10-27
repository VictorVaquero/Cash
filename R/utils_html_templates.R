#' Card
#'
#' @description My custom boostrap card
#'
#' @return html
#'
#' @import assertthat
#'
#' @noRd
customCard <- function(title = NA_character_, content) {
  assertthat::assert_that(is.string(title) | class(title) == "shiny.tag")
  # assertthat::assert_that(is.string(content) | class(content) == "shiny.tag")

  if(is.character(title) & !identical(title, NA_character_)) t <- tags$h5(class = "card-title", title)
  else if (class(title) == "shiny.tag") {
    t <- tags$div(
      class = "card-title d-block",
      title
    )
    # t <- title
    # if (!is.null(t$attribs$class))
    #   t$attribs$class <- glue::glue("card-title {t$attribs$class}")
    # else
    #   t$attribs$class <- "card-title"
    }
  else t <- ""
  tags$div(
    class = "card shadow",
    tags$div(
      class = "card-body",
      t,
      content
    )
  )
}


#' SideItem
#'
#' @description My custom nav item
#'
#' @param text
#'
#' @return html
#'
#' @import assertthat
#'
#' @examples
sideItem <- function(text, .id = NA_character_, .active = FALSE) {
  assertthat::assert_that(is.string(text))
  assertthat::assert_that(is.string(.id))
  assertthat::assert_that(is.logical(.active))

  if (any(!is.na(.id))) {
    ic <- tags$div(class = "d-inline mr-2", shiny::icon(.id))
  } else {
    ic <- ""
  }

  if (.active) {
    class <- " active"
  } else {
    class <- ""
  }

  normalized <- stringr::str_replace_all(tolower(text), " ", "")

  tags$a(
    class = glue::glue("list-group-item list-group-item-action", class, .sep = ""),
    id = glue::glue("list-{normalized}-list"),
    "data-toggle" = "list",
    href = glue::glue("#list-{normalized}"),
    ic,
    text
  )
}

#' Pagina customizada
#'
#' @param text
#' @param content
#' @param .active
#'
#' @return
#' @export
#' @import assertthat
#'
#' @examples
customPage <- function(text, content, .active = FALSE) {
  assertthat::assert_that(is.logical(.active))
  assertthat::assert_that(is.string(text))
  # message(class(content))
  # assertthat::assert_that(is.string(content) | class(content) == "shiny.tag") # TODO: Hacer funcion de is.shiny o similar

  if (.active) {
    class <- " show active"
  } else {
    class <- ""
  }
  normalized <- stringr::str_replace_all(tolower(text), " ", "")
  tags$div(
    class = glue::glue("tab-pane fade", class, .sep = ""),
    id = glue::glue("list-{normalized}"),
    role = "tabpanel",
    "aria-labelledby" = glue::glue("pills-{normalized}-tab"),
    content
  )
}


add_style <- function(tag, new_style) {
  if (!is.null(s$attribs[["style"]]))
    style <- s$attribs[["style"]]
  else
    style <- ""
  style <- glue::glue(style, "; ", style)
}

