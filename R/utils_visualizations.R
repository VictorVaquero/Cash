#' My custom (dark) plotly theme
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
my_plotly_theme <- function(pt) {
  pt %>%
    plotly::layout(
      plot_bgcolor = "rgba(254, 247, 234, 0)",
      paper_bgcolor = "rgba(45, 45, 45, 0)",
      xaxis = list(
        color = "rgb(256, 256, 256)",
        gridcolor = "rgba(256, 256, 256, 0.5)"
      ),
      yaxis = list(
        color = "rgb(256, 256, 256)",
        gridcolor = "rgba(256, 256, 256, 0.5)"
      ),
      legend = list(font = list(color = "#FFFFFF"))
    )
}

#' My custom palette
#'
#' @return
#' @export
#'
#' @examples
my_palette <- function(n) {
  if (n == 2) {
    pal <- c("#e34a33", "#a1d99b")
  } else {
    pal <- c(
      "#23CE6B", "#7D8CC4", "#E83151",
      "#e76f51", "#B1AAAB", "#F2C078",
      "#C6A606", "#5C80BC", "#2a9d8f"
    )
    pal[1:n]
  }

  return(pal)
}

#' My custom font
#'
#' @return
#' @export
#'
#' @examples
my_font <- function() {
  my_font <- list(family = "Fira Sans")
  return(my_font)
}


#' Añade una linea vertical a plotly
#'
#' @param p
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
vline <- function(p, x, ...) {
  aux <- list(...)
  l_shape <- purrr::map(
    x,
    ~ list(
      type = "line",
      y0 = 0, y1 = 1, yref = "paper", # i.e. y as a proportion of visible region
      x0 = .x, x1 = .x,
      line = aux
    )
  )
  return(p %>%
    plotly::layout(shapes = l_shape))
}

#' Añade una linea horizontal a plotly
#'
#' @param p
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
hline <- function(p, y, ...) {
  aux <- list(...)
  l_shape <- purrr::map(
    y,
    ~ list(
      type = "line",
      x0 = 0, x1 = 1, xref = "paper", # i.e. y as a proportion of visible region
      y0 = .x, y1 = .x,
      line = aux
    )
  )
  p <- p %>%
    plotly::layout(shapes = l_shape)
  return(p)
}
