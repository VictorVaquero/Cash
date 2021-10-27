version_bs_date_picker <- 4 # TODO: Config file? Function?


#' Custom input date
#'
#' @param inputId
#' @param label
#' @param value
#' @param min
#' @param max
#' @param format
#' @param startview
#' @param weekstart
#' @param language
#' @param width
#' @param autoclose
#' @param datesdisabled
#' @param daysofweekdisabled
#'
#' @return
#' @export
#'
#' @examples
mydateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                      minviewmode = "months",
                      language = "en", width = NULL, autoclose = TRUE,
                      datesdisabled = NULL, daysofweekdisabled = NULL) {

  value <- dateYMD(value, "value")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  datesdisabled <- dateYMD(datesdisabled, "datesdisabled")

  value <- shiny::restoreInput(id = inputId, default = value)

  tags$div(id = inputId,
           class = "shiny-date-input form-group shiny-input-container",
           style = htmltools::css(width = validateCssUnit(width)),

           shinyInputLabel(inputId, label),
           tags$input(type = "text",
                      class = "form-control",
                      # `aria-labelledby` attribute is required for accessibility to avoid doubled labels (#2951).
                      `aria-labelledby` = paste0(inputId, "-label"),
                      # title attribute is announced for screen readers for date format.
                      title = paste("Date format:", format),
                      `data-date-language` = language,
                      `data-date-week-start` = weekstart,
                      `data-date-format` = format,
                      `data-date-start-view` = startview,
                      `data-date-min-view-mode` = minviewmode,
                      `data-min-date` = min,
                      `data-max-date` = max,
                      `data-initial-date` = value,
                      `data-date-autoclose` = if (autoclose) "true" else "false",
                      `data-date-dates-disabled` =
                        # Ensure NULL is not sent as `{}` but as 'null'
                        jsonlite::toJSON(datesdisabled, null = 'null'),
                      `data-date-days-of-week-disabled` =
                        jsonlite::toJSON(daysofweekdisabled, null = 'null')
           ),
           datePickerDependency()
  )
}


datePickerDependency <- function(theme) {
  list(
    htmltools::htmlDependency(
      name = "bootstrap-datepicker-js",
      version = version_bs_date_picker,
      src = c(href = "shared/datepicker"),
      script = if (getOption("shiny.minified", TRUE)) "js/bootstrap-datepicker.min.js"
      else                                   "js/bootstrap-datepicker.js",
      # Need to enable noConflict mode. See #1346.
      head = "<script>(function() {
        var datepicker = $.fn.datepicker.noConflict();
        $.fn.bsDatepicker = datepicker;
      })();
     </script>"
    ),
    bslib::bs_dependency_defer(datePickerCSS)
  )
}

shinyInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    # `id` attribute is required for `aria-labelledby` used by screen readers:
    id = paste0(inputId, "-label"),
    `for` = inputId
  )
}

datePickerCSS <- function(theme) {
  if (!bslib::is_bs_theme(theme)) {
    return(htmltools::htmlDependency(
      name = "bootstrap-datepicker-css",
      version = version_bs_date_picker,
      src = c(href = "shared/datepicker"),
      stylesheet = "css/bootstrap-datepicker3.min.css"
    ))
  }

  scss_file <- system.file(package = "shiny", "www/shared/datepicker/scss/build3.scss")

  bslib::bs_dependency(
    input = sass::sass_file(scss_file),
    theme = theme,
    name = "bootstrap-datepicker",
    version = version_bs_date_picker
    # cache_key_extra = shinyPackageVersion()
  )
}

dateYMD <- function(date = NULL, argName = "value") {
  if (!length(date)) return(NULL)
  tryCatch({
    res <- format(as.Date(date), "%Y-%m-%d")
    if (any(is.na(res))) stop()
    date <- res
  },
  error = function(e) {
    warning(
      "Couldn't coerce the `", argName,
      "` argument to a date string with format yyyy-mm-dd",
      call. = FALSE
    )
  }
  )
  date
}
