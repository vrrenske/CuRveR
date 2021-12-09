#' @import shiny
#' @import sortable
#' @import purrr
#' @import readxl
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import ggplot2
#' @import sf
#' @import svglite

#' @include hello.R

#' @export run_curver
run_curver <- function() {
  appDir <- system.file("myapp", package = "CuRveR")
  if (appDir == "") {
    stop("Could not find CuRveR. Try re-installing `CuRveR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
