#' Run dashboard
#'
#' Carrega um dashboard com os prencipais resultados
#' @export
enade_dashboard <- function() {
  appDir <- system.file("shiny", "dashboard", package = "enade")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `enade`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
