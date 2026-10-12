#' Launch the rank-heat plot app
#'
#' Opens a 'shiny' app that reproduces the original rank-heat plot web
#' tool: upload a CSV or Excel file (or paste the table), adjust the plot and
#' download it as TIFF, PNG, PDF, SVG or EPS.
#'
#' @param ... Passed to [shiny::runApp()].
#' @export
run_rankheat_app <- function(...) {
  for (pkg in c("shiny")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("The app needs the '", pkg, "' package.", call. = FALSE)
    }
  }
  shiny::runApp(system.file("shiny", package = "rankheatplot"), ...)
}
