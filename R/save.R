#' Save a rank-heat plot for publication
#'
#' Writes a rank-heat plot to a file. The format follows the file extension:
#' `.tiff`/`.tif` (LZW-compressed, the usual journal requirement), `.png`,
#' `.jpg`, `.pdf`, `.eps` or `.svg`.
#'
#' The plot's font sizes are tuned for a 7 x 7 inch figure. At other sizes
#' they are rescaled automatically so the figure keeps the same proportions;
#' set `rescale_text = FALSE` to turn that off.
#'
#' @param plot A plot made by [rankheatplot()].
#' @param file Output path.
#' @param width,height Figure size in `units`.
#' @param units `"in"`, `"cm"` or `"mm"`.
#' @param dpi Resolution for raster formats (TIFF, PNG, JPEG).
#' @param rescale_text Scale fonts with the figure size.
#' @param ... Passed to [ggplot2::ggsave()].
#' @return The file path, invisibly.
#' @export
#' @examples
#' f <- tempfile(fileext = ".tiff")
#' save_rankheatplot(rankheatplot(example_rankheat), f, dpi = 300)
save_rankheatplot <- function(plot, file, width = 7, height = 7,
                              units = c("in", "cm", "mm"), dpi = 600,
                              rescale_text = TRUE, ...) {
  units <- match.arg(units)
  size_in <- min(width, height) / switch(units, "in" = 1, cm = 2.54, mm = 25.4)
  if (rescale_text && abs(size_in - 7) > 1e-6) {
    plot <- rescale_plot_text(plot, size_in / 7)
  }
  ext <- tolower(tools::file_ext(file))
  args <- list(filename = file, plot = plot, width = width, height = height,
               units = units, dpi = dpi, bg = "white", ...)
  if (ext %in% c("tif", "tiff")) {
    args$device <- "tiff"
    if (is.null(args$compression)) args$compression <- "lzw"
  } else if (ext == "pdf") {
    args$device <- grDevices::cairo_pdf
  } else if (ext == "eps") {
    args$device <- grDevices::cairo_ps
  }
  do.call(ggplot2::ggsave, args)
  invisible(file)
}

# Rebuild a rank-heat plot with every font size and the colour bar scaled
# by `k`. Plots that were modified after rankheatplot() (and so lost their
# recipe) are returned unchanged.
rescale_plot_text <- function(plot, k) {
  args <- attr(plot, "rankheat_args")
  if (is.null(args)) return(plot)
  args$text_scale <- (args$text_scale %||% 1) * k
  do.call(rankheatplot, args)
}
