#' Colour palettes for rank-heat plots
#'
#' Returns the colour stops used to fill the sectors of a rank-heat plot.
#'
#' * `"veroniki"` (default): the red - yellow - green scale of the original
#'   rank-heat plot web tool (dark red = 0%, pale yellow = 50%,
#'   dark green = 100%).
#' * `"ryg"`: a plain three-colour red / yellow / green transformation, as
#'   described in Veroniki et al. (2016).
#' * `"RdYlGn"`: the ColorBrewer diverging palette.
#' * `"grey"`: the grey-scale version from the paper (dark grey = 0%,
#'   light grey = 100%).
#' * `"viridis"`: a colour-blind friendly alternative.
#'
#' @param name Palette name, one of `rankheat_palettes()`.
#' @param reverse Reverse the palette (so the lowest value is green).
#' @return A character vector of colours (low to high).
#' @export
#' @examples
#' rankheat_palette("veroniki")
rankheat_palette <- function(name = "veroniki", reverse = FALSE) {
  name <- match.arg(name, rankheat_palettes())
  pal <- switch(name,
    veroniki = c(
      "#B40926", "#D90E1B", "#F71609", "#F8301D", "#FD4E36", "#FE6642",
      "#FE8B55", "#FCAC66", "#FBC574", "#F8DF7E", "#FBED89", "#F9F59D",
      "#F5FAB5", "#EFFBB8", "#E7FDA3", "#DBFF8B", "#CEFF78", "#BDFE69",
      "#ACFE56", "#96FF4A", "#80FD43", "#64FA38", "#4BF428", "#39EC21",
      "#2AD51C", "#23B617", "#1B9113"
    ),
    ryg = c("#FF0000", "#FFFF00", "#00B000"),
    RdYlGn = c(
      "#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF",
      "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837"
    ),
    grey = c("#4D4D4D", "#F2F2F2"),
    viridis = c(
      "#440154", "#482878", "#3E4A89", "#31688E", "#26828E", "#1F9E89",
      "#35B779", "#6DCD59", "#B4DE2C", "#FDE725"
    )
  )
  if (reverse) pal <- rev(pal)
  pal
}

#' @rdname rankheat_palette
#' @export
rankheat_palettes <- function() {
  c("veroniki", "ryg", "RdYlGn", "grey", "viridis")
}

# Value positions (on a 0-1 scale) of the "veroniki" colour stops, sampled
# from the colour bar of the original tool.
veroniki_stops <- c(
  0, 6, 11, 15, 19, 22, 26.5, 31, 35, 39, 42.6, 45.6, 48.5, 51.5, 55, 58.8,
  62.5, 66, 70, 73.5, 77, 81.6, 85, 88, 92, 95.6, 100
) / 100
