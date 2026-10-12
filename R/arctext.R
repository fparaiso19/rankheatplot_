# Text that bends along a circle centred on the origin, used for every label
# of the rank-heat plot. Characters are placed one by one along the arc at
# draw time, so their spacing follows the real font metrics.

GeomArcText <- ggplot2::ggproto("GeomArcText", ggplot2::Geom,
  required_aes = c("x", "y", "label", "theta", "r"),
  default_aes = ggplot2::aes(colour = "black", size = 3.88, vjust = 0.5,
                             family = "", fontface = 1, alpha = NA),
  draw_key = ggplot2::draw_key_text,
  draw_panel = function(data, panel_params, coord) {
    origin <- coord$transform(data.frame(x = 0, y = 0), panel_params)
    unit_x <- coord$transform(data.frame(x = 1, y = 0), panel_params)
    scale <- unit_x$x - origin$x
    grid::gTree(
      data = data, cx = origin$x, cy = origin$y, scale = scale,
      cl = "arctext"
    )
  }
)

#' @exportS3Method grid::makeContent
makeContent.arctext <- function(x) {
  d <- x$data
  kids <- vector("list", nrow(d))
  for (i in seq_len(nrow(d))) {
    kids[[i]] <- arc_label_grob(
      label = d$label[i], theta = d$theta[i], r = d$r[i] * x$scale,
      cx = x$cx, cy = x$cy, vjust = d$vjust[i],
      gp = grid::gpar(
        col = scales::alpha(d$colour[i], d$alpha[i]),
        fontsize = d$size[i] * ggplot2::.pt,
        fontfamily = d$family[i], fontface = d$fontface[i]
      )
    )
  }
  grid::setChildren(x, do.call(grid::gList, kids))
}

arc_label_grob <- function(label, theta, r, cx, cy, vjust, gp) {
  chars <- strsplit(enc2utf8(label), "")[[1]]
  if (!length(chars)) return(grid::nullGrob())
  grid::pushViewport(grid::viewport(gp = gp))
  w <- grid::convertWidth(grid::stringWidth(chars), "npc", valueOnly = TRUE)
  # Rescale the glyph widths so they add up to the width of the whole string.
  total <- grid::convertWidth(grid::stringWidth(label), "npc", valueOnly = TRUE)
  cap <- grid::convertHeight(grid::stringHeight("X"), "npc", valueOnly = TRUE)
  grid::popViewport()
  if (sum(w) > 0) w <- w * total / sum(w)

  flip <- flipped(theta)
  dir <- if (flip) -1 else 1
  # Characters are drawn on their baseline (vjust = 0 keeps the baseline of
  # every glyph on the arc); `vjust` is the share of the cap height that
  # falls inside radius `r`. Flipped text has its baseline on the outside.
  r <- if (flip) r + (1 - vjust) * cap else r - vjust * cap
  # Space the glyphs along the arc through their middle.
  rm <- if (flip) r - cap / 2 else r + cap / 2
  span <- total / rm * 180 / pi
  centres <- (cumsum(w) - w / 2) / rm * 180 / pi
  th <- theta - dir * span / 2 + dir * centres
  rot <- -th + if (flip) 180 else 0
  t <- th * pi / 180
  grid::textGrob(
    chars,
    x = grid::unit(cx + r * sin(t), "npc"),
    y = grid::unit(cy + r * cos(t), "npc"),
    rot = rot, hjust = 0.5, vjust = 0,
    gp = gp
  )
}

geom_arctext <- function(data, ...) {
  ggplot2::layer(
    geom = GeomArcText, stat = "identity", position = "identity",
    data = data,
    mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                           theta = .data$theta, r = .data$r, vjust = .data$vjust),
    inherit.aes = FALSE, show.legend = FALSE, params = list(...)
  )
}
