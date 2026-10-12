#' Rank-heat plot
#'
#' Draws the rank-heat plot of Veroniki et al. (2016): one concentric ring per
#' outcome and one sector per treatment, each cell coloured by the treatment's
#' ranking statistic (SUCRA, P-score, P(best), ...) for that outcome. The first
#' sector at 12 o'clock lists the outcome names; treatments follow clockwise.
#' The first outcome is the outermost ring.
#'
#' The defaults reproduce the look of the original rank-heat plot web tool.
#'
#' @param data Ranking results, in any layout accepted by [rankheat_data()].
#' @param treatment,outcome,value Optional column names, see [rankheat_data()].
#' @param treatment_order Order of the treatment sectors (clockwise from the
#'   outcome sector): `"data"` keeps the order of the data, `"mean"` sorts by
#'   decreasing mean value across outcomes, `"alphabetical"` sorts by name.
#'   A character vector of treatment names sets the order directly.
#' @param outcome_order `"data"` keeps the order of the data (first outcome
#'   outermost); a character vector of outcome names sets it directly.
#' @param style Overall look. `"classic"` (default) reproduces the original
#'   rank-heat plot website: bright red - yellow - green scale and blue-grey
#'   borders. `"current"` reproduces the app Veroniki's group maintains on
#'   GitHub (averoniki/rankheatplot): ColorBrewer RdYlGn scale and grey
#'   borders. The layout is the same in both. `palette` and `border_colour`
#'   override the style when given.
#' @param palette A palette name from [rankheat_palettes()] or a vector of
#'   colours (low to high).
#' @param reverse_palette Reverse the palette.
#' @param limits Range of the colour scale.
#' @param scale See [rankheat_data()].
#' @param show_values Print the value inside each sector.
#' @param digits Decimal places for the printed values.
#' @param na_label Text printed in sectors without data (the paper uses `"*"`
#'   for the grey-scale version).
#' @param na_fill Fill of sectors without data.
#' @param outcome_title Heading above the outcome sector.
#' @param legend_title Title of the colour bar.
#' @param legend_position `"bottomleft"` (as in the original tool),
#'   `"bottom"`, `"right"`, `"left"`, `"top"` or `"none"`.
#' @param inner_radius Radius of the empty centre as a fraction of the outer
#'   radius.
#' @param sector_gap Gap between sectors, in degrees.
#' @param border_colour,border_width Colour and width (mm) of the cell borders.
#' @param text_colour Colour of all text.
#' @param value_size,outcome_size,treatment_size Font sizes (in mm, as
#'   `ggplot2` text sizes) of the values, the outcome names and the treatment
#'   names. `treatment_size` also sets the outcome heading.
#' @param legend_text_size Font size (pt) of the legend text.
#' @param text_scale Multiplies every font size; handy when exporting at a
#'   size very different from 7 x 7 inches.
#' @param label_offset Distance between the outer ring and the treatment
#'   names, as a fraction of the outer radius.
#' @param family Font family.
#' @return A `ggplot` object; draw it with `print()` or save it with
#'   [save_rankheatplot()].
#' @references Veroniki AA, Straus SE, Fyraridis A, Tricco AC. The rank-heat
#'   plot is a novel way to present the results from a network meta-analysis
#'   including multiple outcomes. J Clin Epidemiol. 2016;76:193-199.
#' @export
#' @examples
#' rankheatplot(example_rankheat)
#' rankheatplot(example_rankheat, palette = "grey", na_label = "*")
rankheatplot <- function(data,
                         treatment = NULL, outcome = NULL, value = NULL,
                         treatment_order = "data",
                         outcome_order = "data",
                         style = c("classic", "current"),
                         palette = NULL,
                         reverse_palette = FALSE,
                         limits = c(0, 100),
                         scale = c("auto", "percent", "proportion"),
                         show_values = TRUE,
                         digits = 0,
                         na_label = "",
                         na_fill = "white",
                         outcome_title = "Outcome",
                         legend_title = "Score",
                         legend_position = "bottomleft",
                         inner_radius = 0.35,
                         sector_gap = 2,
                         border_colour = NULL,
                         border_width = 0.2,
                         text_colour = "black",
                         value_size = 2.3,
                         outcome_size = 2.0,
                         treatment_size = 3.0,
                         legend_text_size = 8,
                         text_scale = 1,
                         label_offset = 0.006,
                         family = "") {
  scale <- match.arg(scale)
  style <- match.arg(style)
  if (is.null(palette)) palette <- if (style == "classic") "veroniki" else "RdYlGn"
  if (is.null(border_colour)) border_colour <- if (style == "classic") "#B3C3C9" else "grey"
  recipe <- mget(names(formals(rankheatplot)), environment())
  d <- rankheat_data(data, treatment, outcome, value, scale = scale)
  d <- apply_orders(d, treatment_order, outcome_order)

  trts <- levels(d$treatment)
  outs <- levels(d$outcome)
  n_sec <- length(trts) + 1
  n_ring <- length(outs)
  sec_width <- 360 / n_sec
  ring_w <- (1 - inner_radius) / n_ring

  # Sector k (0 = outcome labels) is centred at k * sec_width degrees,
  # measured clockwise from 12 o'clock. Ring j = 1 is the outermost.
  sec_mid <- (seq_len(n_sec) - 1) * sec_width
  half <- (sec_width - sector_gap) / 2
  ring_outer <- 1 - (seq_len(n_ring) - 1) * ring_w
  ring_inner <- ring_outer - ring_w
  ring_mid <- (ring_outer + ring_inner) / 2

  d$sec <- as.integer(d$treatment) + 1
  d$ring <- as.integer(d$outcome)
  d$id <- seq_len(nrow(d))

  cells <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    k <- d$sec[i]
    j <- d$ring[i]
    p <- arc_cell(sec_mid[k] - half, sec_mid[k] + half, ring_inner[j], ring_outer[j])
    p$id <- d$id[i]
    p$value <- d$value[i]
    p
  }))
  label_cells <- do.call(rbind, lapply(seq_len(n_ring), function(j) {
    p <- arc_cell(-half, half, ring_inner[j], ring_outer[j])
    p$id <- j
    p
  }))

  ts <- text_scale
  pal <- if (length(palette) == 1 && palette %in% rankheat_palettes()) {
    rankheat_palette(palette, reverse = reverse_palette)
  } else {
    if (reverse_palette) rev(palette) else palette
  }
  pal_values <- if (identical(palette, "veroniki")) {
    if (reverse_palette) rev(1 - veroniki_stops) else veroniki_stops
  } else {
    NULL
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = label_cells,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
      fill = "white", colour = border_colour, linewidth = border_width * ts
    ) +
    ggplot2::geom_polygon(
      data = cells,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$value),
      colour = border_colour, linewidth = border_width * ts
    ) +
    ggplot2::scale_fill_gradientn(
      name = legend_title,
      colours = pal, values = pal_values, limits = limits,
      breaks = pretty(limits, n = 5), na.value = na_fill, oob = scales::squish
    )

  # Outcome names, inside the outcome sector.
  p <- p + geom_arctext(
    arc_labels(outs, 0, ring_mid, 0.5),
    size = outcome_size * ts, colour = text_colour, family = family
  )

  # Values inside the cells, following the arc.
  if (show_values) {
    lbl <- ifelse(is.na(d$value), na_label, formatC(d$value, format = "f", digits = digits))
    vt <- arc_labels(lbl, sec_mid[d$sec], ring_mid[d$ring], 0.5)
    vt <- vt[vt$label != "", , drop = FALSE]
    if (nrow(vt)) {
      p <- p + geom_arctext(vt, size = value_size * ts, colour = text_colour, family = family)
    }
  }

  # Treatment names (and the outcome heading) just outside the outer ring.
  p <- p + geom_arctext(
    arc_labels(c(outcome_title, trts), sec_mid, 1 + label_offset, 0),
    size = treatment_size * ts, colour = text_colour, family = family
  )

  # Leave room for the treatment names around the outer ring.
  lim <- 1.097 + label_offset + 0.112 * max(0, treatment_size * ts / 3 - 1)
  p <- p +
    ggplot2::coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim), expand = FALSE, clip = "off") +
    ggplot2::theme_void(base_family = family) +
    legend_theme(legend_position, legend_text_size * ts, text_colour, ts) +
    colourbar_guide(legend_position, ts)
  attr(p, "rankheat_args") <- recipe
  p
}

# Points of an annular sector between angles a1 < a2 (degrees, clockwise from
# 12 o'clock) and radii r1 < r2.
arc_cell <- function(a1, a2, r1, r2) {
  n <- max(8, ceiling((a2 - a1) / 0.5))
  th <- seq(a1, a2, length.out = n)
  outer <- polar_xy(th, r2)
  inner <- polar_xy(rev(th), r1)
  data.frame(x = c(outer$x, inner$x), y = c(outer$y, inner$y))
}

polar_xy <- function(theta_deg, r) {
  t <- theta_deg * pi / 180
  list(x = r * sin(t), y = r * cos(t))
}

arc_labels <- function(label, theta, r, vjust) {
  pos <- polar_xy(theta, r)
  data.frame(label = label, theta = theta, r = r, x = pos$x, y = pos$y,
             vjust = vjust, stringsAsFactors = FALSE)
}

flipped <- function(theta_deg) {
  th <- theta_deg %% 360
  th >= 90 - 1e-8 & th <= 270 + 1e-8
}

apply_orders <- function(d, treatment_order, outcome_order) {
  trts <- levels(d$treatment)
  if (length(treatment_order) > 1 || !treatment_order %in% c("data", "mean", "alphabetical")) {
    trts <- check_order(treatment_order, trts, "treatment_order")
  } else if (treatment_order == "mean") {
    m <- tapply(d$value, d$treatment, mean, na.rm = TRUE)
    m[is.nan(m)] <- -Inf
    trts <- names(sort(m, decreasing = TRUE))
  } else if (treatment_order == "alphabetical") {
    trts <- sort(trts)
  }
  outs <- levels(d$outcome)
  if (length(outcome_order) > 1 || !identical(outcome_order, "data")) {
    outs <- check_order(outcome_order, outs, "outcome_order")
  }
  d <- d[d$treatment %in% trts & d$outcome %in% outs, , drop = FALSE]
  d$treatment <- factor(as.character(d$treatment), levels = trts)
  d$outcome <- factor(as.character(d$outcome), levels = outs)
  d
}

check_order <- function(order, available, arg) {
  unknown <- setdiff(order, available)
  if (length(unknown)) {
    stop("`", arg, "` has names not in the data: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  unique(order)
}

legend_theme <- function(position, size, colour, ts = 1) {
  base <- ggplot2::theme(
    plot.margin = ggplot2::margin(11, 5.5, 1, 5.5) * ts,
    legend.title = ggplot2::element_text(face = "bold", size = size, colour = colour,
                                         hjust = 0.5, margin = ggplot2::margin(b = -2 * ts)),
    legend.text = ggplot2::element_text(size = size, colour = colour,
                                        margin = ggplot2::margin(t = ts, r = ts, l = ts)),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )
  if (position == "bottomleft") {
    base + ggplot2::theme(
      legend.position = "bottom",
      legend.justification = c(0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(-8 * ts, "pt")
    )
  } else {
    base + ggplot2::theme(legend.position = position)
  }
}

colourbar_guide <- function(position, ts) {
  horizontal <- position %in% c("bottomleft", "bottom", "top")
  w <- grid::unit(if (horizontal) 1.0 else 0.17, "in") * ts
  h <- grid::unit(if (horizontal) 0.11 else 1.0, "in") * ts
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    g <- ggplot2::guide_colourbar(
      title.position = "top", direction = if (horizontal) "horizontal" else "vertical",
      nbin = 20,
      theme = ggplot2::theme(legend.key.width = w, legend.key.height = h,
                             legend.ticks = ggplot2::element_line(colour = "white", linewidth = 0.6))
    )
  } else {
    g <- ggplot2::guide_colourbar(
      title.position = "top", direction = if (horizontal) "horizontal" else "vertical",
      barwidth = w, barheight = h, nbin = 20,
      ticks.colour = "white", ticks.linewidth = 0.6
    )
  }
  ggplot2::guides(fill = g)
}
