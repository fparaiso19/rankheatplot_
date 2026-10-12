test_that("the plot builds with default and alternative settings", {
  p <- rankheatplot(example_rankheat)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  for (pal in rankheat_palettes()) {
    expect_s3_class(rankheatplot(example_rankheat, palette = pal), "ggplot")
  }
  p2 <- rankheatplot(example_rankheat, treatment_order = "mean", legend_position = "right",
                     na_label = "*", palette = "grey", digits = 1)
  expect_s3_class(p2, "ggplot")
  f <- tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p2))
})

test_that("treatment and outcome orders are applied", {
  d <- rankheat_data(example_rankheat)
  o <- apply_orders(d, "mean", rev(levels(d$outcome)))
  m <- tapply(example_rankheat[-1] |> as.matrix(), rep(example_rankheat$treatment, 5),
              mean, na.rm = TRUE)
  expect_equal(levels(o$treatment), names(sort(m, decreasing = TRUE)))
  expect_equal(levels(o$outcome), rev(levels(d$outcome)))
  expect_error(rankheatplot(example_rankheat, treatment_order = c("Drug A", "Nope")),
               "not in the data")
})

test_that("text never reads upside down", {
  expect_equal(flipped(c(0, 45, 89, 90, 180, 270, 271, 315)),
               c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("plots are saved in every format", {
  p <- rankheatplot(example_rankheat)
  for (ext in c("tiff", "png", "pdf")) {
    f <- tempfile(fileext = paste0(".", ext))
    save_rankheatplot(p, f, width = 3.5, height = 3.5, dpi = 150)
    expect_gt(file.size(f), 1000)
  }
  skip_if_not_installed("svglite")
  f <- tempfile(fileext = ".svg")
  save_rankheatplot(p, f)
  expect_gt(file.size(f), 1000)
})

test_that("styles set palette and borders unless overridden", {
  cur <- attr(rankheatplot(example_rankheat, style = "current"), "rankheat_args")
  expect_equal(cur$palette, "RdYlGn")
  expect_equal(cur$border_colour, "grey")
  cls <- attr(rankheatplot(example_rankheat), "rankheat_args")
  expect_equal(cls$palette, "veroniki")
  own <- attr(rankheatplot(example_rankheat, style = "current", palette = "grey"), "rankheat_args")
  expect_equal(own$palette, "grey")
})

test_that("TIFFs carry the requested resolution", {
  f <- tempfile(fileext = ".tiff")
  save_rankheatplot(rankheatplot(example_rankheat), f, width = 2, height = 2, dpi = 900)
  # A 2 x 2 inch plot at 900 dpi is 1800 x 1800 pixels.
  expect_gt(file.size(f), 1000)
  skip_if_not_installed("tiff")
  expect_equal(dim(tiff::readTIFF(f))[1:2], c(1800, 1800))
})
