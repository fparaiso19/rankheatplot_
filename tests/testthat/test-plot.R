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
