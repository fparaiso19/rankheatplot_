test_that("netmeta P-scores feed the plot", {
  skip_if_not_installed("netmeta")
  suppressPackageStartupMessages(library(netmeta))
  data(Senn2013, package = "netmeta")
  nm <- netmeta(TE, seTE, treat1, treat2, studlab, data = Senn2013, sm = "MD",
                small.values = "desirable")
  d <- rankheat_from_netmeta(list(HbA1c = nm))
  ps <- netrank(nm)$ranking.random
  expect_equal(d$value[match(names(ps), d$treatment)], unname(ps) * 100)
  expect_s3_class(rankheatplot(d), "ggplot")
})

test_that("gemtc SUCRAs feed the plot", {
  skip_if_not_installed("gemtc")
  skip_on_cran()
  set.seed(1)
  m <- gemtc::mtc.model(gemtc::smoking, likelihood = "binom", link = "logit")
  r <- suppressWarnings(utils::capture.output(
    res <- gemtc::mtc.run(m, n.adapt = 500, n.iter = 2000)
  ))
  d <- rankheat_from_gemtc(list(Cessation = res, Reversed = res),
                           preferredDirection = c(1, -1))
  expect_equal(nrow(d), 8)
  expect_true(all(d$value >= 0 & d$value <= 100))
  a <- d$value[d$outcome == "Cessation"]
  b <- d$value[d$outcome == "Reversed"]
  expect_equal(a + b, rep(100, 4), tolerance = 1e-8)
  expect_s3_class(rankheatplot(d), "ggplot")
})
