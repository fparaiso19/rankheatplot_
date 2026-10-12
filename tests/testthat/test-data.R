test_that("wide data are reshaped in data order", {
  d <- rankheat_data(example_rankheat)
  expect_equal(levels(d$treatment), example_rankheat$treatment)
  expect_equal(levels(d$outcome), names(example_rankheat)[-1])
  expect_equal(nrow(d), 6 * 5)
  expect_equal(sum(is.na(d$value)), 2)
})

test_that("long data and proportions are recognised", {
  long <- data.frame(Treatment = c("A", "B", "A", "B"),
                     Outcome = c("x", "x", "y", "y"),
                     SUCRA = c(0.9, 0.1, 0.4, 0.6))
  d <- rankheat_data(long)
  expect_equal(d$value[d$treatment == "A" & d$outcome == "x"], 90)
  expect_equal(rankheat_data(long, scale = "percent")$value[1], 0.9)
})

test_that("missing pairs in long data become NA sectors", {
  long <- data.frame(treatment = c("A", "B", "A"), outcome = c("x", "x", "y"),
                     value = c(80, 20, 50))
  d <- rankheat_data(long)
  expect_equal(nrow(d), 4)
  expect_true(is.na(d$value[d$treatment == "B" & d$outcome == "y"]))
})

test_that("matrices, text values and files are read", {
  m <- matrix(c(10, 90, 50, NA), 2, dimnames = list(c("A", "B"), c("o1", "o2")))
  expect_equal(nrow(rankheat_data(m)), 4)
  df <- data.frame(trt = c("A", "B"), o1 = c("45%", "."), check.names = FALSE)
  d <- rankheat_data(df)
  expect_equal(d$value, c(45, NA))
  f <- system.file("extdata", "example_rankheat.csv", package = "rankheatplot")
  expect_equal(read_rankheat(f), rankheat_data(example_rankheat))
  semi <- tempfile(fileext = ".csv")
  writeLines(c("trt;a;b", "X;1,5;20", "Y;30;"), semi)
  expect_equal(read_rankheat(semi)$value, c(1.5, 30, 20, NA))
})

test_that("bad input gives clear errors", {
  expect_error(rankheat_data(data.frame(t = "A", o = "zz")), "Non-numeric")
  dup <- data.frame(treatment = c("A", "A"), outcome = c("x", "x"), value = 1:2)
  expect_error(rankheat_data(dup), "only once")
})
