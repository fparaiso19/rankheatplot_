# Fictional SUCRA values (%) for six treatments and five outcomes, used in
# examples and tests. "Drug E" was not studied for two outcomes.
example_rankheat <- data.frame(
  treatment = c("Drug A", "Drug B", "Drug C", "Drug D", "Drug E", "Placebo"),
  Efficacy = c(88, 71, 52, 64, 40, 9),
  Remission = c(80, 66, 58, 47, NA, 12),
  `Quality of life` = c(73, 79, 45, 55, 38, 22),
  `Serious adverse events` = c(35, 48, 62, 70, 80, 91),
  Discontinuation = c(42, 55, 61, 33, NA, 75),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save(example_rankheat, file = "data/example_rankheat.rda", compress = "xz")
utils::write.csv(example_rankheat, "inst/extdata/example_rankheat.csv",
                 row.names = FALSE, na = "")
