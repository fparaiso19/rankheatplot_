#' Rank-heat plot data from gemtc analyses
#'
#' Computes SUCRA values from one Bayesian network meta-analysis per outcome,
#' fitted with [gemtc::mtc.run()], and returns them in the long format used by
#' [rankheatplot()].
#'
#' SUCRAs from any other Bayesian software (multinma, BUGSnet, WinBUGS,
#' JAGS, Stan, ...) work too: put them in a table with treatments in the
#' first column and one column per outcome, and pass it to [rankheatplot()].
#'
#' @param x A named list with one `mtc.result` object per outcome; the names
#'   become the outcome labels, in ring order (first = outermost).
#' @param preferredDirection Direction of benefit, passed to
#'   [gemtc::rank.probability()]: `1` when higher effect estimates are better
#'   (e.g. log odds of response), `-1` when lower are better (e.g. log odds
#'   of an adverse event). One value for all outcomes or one per outcome.
#' @return A data frame with columns `treatment`, `outcome` and `value` (%).
#' @export
#' @examples
#' \dontrun{
#' rh <- rankheat_from_gemtc(list(Response = res1, `Adverse events` = res2),
#'                           preferredDirection = c(1, -1))
#' rankheatplot(rh)
#' }
rankheat_from_gemtc <- function(x, preferredDirection = 1) {
  if (!requireNamespace("gemtc", quietly = TRUE)) {
    stop("This function needs the 'gemtc' package.", call. = FALSE)
  }
  if (inherits(x, "mtc.result")) x <- list(Outcome = x)
  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    stop("`x` must be a named list with one analysis per outcome.", call. = FALSE)
  }
  if (length(preferredDirection) == 1) preferredDirection <- rep(preferredDirection, length(x))
  if (length(preferredDirection) != length(x)) {
    stop("`preferredDirection` needs one value per outcome.", call. = FALSE)
  }
  rows <- lapply(seq_along(x), function(i) {
    if (!inherits(x[[i]], "mtc.result")) {
      stop("Element '", names(x)[i], "' is not an mtc.result object.", call. = FALSE)
    }
    ranks <- gemtc::rank.probability(x[[i]], preferredDirection = preferredDirection[i])
    s <- gemtc::sucra(ranks)
    data.frame(treatment = names(s), outcome = names(x)[i],
               value = as.numeric(s) * 100, stringsAsFactors = FALSE)
  })
  rankheat_data(do.call(rbind, rows), "treatment", "outcome", "value", scale = "percent")
}
