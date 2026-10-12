#' Rank-heat plot data from netmeta analyses
#'
#' Collects P-scores from one `netmeta` (or `netrank`) object per outcome
#' into the long format used by [rankheatplot()]. P-scores are the
#' frequentist analogue of SUCRA values (Ruecker & Schwarzer 2015).
#'
#' @param x A named list with one `netmeta` or `netrank` object per outcome;
#'   the names become the outcome labels, in ring order (first = outermost).
#' @param small.values Per-outcome direction of benefit, passed to
#'   [netmeta::netrank()]: a single value for all outcomes or one per
#'   outcome (e.g. `"desirable"`/`"undesirable"`, or `"good"`/`"bad"` in
#'   recent netmeta versions). `NULL` uses what each analysis was fitted with.
#' @param random Use the random-effects P-scores (`TRUE`) or the common-effect
#'   ones (`FALSE`).
#' @return A data frame with columns `treatment`, `outcome` and `value` (%).
#' @export
#' @examples
#' \dontrun{
#' rh <- rankheat_from_netmeta(list(Efficacy = nm1, Acceptability = nm2),
#'                             small.values = c("undesirable", "desirable"))
#' rankheatplot(rh)
#' }
rankheat_from_netmeta <- function(x, small.values = NULL, random = TRUE) {
  if (!requireNamespace("netmeta", quietly = TRUE)) {
    stop("This function needs the 'netmeta' package.", call. = FALSE)
  }
  if (inherits(x, c("netmeta", "netrank"))) x <- list(Outcome = x)
  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    stop("`x` must be a named list with one analysis per outcome.", call. = FALSE)
  }
  if (!is.null(small.values) && length(small.values) == 1) {
    small.values <- rep(small.values, length(x))
  }
  if (!is.null(small.values) && length(small.values) != length(x)) {
    stop("`small.values` needs one value per outcome.", call. = FALSE)
  }
  rows <- lapply(seq_along(x), function(i) {
    obj <- x[[i]]
    if (inherits(obj, "netmeta")) {
      obj <- if (is.null(small.values)) {
        netmeta::netrank(obj)
      } else {
        netmeta::netrank(obj, small.values = small.values[i])
      }
    }
    if (!inherits(obj, "netrank")) {
      stop("Element '", names(x)[i], "' is not a netmeta or netrank object.", call. = FALSE)
    }
    fields <- if (random) "ranking.random" else c("ranking.common", "ranking.fixed")
    score <- NULL
    for (f in fields) if (is.null(score)) score <- obj[[f]]
    if (is.null(score)) stop("No P-scores found for '", names(x)[i], "'.", call. = FALSE)
    data.frame(treatment = names(score), outcome = names(x)[i],
               value = as.numeric(score) * 100, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rankheat_data(out, "treatment", "outcome", "value", scale = "percent")
}
