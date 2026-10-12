#' Prepare data for a rank-heat plot
#'
#' Converts ranking results into the long format used by [rankheatplot()].
#' Two layouts are accepted:
#'
#' * **wide** (the layout of the original web tool): one row per treatment,
#'   the first column holds the treatment names and every other column is an
#'   outcome holding the ranking statistic (SUCRA, P-score, ...). A matrix
#'   with treatment row names also works.
#' * **long**: one row per treatment-outcome pair, with columns for the
#'   treatment, the outcome and the value.
#'
#' Empty cells, `NA` and the strings `"NA"`, `"."`, `"-"` are treated as
#' missing (the treatment was not part of the network for that outcome); such
#' sectors are drawn uncoloured.
#'
#' @param data A data frame or matrix.
#' @param treatment,outcome,value Column names for long data. When `NULL`
#'   the layout is detected: if columns called treatment/outcome/value (or
#'   similar) exist the data are read as long, otherwise as wide.
#' @param scale How to treat the values: `"auto"` multiplies by 100 when all
#'   values lie in \[0, 1\] (P-scores and SUCRAs reported as proportions);
#'   `"percent"` keeps them; `"proportion"` always multiplies by 100.
#' @return A data frame with columns `treatment`, `outcome` (factors, in the
#'   order they appear in the data) and `value` (numeric, 0-100).
#' @export
#' @examples
#' rankheat_data(example_rankheat)
rankheat_data <- function(data, treatment = NULL, outcome = NULL, value = NULL,
                          scale = c("auto", "percent", "proportion")) {
  scale <- match.arg(scale)
  if (is.matrix(data)) {
    rn <- rownames(data)
    if (is.null(rn)) stop("A matrix needs treatment names as row names.", call. = FALSE)
    data <- data.frame(treatment = rn, as.data.frame(data, stringsAsFactors = FALSE),
                       check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame or a matrix.", call. = FALSE)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (nrow(data) == 0) stop("`data` has no rows.", call. = FALSE)

  long_cols <- guess_long_columns(data, treatment, outcome, value)
  if (!is.null(long_cols)) {
    out <- data.frame(
      treatment = as.character(data[[long_cols[1]]]),
      outcome = as.character(data[[long_cols[2]]]),
      value = to_numeric(data[[long_cols[3]]]),
      stringsAsFactors = FALSE
    )
  } else {
    if (ncol(data) < 2) {
      stop("Wide data need a treatment column and at least one outcome column.", call. = FALSE)
    }
    trt_col <- if (!is.null(treatment)) treatment else names(data)[1]
    out_cols <- setdiff(names(data), trt_col)
    trt <- as.character(data[[trt_col]])
    out <- data.frame(
      treatment = rep(trt, times = length(out_cols)),
      outcome = rep(out_cols, each = length(trt)),
      value = unlist(lapply(out_cols, function(o) to_numeric(data[[o]])), use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }

  out$treatment <- trimws(out$treatment)
  out$outcome <- trimws(out$outcome)
  out <- out[!is.na(out$treatment) & out$treatment != "" &
               !is.na(out$outcome) & out$outcome != "", , drop = FALSE]
  if (anyDuplicated(out[c("treatment", "outcome")])) {
    stop("Each treatment-outcome pair must appear only once.", call. = FALSE)
  }

  vals <- out$value[!is.na(out$value)]
  if (scale == "proportion" || (scale == "auto" && length(vals) && all(vals >= 0 & vals <= 1))) {
    out$value <- out$value * 100
  }

  # Complete the grid so absent treatment-outcome pairs become NA sectors.
  trt_levels <- unique(out$treatment)
  out_levels <- unique(out$outcome)
  grid <- expand.grid(treatment = trt_levels, outcome = out_levels,
                      stringsAsFactors = FALSE)
  out <- merge(grid, out, by = c("treatment", "outcome"), all.x = TRUE, sort = FALSE)
  out$treatment <- factor(out$treatment, levels = trt_levels)
  out$outcome <- factor(out$outcome, levels = out_levels)
  out <- out[order(out$outcome, out$treatment), , drop = FALSE]
  rownames(out) <- NULL
  out
}

guess_long_columns <- function(data, treatment, outcome, value) {
  if (!is.null(outcome) || !is.null(value)) {
    cols <- c(treatment %||% names(data)[1], outcome, value)
    if (length(cols) != 3) {
      stop("For long data supply `treatment`, `outcome` and `value`.", call. = FALSE)
    }
    missing <- setdiff(cols, names(data))
    if (length(missing)) {
      stop("Column(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    return(cols)
  }
  if (ncol(data) != 3) return(NULL)
  nm <- tolower(names(data))
  find <- function(pattern) {
    hit <- grep(pattern, nm)
    if (length(hit) == 1) hit else NA_integer_
  }
  idx <- c(
    find("^(treat|treatment|intervention|arm|drug|trt)"),
    find("^(outcome|endpoint|out)"),
    find("^(value|sucra|pscore|p\\.score|p_score|score|rank)")
  )
  if (anyNA(idx)) return(NULL)
  names(data)[idx]
}

to_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "N/A", ".", "-", "NaN")] <- NA
  pct <- grepl("%$", x)
  x <- sub("%$", "", x)
  x <- sub(",", ".", x, fixed = TRUE)
  num <- suppressWarnings(as.numeric(x))
  bad <- !is.na(x) & is.na(num)
  if (any(bad)) {
    stop("Non-numeric values found: ", paste(utils::head(unique(x[bad]), 5), collapse = ", "),
         call. = FALSE)
  }
  num
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Read rank-heat plot data from a file
#'
#' Reads a `.csv`, `.tsv`/`.txt` or `.xlsx`/`.xls` file laid out like the
#' original web tool (treatments in the first column, one column per outcome)
#' or in long format, and returns it ready for [rankheatplot()].
#'
#' @param file Path to the file.
#' @param sheet Sheet to read for Excel files.
#' @param ... Passed to [rankheat_data()].
#' @return A data frame as returned by [rankheat_data()].
#' @export
read_rankheat <- function(file, sheet = 1, ...) {
  ext <- tolower(tools::file_ext(file))
  raw <- switch(ext,
    csv = read_delim_guess(file),
    tsv = utils::read.delim(file, check.names = FALSE, stringsAsFactors = FALSE,
                            na.strings = c("", "NA")),
    txt = read_delim_guess(file),
    xlsx = , xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reading Excel files needs the 'readxl' package.", call. = FALSE)
      }
      as.data.frame(readxl::read_excel(file, sheet = sheet), stringsAsFactors = FALSE)
    },
    stop("Unsupported file type: .", ext, call. = FALSE)
  )
  rankheat_data(raw, ...)
}

read_delim_guess <- function(file) {
  first <- readLines(file, n = 1, warn = FALSE)
  sep <- if (grepl("\t", first)) "\t" else if (lengths(regmatches(first, gregexpr(";", first))) >
                                             lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
  utils::read.table(file, header = TRUE, sep = sep, check.names = FALSE,
                    stringsAsFactors = FALSE, na.strings = c("", "NA"),
                    quote = "\"", comment.char = "", strip.white = TRUE)
}
