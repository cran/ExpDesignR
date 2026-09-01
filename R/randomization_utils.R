# Internal validation and randomization helpers

.validate_n <- function(n, name = "n") {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n <= 0 || n != as.integer(n)) {
    stop("'", name, "' must be a positive integer.", call. = FALSE)
  }
  as.integer(n)
}

.validate_groups <- function(groups) {
  if (length(groups) < 2L) stop("At least two groups are required.", call. = FALSE)
  if (anyNA(groups) || any(!nzchar(as.character(groups)))) stop("'groups' must contain non-empty, non-missing labels.", call. = FALSE)
  groups <- as.character(groups)
  if (anyDuplicated(groups)) stop("Treatment group labels must be unique.", call. = FALSE)
  groups
}

.validate_ratio <- function(ratio, groups) {
  groups <- .validate_groups(groups)
  if (is.null(ratio)) ratio <- rep(1, length(groups))
  if (!is.numeric(ratio) || length(ratio) != length(groups) || any(!is.finite(ratio)) || any(ratio <= 0)) {
    stop("'ratio' must contain one positive finite value for each treatment group.", call. = FALSE)
  }
  ratio / sum(ratio)
}

.with_seed <- function(seed, expr) {
  if (is.null(seed)) return(force(expr))
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) stop("'seed' must be a single non-missing number.", call. = FALSE)
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old, envir = .GlobalEnv)
  }, add = TRUE)
  force(expr)
}

.ratio_counts <- function(n, ratio) {
  raw <- n * ratio
  base <- floor(raw)
  rem <- n - sum(base)
  if (rem > 0L) base[order(raw - base, decreasing = TRUE)[seq_len(rem)]] <- base[order(raw - base, decreasing = TRUE)[seq_len(rem)]] + 1L
  base
}

.validate_data <- function(data) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  if (nrow(data) < 1L) stop("'data' must contain at least one row.", call. = FALSE)
  tibble::as_tibble(data)
}

.validate_strata <- function(data, strata) {
  if (!is.character(strata) || length(strata) < 1L || anyNA(strata) || any(!nzchar(strata))) stop("'strata' must contain one or more column names.", call. = FALSE)
  if (!all(strata %in% names(data))) stop("One or more strata variables are not present in data.", call. = FALSE)
  strata
}

.make_stratum <- function(data, strata) {
  do.call(interaction, c(unname(data[strata]), list(drop = TRUE, sep = "::", lex.order = TRUE)))
}
