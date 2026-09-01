#' Completely Randomized Design
#'
#' Randomly assign experimental units to treatment groups.
#' @param n Number of units.
#' @param treatments Treatment labels.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#' @return A tibble containing unit and treatment.
#' @export
completely_randomized_design <- function(n, treatments, seed = NULL, ratio = NULL) {
  simple_randomization(n, treatments, seed = seed, ratio = ratio) |>
    dplyr::rename(Unit = Subject, Treatment = Group)
}

#' Randomized Block Design
#'
#' Randomize units within balanced blocks.
#' @param n Number of units.
#' @param treatments Treatment labels.
#' @param block_size Block size.
#' @param seed Optional random seed.
#' @return A tibble with unit, block, and treatment.
#' @export
randomized_block_design <- function(n, treatments, block_size = 4, seed = NULL) {
  block_randomization(n, treatments, block_size = block_size, seed = seed) |>
    dplyr::rename(Unit = Subject, Treatment = Group)
}

#' Factorial Design
#'
#' Generate a randomized full-factorial treatment combination design.
#' @param factors Named list of factor levels.
#' @param replicates Number of replicates per combination.
#' @param seed Optional random seed.
#' @return A tibble containing randomized factorial combinations.
#' @export
factorial_design <- function(factors, replicates = 1L, seed = NULL) {
  if (!is.list(factors) || is.null(names(factors)) || length(factors) < 2L) stop("'factors' must be a named list with at least two factors.", call. = FALSE)
  if (any(vapply(factors, length, integer(1)) < 2L)) stop("Each factor must have at least two levels.", call. = FALSE)
  replicates <- .validate_n(replicates, "replicates")
  grid <- expand.grid(factors, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- grid[rep(seq_len(nrow(grid)), each = replicates), , drop = FALSE]
  .with_seed(seed, {
    grid <- grid[sample(seq_len(nrow(grid))), , drop = FALSE]
    grid$Unit <- seq_len(nrow(grid)); grid[, c("Unit", names(factors)), drop = FALSE]
  })
}

#' Split-Plot Design
#'
#' Generate a randomized split-plot treatment schedule.
#' @param whole Whole-plot treatment labels.
#' @param sub Sub-plot treatment labels.
#' @param n_whole Number of whole plots.
#' @param seed Optional random seed.
#' @return A tibble with whole plots and sub-plots.
#' @export
split_plot_design <- function(whole, sub, n_whole = length(whole), seed = NULL) {
  if (length(whole) < 2L || length(sub) < 2L) stop("Both treatment vectors must contain at least two levels.", call. = FALSE)
  n_whole <- .validate_n(n_whole, "n_whole")
  .with_seed(seed, {
    wp <- sample(rep(whole, length.out = n_whole))
    out <- do.call(rbind, lapply(seq_len(n_whole), function(i) data.frame(WholePlot = i, WholeTreatment = wp[i], SubTreatment = sample(sub), stringsAsFactors = FALSE)))
    tibble::as_tibble(out)
  })
}
