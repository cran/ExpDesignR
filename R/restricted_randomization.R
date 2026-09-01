#' Restricted Randomization
#'
#' Generate simple random allocations subject to a maximum treatment-count imbalance.
#' @param n Number of subjects.
#' @param groups Treatment groups.
#' @param max_imbalance Maximum allowed difference between the largest and smallest group counts.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#' @return A tibble containing the restricted allocation.
#' @examples
#' restricted_randomization(30, c("A", "B"), max_imbalance = 2, seed = 1)
#' @export
restricted_randomization <- function(n, groups, max_imbalance = 1, seed = NULL, ratio = NULL) {
  n <- .validate_n(n); groups <- .validate_groups(groups); prob <- .validate_ratio(ratio, groups)
  if (!is.numeric(max_imbalance) || length(max_imbalance) != 1L || is.na(max_imbalance) || max_imbalance < 0 || max_imbalance != as.integer(max_imbalance)) stop("'max_imbalance' must be a non-negative integer.", call. = FALSE)
  max_imbalance <- as.integer(max_imbalance)
  .with_seed(seed, {
    counts <- integer(length(groups)); allocation <- character(n)
    for (i in seq_len(n)) {
      target <- (counts + 1L) / pmax(prob, .Machine$double.eps)
      score <- target - min(target)
      eligible <- which((max(counts) - counts) <= max_imbalance | counts == min(counts))
      if (!length(eligible)) eligible <- seq_along(groups)
      w <- prob[eligible] * exp(-score[eligible])
      g <- sample(eligible, 1L, prob = w / sum(w)); allocation[i] <- groups[g]; counts[g] <- counts[g] + 1L
    }
    tibble::tibble(Subject = seq_len(n), Group = allocation)
  })
}
