#' Simple Randomization
#'
#' Generate a simple random allocation schedule.
#'
#' @param n Number of subjects.
#' @param groups Character vector of treatment groups.
#' @param seed Optional random seed.
#' @param ratio Optional positive allocation weights for the groups.
#' @return A tibble containing subject IDs and assigned groups.
#' @examples
#' simple_randomization(20, c("Control", "Treatment"), seed = 123)
#' @export
simple_randomization <- function(n, groups, seed = NULL, ratio = NULL) {
  n <- .validate_n(n)
  groups <- .validate_groups(groups)
  prob <- .validate_ratio(ratio, groups)
  .with_seed(seed, {
    tibble::tibble(Subject = seq_len(n), Group = sample(groups, n, replace = TRUE, prob = prob))
  })
}
