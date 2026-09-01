#' Cluster Randomization
#'
#' Randomly assigns intact clusters to treatment groups.
#' @param clusters Character or numeric vector of unique cluster IDs.
#' @param groups Treatment groups.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#' @return A tibble containing cluster assignments.
#' @examples
#' cluster_randomization(paste0("Farm_", 1:20), c("Control", "Treatment"), seed = 123)
#' @export
cluster_randomization <- function(clusters, groups, seed = NULL, ratio = NULL) {
  if (length(clusters) < 1L || anyNA(clusters)) stop("'clusters' must contain at least one non-missing ID.", call. = FALSE)
  clusters <- as.character(clusters); if (anyDuplicated(clusters)) stop("Cluster IDs must be unique.", call. = FALSE)
  groups <- .validate_groups(groups); prob <- .validate_ratio(ratio, groups)
  if (length(clusters) < length(groups)) stop("Number of clusters must be at least the number of groups.", call. = FALSE)
  .with_seed(seed, tibble::tibble(Cluster = clusters, Group = sample(groups, length(clusters), replace = TRUE, prob = prob)))
}
