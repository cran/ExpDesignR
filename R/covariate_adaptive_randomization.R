#' Covariate-Adaptive Randomization
#'
#' Allocate subjects sequentially while reducing imbalance across categorical
#' covariates. A probabilistic element preserves allocation randomness.
#' @param data Study-subject data frame.
#' @param covariates Covariate column names.
#' @param groups Treatment groups.
#' @param p Probability of selecting one of the best-scoring groups.
#' @param seed Optional random seed.
#' @return A tibble containing the original data and `Treatment`.
#' @examples
#' dat <- data.frame(ID = 1:20, Sex = rep(c("M", "F"), 10), Site = rep(c("A", "B"), 10))
#' covariate_adaptive_randomization(dat, c("Sex", "Site"), seed = 1)
#' @export
covariate_adaptive_randomization <- function(data, covariates, groups = c("Control", "Treatment"), p = 0.75, seed = NULL) {
  data <- .validate_data(data); groups <- .validate_groups(groups)
  if (!is.character(covariates) || length(covariates) < 1L || !all(covariates %in% names(data))) stop("'covariates' must name columns in 'data'.", call. = FALSE)
  if (any(vapply(data[covariates], function(z) anyNA(z), logical(1)))) stop("Covariates cannot contain missing values.", call. = FALSE)
  if (!is.numeric(p) || length(p) != 1L || is.na(p) || p < 0.5 || p > 1) stop("'p' must be between 0.5 and 1.", call. = FALSE)
  .with_seed(seed, {
    allocation <- character(nrow(data)); history <- data.frame(stringsAsFactors = FALSE)
    score_candidate <- function(i, g) {
      score <- 0
      for (v in covariates) {
        val <- as.character(data[[v]][i])
        for (lev in unique(as.character(data[[v]]))) {
          totals <- sum(as.character(data[[v]][seq_len(i - 1L)]) == lev)
          group_count <- if (nrow(history)) sum(history$Group == g & history[[v]] == lev) else 0L
          expected <- totals / length(groups)
          score <- score + abs((group_count + as.integer(val == lev)) - expected)
        }
      }
      score
    }
    for (i in seq_len(nrow(data))) {
      scores <- vapply(groups, function(g) score_candidate(i, g), numeric(1))
      best <- which(scores == min(scores)); g <- if (runif(1) < p) sample(best, 1L) else sample(seq_along(groups), 1L)
      allocation[i] <- groups[g]
      row <- as.data.frame(data[i, covariates, drop = FALSE], stringsAsFactors = FALSE); row$Group <- groups[g]
      history <- rbind(history, row)
    }
    data$Treatment <- allocation
    data
  })
}
