#' Minimization Randomization
#'
#' Perform covariate-adaptive minimization by selecting the treatment that
#' gives the smallest resulting marginal imbalance, with optional randomness.
#' @param data Study-subject data frame.
#' @param covariates Character vector of categorical covariate columns.
#' @param groups Treatment groups.
#' @param probability Probability of selecting a best-scoring group.
#' @param seed Optional random seed.
#' @return A tibble containing the original data and treatment allocation.
#' @examples
#' dat <- data.frame(ID = 1:30, Sex = rep(c("M", "F"), 15), Site = rep(LETTERS[1:3], 10))
#' minimization_randomization(dat, c("Sex", "Site"), seed = 123)
#' @export
minimization_randomization <- function(data, covariates, groups = c("Control", "Treatment"), probability = 0.8, seed = NULL) {
  data <- .validate_data(data); groups <- .validate_groups(groups)
  if (!is.character(covariates) || length(covariates) < 1L || !all(covariates %in% names(data))) stop("'covariates' must name one or more columns in 'data'.", call. = FALSE)
  if (any(vapply(data[covariates], function(z) anyNA(z), logical(1)))) stop("Covariates cannot contain missing values.", call. = FALSE)
  if (!is.numeric(probability) || length(probability) != 1L || is.na(probability) || probability < 0.5 || probability > 1) stop("'probability' must be between 0.5 and 1.", call. = FALSE)
  .with_seed(seed, {
    allocation <- character(nrow(data)); history <- data.frame(stringsAsFactors = FALSE)
    score <- function(i, g) {
      s <- 0
      for (v in covariates) {
        val <- as.character(data[[v]][i]); levels_v <- unique(as.character(data[[v]]))
        for (lev in levels_v) {
          counts <- if (nrow(history)) table(factor(history$Group, levels = groups), factor(history[[v]], levels = levels_v)) else matrix(0, nrow = length(groups), ncol = length(levels_v), dimnames = list(groups, levels_v))
          counts[g, lev] <- counts[g, lev] + as.integer(val == lev)
          s <- s + max(counts[, lev]) - min(counts[, lev])
        }
      }
      s
    }
    for (i in seq_len(nrow(data))) {
      scores <- vapply(seq_along(groups), function(g) score(i, g), numeric(1)); best <- which(scores == min(scores))
      g <- if (runif(1) < probability) sample(best, 1L) else sample(seq_along(groups), 1L)
      allocation[i] <- groups[g]
      row <- as.data.frame(data[i, covariates, drop = FALSE], stringsAsFactors = FALSE); row$Group <- groups[g]; history <- rbind(history, row)
    }
    data$Treatment <- allocation; data
  })
}
