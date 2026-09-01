#' Stratified Randomization
#'
#' Randomize independently within one or more strata.
#' @param data Study-subject data frame.
#' @param strata Character vector of stratification columns.
#' @param groups Treatment groups.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#' @return The input data with a `Treatment` column.
#' @examples
#' dat <- data.frame(ID = 1:20, Sex = rep(c("M", "F"), each = 10))
#' stratified_randomization(dat, "Sex", c("Control", "Treatment"), seed = 123)
#' @export
stratified_randomization <- function(data, strata, groups, seed = NULL, ratio = NULL) {
  data <- .validate_data(data); strata <- .validate_strata(data, strata); groups <- .validate_groups(groups); prob <- .validate_ratio(ratio, groups)
  .with_seed(seed, {
    stratum <- .make_stratum(data, strata)
    allocation <- character(nrow(data))
    for (lev in levels(stratum)) {
      idx <- which(stratum == lev)
      allocation[idx] <- sample(groups, length(idx), replace = TRUE, prob = prob)
    }
    data$Treatment <- allocation
    data$Stratum <- as.character(stratum)
    data
  })
}
