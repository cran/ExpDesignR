#' Stratified Block Randomization
#'
#' Perform blocked randomization independently within each stratum.
#' @param data Study-subject data frame.
#' @param strata Character vector of stratification columns.
#' @param groups Treatment groups.
#' @param block_size Block size.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#' @return A tibble containing the original data, stratum, block, and treatment.
#' @examples
#' dat <- data.frame(ID = 1:40, Sex = rep(c("M", "F"), each = 20))
#' stratified_block_randomization(dat, "Sex", c("A", "B"), 4, seed = 1)
#' @export
stratified_block_randomization <- function(data, strata, groups, block_size = 4, seed = NULL, ratio = NULL) {
  data <- .validate_data(data); strata <- .validate_strata(data, strata); groups <- .validate_groups(groups); prob <- .validate_ratio(ratio, groups)
  if (!is.numeric(block_size) || length(block_size) != 1L || block_size <= 0 || block_size != as.integer(block_size)) stop("'block_size' must be a positive integer.", call. = FALSE)
  block_size <- as.integer(block_size); counts <- .ratio_counts(block_size, prob)
  if (any(counts < 1L)) stop("Each block must allocate at least one subject to every group.", call. = FALSE)
  .with_seed(seed, {
    stratum <- .make_stratum(data, strata); allocation <- character(nrow(data)); block <- integer(nrow(data)); bglobal <- 0L
    for (lev in levels(stratum)) {
      idx <- which(stratum == lev)
      pos <- 1L; local <- 0L
      while (pos <= length(idx)) {
        take <- min(block_size, length(idx) - pos + 1L); local <- local + 1L; bglobal <- bglobal + 1L
        if (take == block_size) vals <- sample(rep(groups, counts)) else vals <- sample(groups, take, replace = TRUE, prob = prob)
        ii <- idx[pos:(pos + take - 1L)]; allocation[ii] <- vals; block[ii] <- bglobal; pos <- pos + take
      }
    }
    data$Treatment <- allocation; data$Stratum <- as.character(stratum); data$Block <- block; data
  })
}
