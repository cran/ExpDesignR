#' Matched-Pair Randomization
#'
#' Randomly assigns one member of each matched pair to each of two treatments.
#'
#' @param data Study-subject data frame.
#' @param pair Column containing matched-pair IDs.
#' @param groups Exactly two treatment labels.
#' @param seed Optional random seed.
#' @return A tibble containing the original data and treatment assignment.
#'
#' @examples
#' dat <- data.frame(
#'   ID = 1:10,
#'   Pair = rep(1:5, each = 2)
#' )
#'
#' matched_pair_randomization(
#'   dat,
#'   "Pair",
#'   c("Control", "Treatment"),
#'   seed = 1
#' )
#'
#' @export
matched_pair_randomization <- function(
    data,
    pair,
    groups = c("Control", "Treatment"),
    seed = NULL
) {
  
  data <- .validate_data(data)
  
  # Matched-pair randomization requires exactly two groups.
  if (length(groups) != 2L) {
    stop(
      "Matched-pair randomization requires exactly two treatment groups.",
      call. = FALSE
    )
  }
  
  groups <- .validate_groups(groups)
  
  if (
    !is.character(pair) ||
    length(pair) != 1L ||
    !(pair %in% names(data))
  ) {
    stop(
      "'pair' must be the name of a column in 'data'.",
      call. = FALSE
    )
  }
  
  if (anyNA(data[[pair]])) {
    stop(
      "Pair IDs cannot contain missing values.",
      call. = FALSE
    )
  }
  
  counts <- table(data[[pair]])
  
  if (any(counts != 2L)) {
    stop(
      "Each matched pair must contain exactly two subjects.",
      call. = FALSE
    )
  }
  
  .with_seed(seed, {
    
    allocation <- character(nrow(data))
    pairs <- unique(data[[pair]])
    
    for (p in pairs) {
      
      idx <- which(data[[pair]] == p)
      
      allocation[idx] <- sample(groups)
      
    }
    
    data$Treatment <- allocation
    
    data
    
  })
}