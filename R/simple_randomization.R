#' Simple Randomization
#'
#' Generate a simple random allocation schedule.
#'
#' @param n Number of subjects.
#' @param groups Character vector of treatment groups.
#' @param seed Optional random seed.
#'
#' @return A tibble containing subject IDs and assigned groups.
#'
#' @export
#'
#' @examples
#' simple_randomization(
#'   n = 20,
#'   groups = c("Control", "Treatment"),
#'   seed = 123
#' )
simple_randomization <- function(n,
                                 groups,
                                 seed = NULL) {
  
  if (!is.null(seed))
    set.seed(seed)
  
  if (!is.numeric(n) || length(n) != 1 || n <= 0)
    stop("'n' must be a positive integer.")
  
  if (length(groups) < 2)
    stop("At least two groups are required.")
  
  allocation <- sample(
    groups,
    size = n,
    replace = TRUE
  )
  
  tibble::tibble(
    Subject = seq_len(n),
    Group = allocation
  )
}