#' Fixed Block Randomization
#'
#' Generate a balanced fixed-block randomization schedule.
#'
#' @param n Number of subjects. It must be divisible by `block_size`.
#' @param groups Character vector of treatment groups.
#' @param block_size Size of each block.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#' @return A tibble with subject, block, and treatment assignment.
#'
#' @examples
#' block_randomization(
#'   24,
#'   c("Control", "Treatment"),
#'   4,
#'   seed = 123
#' )
#'
#' @export
block_randomization <- function(
    n,
    groups,
    block_size = 4,
    seed = NULL,
    ratio = NULL
) {
  
  n <- .validate_n(n)
  groups <- .validate_groups(groups)
  
  if (
    !is.numeric(block_size) ||
    length(block_size) != 1L ||
    is.na(block_size) ||
    block_size <= 0 ||
    block_size != as.integer(block_size)
  ) {
    stop(
      "'block_size' must be a positive integer.",
      call. = FALSE
    )
  }
  
  block_size <- as.integer(block_size)
  
  if (n %% block_size != 0L) {
    stop(
      "'n' must be divisible by 'block_size'.",
      call. = FALSE
    )
  }
  
  prob <- .validate_ratio(ratio, groups)
  
  # For equal allocation, block size must be divisible
  # by the number of treatment groups.
  if (is.null(ratio)) {
    
    if (block_size %% length(groups) != 0L) {
      stop(
        "'block_size' must be divisible by the number of groups.",
        call. = FALSE
      )
    }
    
  } else {
    
    # A fixed block can represent a ratio only when
    # the block size is an integer multiple of the
    # smallest integer representation of that ratio.
    
    ratio_scaled <- prob / min(prob)
    
    ratio_scaled <- ratio_scaled / min(ratio_scaled)
    
    ratio_scaled <- round(ratio_scaled, 10)
    
    ratio_counts_base <- round(ratio_scaled)
    
    if (
      any(abs(ratio_scaled - ratio_counts_base) > 1e-8)
    ) {
      stop(
        "Allocation ratio cannot be represented by the requested block size.",
        call. = FALSE
      )
    }
    
    ratio_unit <- sum(ratio_counts_base)
    
    if (block_size %% ratio_unit != 0L) {
      stop(
        "Allocation ratio cannot be represented by the requested block size.",
        call. = FALSE
      )
    }
    
  }
  
  counts <- .ratio_counts(block_size, prob)
  
  if (any(counts < 1L)) {
    stop(
      "Each treatment group must receive at least one subject per block.",
      call. = FALSE
    )
  }
  
  if (sum(counts) != block_size) {
    stop(
      "Allocation ratio cannot be represented by the requested block size.",
      call. = FALSE
    )
  }
  
  .with_seed(seed, {
    
    blocks <- lapply(
      seq_len(n / block_size),
      function(i) {
        sample(rep(groups, counts))
      }
    )
    
    tibble::tibble(
      Subject = seq_len(n),
      Block = rep(
        seq_len(n / block_size),
        each = block_size
      ),
      Group = unlist(
        blocks,
        use.names = FALSE
      )
    )
    
  })
}