#' Variable Block Randomization
#'
#' Generate randomization using randomly selected permitted block sizes.
#'
#' @param n Number of subjects.
#' @param groups Treatment groups.
#' @param block_sizes Permitted block sizes. Each must support the requested
#'   allocation ratio.
#' @param seed Optional random seed.
#' @param ratio Optional allocation weights.
#'
#' @return A tibble with subject, block, block size, and group.
#'
#' @examples
#' \donttest{
#' variable_block_randomization(
#'   30,
#'   c("Control", "Treatment"),
#'   c(4, 6, 8),
#'   seed = 123
#' )
#' }
#'
#' @export
variable_block_randomization <- function(
    n,
    groups,
    block_sizes = c(4, 6, 8),
    seed = NULL,
    ratio = NULL
) {
  
  n <- .validate_n(n)
  groups <- .validate_groups(groups)
  prob <- .validate_ratio(ratio, groups)
  
  if (
    !is.numeric(block_sizes) ||
    length(block_sizes) < 1L ||
    anyNA(block_sizes) ||
    any(block_sizes <= 0) ||
    any(block_sizes != as.integer(block_sizes))
  ) {
    stop(
      "'block_sizes' must contain positive integers.",
      call. = FALSE
    )
  }
  
  block_sizes <- sort(unique(as.integer(block_sizes)))
  
  if (
    any(
      vapply(
        block_sizes,
        function(b) any(.ratio_counts(b, prob) < 1L),
        logical(1)
      )
    )
  ) {
    stop(
      "Every block size must allocate at least one subject to every group.",
      call. = FALSE
    )
  }
  
  # Check whether n can be represented exactly by the permitted
  # block sizes.
  reachable <- logical(n + 1L)
  reachable[1L] <- TRUE
  
  for (i in seq_len(n)) {
    
    if (!reachable[i]) {
      next
    }
    
    for (b in block_sizes) {
      
      j <- i + b
      
      if (j <= n + 1L) {
        reachable[j] <- TRUE
      }
    }
  }
  
  if (!reachable[n + 1L]) {
    stop(
      "The supplied block_sizes cannot exactly partition n. ",
      "Choose block sizes whose sum can equal n.",
      call. = FALSE
    )
  }
  
  .with_seed(seed, {
    
    # Randomly construct a valid sequence of block sizes.
    remaining <- n
    selected_blocks <- integer(0L)
    
    while (remaining > 0L) {
      
      feasible <- block_sizes[block_sizes <= remaining]
      
      if (length(feasible)) {
        feasible <- feasible[
          vapply(
            feasible,
            function(b) reachable[remaining - b + 1L],
            logical(1)
          )
        ]
      }
      
      if (!length(feasible)) {
        stop(
          "Unable to construct a valid sequence of blocks.",
          call. = FALSE
        )
      }
      
      b <- sample(feasible, size = 1L)
      
      selected_blocks <- c(selected_blocks, b)
      remaining <- remaining - b
    }
    out <- vector("list", length(selected_blocks))
    
    for (i in seq_along(selected_blocks)) {
      
      b <- selected_blocks[i]
      
      counts <- .ratio_counts(b, prob)
      
      out[[i]] <- tibble::tibble(
        Block = i,
        BlockSize = b,
        Group = sample(rep(groups, counts))
      )
    }
    
    ans <- dplyr::bind_rows(out)
    
    ans$Subject <- seq_len(n)
    
    ans[, c(
      "Subject",
      "Block",
      "BlockSize",
      "Group"
    )]
  })
}