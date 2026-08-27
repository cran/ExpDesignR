#===========================================================
# Block Randomization
#===========================================================

#' Block Randomization
#'
#' Generates a randomized allocation schedule using fixed block randomization.
#'
#' @param n Total number of subjects.
#' @param groups Character vector of treatment groups.
#' @param block_size Size of each block. Must be a multiple of the
#' number of treatment groups.
#' @param seed Optional random seed.
#'
#' @return A tibble with subject allocation.
#'
#' @examples
#' block_randomization(
#'   n = 24,
#'   groups = c("Control","Treatment"),
#'   block_size = 4,
#'   seed = 123
#' )
#'
#' @export

block_randomization <- function(n,
                                groups,
                                block_size = 4,
                                seed = NULL){
  
  if(!is.null(seed))
    set.seed(seed)
  
  if(!is.numeric(n) || n <= 0)
    stop("'n' must be a positive integer.", call. = FALSE)
  
  if(length(groups) < 2)
    stop("At least two groups are required.", call. = FALSE)
  
  if(block_size %% length(groups) != 0)
    stop("block_size must be divisible by the number of groups.",
         call. = FALSE)
  
  if(n %% block_size != 0)
    stop("n must be divisible by block_size.",
         call. = FALSE)
  
  n_blocks <- n / block_size
  
  allocation <- character()
  
  per_group <- block_size / length(groups)
  
  for(i in seq_len(n_blocks)){
    
    block <- rep(groups, each = per_group)
    
    allocation <- c(
      allocation,
      sample(block)
    )
    
  }
  
  tibble::tibble(
    Subject = seq_len(n),
    Block = rep(seq_len(n_blocks), each = block_size),
    Group = allocation
  )
  
}