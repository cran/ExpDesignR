#===========================================================
# Latin Square Design
#===========================================================

#' Latin Square Design
#'
#' Generates a Latin Square design for experimental studies.
#'
#' @param treatments Character vector of treatment labels.
#' @param randomize Logical. Should rows, columns and treatments
#' be randomized? Default is TRUE.
#' @param seed Optional random seed.
#'
#' @return A matrix representing a Latin square.
#'
#' @examples
#' latin_square(
#'   treatments = LETTERS[1:4],
#'   seed = 123
#' )
#'
#' @importFrom stats setNames
#' @export

latin_square <- function(treatments,
                         randomize = TRUE,
                         seed = NULL){
  
  if(!is.null(seed))
    set.seed(seed)
  
  if(length(unique(treatments)) != length(treatments))
    stop("Treatment names must be unique.",
         call. = FALSE)
  
  n <- length(treatments)
  
  if(n < 2)
    stop("At least two treatments are required.",
         call. = FALSE)
  
  ## Basic Latin square
  design <- matrix(NA_character_, n, n)
  
  for(i in seq_len(n)){
    design[i, ] <- treatments[((0:(n-1) + i - 1) %% n) + 1]
  }
  
  ## Randomization
  if(randomize){
    
    row_order <- sample(seq_len(n))
    col_order <- sample(seq_len(n))
    trt_order <- sample(treatments)
    
    design <- design[row_order, col_order]
    
    for(i in seq_len(n)){
      design[design == treatments[i]] <- paste0("TMP", i)
    }
    
    for(i in seq_len(n)){
      design[design == paste0("TMP", i)] <- trt_order[i]
    }
    
  }
  
  rownames(design) <- paste0("Row_", seq_len(n))
  colnames(design) <- paste0("Col_", seq_len(n))
  
  design
  
}