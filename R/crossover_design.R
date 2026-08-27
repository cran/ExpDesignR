#===========================================================
# Crossover Design
#===========================================================

#' Crossover Design
#'
#' Generates a crossover design for clinical, veterinary,
#' pharmaceutical and agricultural experiments.
#'
#' @param treatments Character vector of treatment labels.
#' @param subjects Number of subjects.
#' @param periods Number of study periods.
#' @param seed Optional random seed.
#'
#' @return A tibble containing the crossover schedule.
#'
#' @examples
#' crossover_design(
#'   treatments = c("A","B"),
#'   subjects = 8,
#'   periods = 2,
#'   seed = 123
#' )
#'
#' @importFrom tibble tibble
#' @export

crossover_design <- function(treatments,
                             subjects,
                             periods = length(treatments),
                             seed = NULL){
  
  if(!is.null(seed))
    set.seed(seed)
  
  if(length(unique(treatments)) != length(treatments))
    stop("Treatment labels must be unique.",
         call. = FALSE)
  
  if(subjects < 2)
    stop("subjects must be at least 2.",
         call. = FALSE)
  
  if(periods < 2)
    stop("periods must be at least 2.",
         call. = FALSE)
  
  n_trt <- length(treatments)
  
  sequences <- lapply(seq_len(n_trt), function(i){
    
    treatments[((0:(periods-1) + i - 1) %% n_trt) + 1]
    
  })
  
  sequence_ids <- sample(
    seq_along(sequences),
    size = subjects,
    replace = TRUE
  )
  
  design <- data.frame(
    Subject = seq_len(subjects)
  )
  
  for(p in seq_len(periods)){
    
    design[[paste0("Period_", p)]] <-
      sapply(sequence_ids,
             function(x) sequences[[x]][p])
    
  }
  
  tibble::as_tibble(design)
  
}