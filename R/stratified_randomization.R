#===========================================================
# Stratified Randomization
#===========================================================

#' Stratified Randomization
#'
#' Generates a randomized allocation schedule within each stratum.
#'
#' @param data A data frame containing the study subjects.
#' @param strata Character vector specifying one or more stratification variables.
#' @param groups Character vector of treatment groups.
#' @param seed Optional random seed.
#'
#' @return A tibble containing the original data with an additional
#' treatment allocation column.
#'
#' @examples
#' df <- data.frame(
#'   ID = 1:20,
#'   Sex = rep(c("Male","Female"), each = 10),
#'   Age = rep(c("Young","Adult"), times = 10)
#' )
#'
#' stratified_randomization(
#'   data = df,
#'   strata = c("Sex"),
#'   groups = c("Control","Treatment"),
#'   seed = 123
#' )
#'
#' @importFrom tibble as_tibble
#' @export

stratified_randomization <- function(data,
                                     strata,
                                     groups,
                                     seed = NULL){
  
  if(!is.null(seed))
    set.seed(seed)
  
  if(!is.data.frame(data))
    stop("'data' must be a data frame.", call. = FALSE)
  
  if(length(groups) < 2)
    stop("At least two groups are required.", call. = FALSE)
  
  if(!all(strata %in% names(data)))
    stop("One or more strata variables are not present in data.",
         call. = FALSE)
  
  data <- tibble::as_tibble(data)
  
  ## Create stratum label
  stratum <- interaction(
    data[, strata],
    drop = TRUE,
    sep = "_"
  )
  
  allocation <- character(nrow(data))
  
  for(level in levels(stratum)){
    
    idx <- which(stratum == level)
    
    allocation[idx] <- sample(
      rep(
        groups,
        length.out = length(idx)
      )
    )
    
  }
  
  data$Treatment <- allocation
  
  data
  
}