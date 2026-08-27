#===========================================================
# Cluster Randomization
#===========================================================

#' Cluster Randomization
#'
#' Randomly assigns clusters (e.g., villages, farms, schools,
#' hospitals) to treatment groups.
#'
#' @param clusters Character or numeric vector of cluster IDs.
#' @param groups Character vector of treatment groups.
#' @param seed Optional random seed.
#'
#' @return A tibble containing cluster assignments.
#'
#' @examples
#' cluster_randomization(
#'   clusters = paste0("Farm_", 1:20),
#'   groups = c("Control", "Treatment"),
#'   seed = 123
#' )
#'
#' @importFrom tibble tibble
#' @export

cluster_randomization <- function(clusters,
                                  groups,
                                  seed = NULL){
  
  if(!is.null(seed))
    set.seed(seed)
  
  if(length(groups) < 2)
    stop("At least two groups are required.",
         call. = FALSE)
  
  if(length(clusters) < length(groups))
    stop("Number of clusters must be at least the number of groups.",
         call. = FALSE)
  
  clusters <- as.character(clusters)
  
  allocation <- sample(
    rep(
      groups,
      length.out = length(clusters)
    )
  )
  
  tibble::tibble(
    Cluster = clusters,
    Group = allocation
  )
  
}