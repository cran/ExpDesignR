#' Check Treatment Allocation Balance
#'
#' Summarize treatment-count and percentage imbalance in an allocation schedule.
#' @param schedule A data frame or tibble containing treatment assignments.
#' @param group_col Name of the treatment column.
#' @return A tibble with treatment counts, percentages, and balance statistics.
#' @examples
#' x <- block_randomization(40, c("A", "B"), 4, seed = 1)
#' balance_check(x)
#' @export
balance_check <- function(schedule, group_col = "Group") {
  if (!is.data.frame(schedule)) stop("'schedule' must be a data frame.", call. = FALSE)
  if (!(group_col %in% names(schedule))) stop("Column '", group_col, "' not found in schedule.", call. = FALSE)
  g <- as.character(schedule[[group_col]])
  if (anyNA(g)) stop("Treatment assignments cannot contain missing values.", call. = FALSE)
  counts <- table(g); pct <- 100 * counts / sum(counts)
  tibble::tibble(Group = names(counts), Count = as.integer(counts), Percentage = round(as.numeric(pct), 2), CountDeviation = as.integer(counts - mean(counts)), PercentageDeviation = round(as.numeric(pct - 100 / length(counts)), 2))
}

#' Randomization Diagnostics
#'
#' Return compact diagnostics for a treatment allocation schedule.
#' @param schedule A data frame or tibble containing treatment assignments.
#' @param group_col Name of the treatment column.
#' @return A named list of allocation diagnostics.
#' @examples
#' x <- simple_randomization(50, c("A", "B"), seed = 1)
#' randomization_diagnostics(x)
#' @export
randomization_diagnostics <- function(schedule, group_col = "Group") {
  tab <- balance_check(schedule, group_col)
  list(
    n = sum(tab$Count),
    groups = nrow(tab),
    counts = stats::setNames(tab$Count, tab$Group),
    percentages = stats::setNames(tab$Percentage, tab$Group),
    max_count_difference = max(tab$Count) - min(tab$Count),
    max_percentage_difference = max(tab$Percentage) - min(tab$Percentage)
  )
}
