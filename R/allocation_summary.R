#===========================================================
# Allocation Summary
#===========================================================

#' Allocation Summary
#'
#' Summarizes treatment allocations from a randomization schedule.
#'
#' @param schedule A data frame or tibble produced by an
#' ExpDesignR randomization function.
#'
#' @param group_col Name of the treatment group column.
#'
#' @return A tibble summarizing the number and percentage
#' of subjects in each treatment group.
#'
#' @examples
#' sch <- simple_randomization(
#'   n = 20,
#'   groups = c("Control","Treatment"),
#'   seed = 123
#' )
#'
#' allocation_summary(sch)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr group_by summarise mutate n
#' @export

allocation_summary <- function(schedule,
                               group_col = "Group") {
  
  if (!is.data.frame(schedule))
    stop("'schedule' must be a data frame.",
         call. = FALSE)
  
  if (!(group_col %in% names(schedule)))
    stop(paste0("Column '", group_col,
                "' not found in schedule."),
         call. = FALSE)
  
  grp <- schedule[[group_col]]
  
  result <- tibble::tibble(Group = grp)
  
  result <- result |>
    dplyr::group_by(.data$Group) |>
    dplyr::summarise(
      Count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Percentage = round(
        100 * .data$Count / sum(.data$Count),
        2
      )
    )
  
  result
}