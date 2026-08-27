#===========================================================
# Plot Randomization
#===========================================================

#' Plot Randomization Schedule
#'
#' Creates a bar chart showing the number of subjects allocated
#' to each treatment group.
#'
#' @param schedule A data frame produced by ExpDesignR.
#' @param group_col Character. Name of the treatment column.
#' @param fill Character. Fill colour.
#' @param title Character. Plot title.
#'
#' @return A ggplot object.
#'
#' @examples
#' sch <- simple_randomization(
#'   n = 40,
#'   groups = c("Control","Treatment"),
#'   seed = 123
#' )
#'
#' plot_randomization(sch)
#'
#' @importFrom dplyr count
#' @importFrom ggplot2 ggplot aes geom_col labs
#' @importFrom ggplot2 theme_bw theme element_text
#' @importFrom rlang .data
#' @export

plot_randomization <- function(schedule,
                               group_col = "Group",
                               fill = "#2C7FB8",
                               title = "Treatment Allocation") {
  
  if (!is.data.frame(schedule))
    stop("'schedule' must be a data frame.",
         call. = FALSE)
  
  if (!(group_col %in% names(schedule)))
    stop(paste0("Column '", group_col,
                "' not found."),
         call. = FALSE)
  
  df <- dplyr::count(
    schedule,
    .data[[group_col]]
  )
  
  names(df) <- c("Group", "Count")
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$Group,
      y = .data$Count
    )
  ) +
    ggplot2::geom_col(
      fill = fill,
      width = 0.7
    ) +
    ggplot2::labs(
      title = title,
      x = "Treatment Group",
      y = "Number of Subjects"
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      ),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(colour = "black")
    )
  
}