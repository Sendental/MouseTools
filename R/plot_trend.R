#' Plot Trend
#'
#' Visualizes a trend from a data frame.
#'
#' @importFrom dplyr mutate filter summarise across all_of
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal
#' @importFrom lubridate ymd
#' @importFrom stats IQR quantile median sd
#' @param data A data frame.
#' @param x Name of the column for x-axis (unquoted).
#' @param y Name of the column for y-axis (unquoted).
#' @param group Optional column to group data by (unquoted).
#' @return A ggplot object.
#' @examples
#' library(dplyr)
#' df <- data.frame(Day = 1:10, Weight = rnorm(10))
#' plot_trend(df, Day, Weight)
#' @export
plot_trend <- function(data, x, y, group = NULL) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ group }})) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}
