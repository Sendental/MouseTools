#' Standardize Dates
#'
#' Converts a specified column in a data frame to the `YYYY-MM-DD` format.
#'
#' @importFrom dplyr mutate filter summarise across all_of
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal
#' @importFrom lubridate ymd
#' @importFrom stats IQR quantile median sd
#' @importFrom data.table :=
#' @param data A data frame.
#' @param date_column Name of the column containing dates (unquoted).
#' @return A data frame with standardized date column.
#' @examples
#' library(dplyr)
#' df <- data.frame(Date = c("01/02/2020", "2020-02-03"))
#' date_standardizer(df, Date)
#' @export
date_standardizer <- function(data, date_column) {
  data <- dplyr::mutate(data, {{ date_column }} := lubridate::ymd({{ date_column }}))
}
