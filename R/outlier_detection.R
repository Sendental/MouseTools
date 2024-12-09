#' Detect Outliers
#'
#' Identifies outliers in numeric columns based on the IQR method.
#'
#' @importFrom dplyr mutate filter summarise across all_of %>%
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal
#' @importFrom lubridate ymd
#' @importFrom stats IQR quantile median sd
#' @param data A data frame.
#' @param column Name of the numeric column to check for outliers (unquoted).
#' @param threshold Multiplier for the IQR threshold (default is 1.5).
#' @return A list of rows flagged as outliers.
#' @examples
#' library(dplyr)
#' df <- data.frame(Values = c(1, 2, 3, 100))
#' outlier_detection(df, Values)
#' @export
outlier_detection <- function(data, column, threshold = 1.5) {
  column_name <- deparse(substitute(column))  # Capture column name
  column_values <- data[[column_name]]       # Extract column values

  # Ensure column has numeric data
  if (!is.numeric(column_values)) {
    stop("The specified column must be numeric.")
  }

  # Calculate IQR, bounds, and handle potential issues
  iqr <- IQR(column_values, na.rm = TRUE)
  if (iqr == 0 || is.na(iqr)) {
    stop("The IQR is zero or undefined, outlier detection is not meaningful.")
  }

  lower <- quantile(column_values, 0.25, na.rm = TRUE) - threshold * iqr
  upper <- quantile(column_values, 0.75, na.rm = TRUE) + threshold * iqr

  # Filter rows that are outliers
  outliers <- data %>%
    dplyr::filter(
      column_values < lower | column_values > upper
    )

  return(outliers)
}
