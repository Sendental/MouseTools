#' Generate Summary Table with Temperature Standardization
#'
#' Summarizes numeric columns and handles unit conversion for temperature data.
#'
#' @importFrom dplyr mutate filter summarise across all_of
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal
#' @importFrom lubridate ymd
#' @importFrom stats IQR quantile median sd
#' @importFrom data.table :=
#' @param data A data frame.
#' @param columns A vector of column names (quoted) to summarize.
#' @param temp_column Name of the temperature column (quoted, optional).
#' @param unit_column Name of the column with temperature units (quoted, optional).
#' @param target_unit Target unit for temperature conversion ("C" or "F").
#' @return A data frame with summary statistics.
#' @examples
#'
#' df <- data.frame(Temperature = c(98.6, 37.0), Unit = c("F", "C"))
#' summary_table_with_conversion(df, c("Temperature"), "Temperature", "Unit", "C")
#' @export
summary_table_with_conversion <- function(data, columns, temp_column = NULL, unit_column = NULL, target_unit = "C") {
  if (!is.null(temp_column) && !is.null(unit_column)) {
    data <- dplyr::mutate(
      data,
      {{ temp_column }} := ifelse(
        {{ unit_column }} == "F" & target_unit == "C",
        ({{ temp_column }} - 32) * 5 / 9,
        ifelse(
          {{ unit_column }} == "C" & target_unit == "F",
          {{ temp_column }} * 9 / 5 + 32,
          {{ temp_column }}
        )
      )
    )
  }

  data <- dplyr::summarise(data, dplyr::across(dplyr::all_of(columns), list(mean = mean, sd = stats::sd, median = stats::median), .names = "{col}_{fn}"))
}
