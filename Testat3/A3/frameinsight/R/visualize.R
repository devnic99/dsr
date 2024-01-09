#' visualize
#'
#' Diese Funktion erstellt Boxplots für numerische Spalten in einem Datenframe.
#'
#' @param df Der Datenframe, für den Boxplots erstellt werden sollen.
#'
#' @return
#' Boxplots für alle numerischen Spalten im Datenframe. Wenn es keine numerischen Spalten gibt, wird eine Warnung ausgegeben.
#'
#' @examples
#' # Beispiel mit einem Datenframe namens 'data'
#' data <- data.frame(
#'   age = sample(18:60, 100, replace = TRUE),
#'   income = rnorm(100, mean = 50000, sd = 10000),
#'   sex = sample(c("f", "m", "d"), 100, replace = TRUE),
#'   height = rnorm(100, mean = 175, sd = 10),
#'   weight = rnorm(100, mean = 70, sd = 5)
#' )
#' visualize(data)
#' @name visualize
#' @export

library(ggplot2)

visualize <- function(df) {
  numeric_columns <- sapply(df, is.numeric)

  if (!any(numeric_columns)) {
    warning("No numeric columns to visualize")
    return(-1)
  }

  num_plots = 0
  for (col_name in names(df)[numeric_columns]) {
    print(col_name)
    ggplot(df, aes(x = 1, y = df[[col_name]])) +
      geom_boxplot() +
      labs(title = col_name, y = col_name) +
      theme_minimal()
    num_plots = num_plots + 1
  }
  return(num_plots)
}

data = data.frame(
  age = sample(18:60, n_rows, replace = TRUE),
  income = rnorm(n_rows, mean = 50000, sd = 10000),
  sex = sample(c("f", "m", "d"), n_rows, replace = TRUE),
  height = rnorm(n_rows, mean = 175, sd = 10),
  weight = rnorm(n_rows, mean = 70, sd = 5)
)

visualize(data)
