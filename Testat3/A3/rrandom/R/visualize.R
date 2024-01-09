#' The function 'visualize' creates box plots for numeric columns in a data frame.
#' @param df data frame for which box plots are to be created
#' @return boxplot for all numeric columns in the data frame. If there are no numerical columns, a warning is issued.

library(ggplot2)

visualize <- function(df) {
  numeric_columns <- sapply(df, is.numeric)
  
  if (!any(numeric_columns)) {
    warning("No numeric columns to visualize")
    return(NULL)
  }
  
  for (col_name in names(df)[numeric_columns]) {
    print(col_name)
    ggplot(df, aes(x = 1, y = df[[col_name]])) +
      geom_boxplot() +
      labs(title = col_name, y = col_name) +
      theme_minimal()
  }
}

data = data.frame(
  age = sample(18:60, n_rows, replace = TRUE),
  income = rnorm(n_rows, mean = 50000, sd = 10000),
  sex = sample(c("f", "m", "d"), n_rows, replace = TRUE),
  height = rnorm(n_rows, mean = 175, sd = 10),
  weight = rnorm(n_rows, mean = 70, sd = 5)
)

visualize(data)
