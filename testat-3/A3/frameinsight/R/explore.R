describe = function(df) {
  num_columns = ncol(df)
  
  num_rows = nrow(df)

  columns_info = list()
  for (col_name in colnames(df)) {
    col_type = class(df[[col_name]])
    col_info = list(type = col_type)

    if (col_type %in% c("integer", "numeric")) {
        col_stats = c(mean = mean(df[[col_name]], na.rm = TRUE),
                     median = median(df[[col_name]], na.rm = TRUE),
                     sd = sd(df[[col_name]], na.rm = TRUE))
        col_info = append(col_info, list(col_stats=col_stats))
    }
    
    columns_info[[col_name]] = col_info
  }
  
  result = list(
    num_columns = num_columns,
    num_rows = num_rows,
    columns_info = columns_info
  )
  
  return(result)
}

has_NA = function(col)


data = data.frame(
  age = sample(18:60, n_rows, replace = TRUE),
  income = rnorm(n_rows, mean = 50000, sd = 10000),
  sex = sample(c("f", "m", "d"), n_rows, replace = TRUE),
  height = rnorm(n_rows, mean = 175, sd = 10),
  weight = rnorm(n_rows, mean = 70, sd = 5)
)

describe(data)