set.seed(42)

n_rows = 100
data = data.frame(
  age = sample(18:60, n_rows, replace = TRUE),
  income = rnorm(n_rows, mean = 50000, sd = 10000),
  sex = sample(c("f", "m", "d"), n_rows, replace = TRUE),
  height = rnorm(n_rows, mean = 175, sd = 10),
  weight = rnorm(n_rows, mean = 70, sd = 5)
)

head(data)