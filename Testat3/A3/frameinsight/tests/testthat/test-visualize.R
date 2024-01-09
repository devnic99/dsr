library(testthat)
# note for presentation: run devtools::test() to run test

# Test 1: Check if the function shows warning for dataframe with no numeric columns
test_that("visualize returns warning for dataframe with no numeric columns", {
  df <- data.frame(name = c("John", "Jane"), grade = c("A", "B"))
  expect_warning(visualize(df), "No numeric columns to visualize")
})


# Test 2: Check if the function returns warning for dataframe with all numeric columns being NULL
test_that("visualize returns warning for dataframe with all numeric columns being NULL", {
  df <- data.frame(age = NULL, income = NULL)
  expect_warning(visualize(df), "No numeric columns to visualize")
})


# Test 3: Check if the function returns correct number of plots for a dataframe with mixed columns
test_that("returns correct number of plots for a dataframe with mixed columns", {
  df <- data.frame(
    age = c(25, 30, 35),
    category = c("A", "B", "A"),
    income = c(50000, 60000, 70000)
  )
  expect_equal(visualize(df), sum(sapply(df, is.numeric)))
})


# Test 4: Check if the function generates a plot for each numeric column in the dataframe
test_that("generates a plot for each numeric column", {
  df <- data.frame(
    age = c(25, 30, 35),
    income = c(50000, 60000, 70000),
    height = c(170, 175, 180)
  )
  num_plots <- visualize(df)
  expect_equal(num_plots, sum(sapply(df, is.numeric)))
})

