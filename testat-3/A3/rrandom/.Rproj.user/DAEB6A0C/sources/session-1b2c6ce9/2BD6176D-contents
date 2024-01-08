library(testthat)
# note for presentation: run devtools::test() to run test

# Test 1: Überprüfe Anzahl Spalten und Zeilen
test_that("describe returns correct number of columns and rows", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  result <- describe(df)
  expect_equal(result$num_columns, ncol(df))
  expect_equal(result$num_rows, nrow(df))
})


# Test 2: Überprüfe, ob alle Spalten im Ergebnis enthalten sind
test_that("describe returns information for all columns", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  result <- describe(df)
  expect_equal(length(names(result$columns_info)), 2)
})

# Test 3: Überprüfe, ob Statistiken für numerische Spalten korrekt
test_that("describe returns correct statistics for numeric columns", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  result <- describe(df)
  expect_equal(result$columns_info$a$type, "numeric")
  expect_equal(result$columns_info$b$type, "numeric")
})


# Test 4: Überprüfe, ob Funktion für leere Datenframe korrekt funktioniert
test_that("describe handles empty data frame", {
  df <- data.frame()
  result <- describe(df)
  expect_equal(result$num_columns, 0)
  expect_equal(result$num_rows, 0)
})


# Test 5: Überprüfe, ob die Funktion mit fehlenden Werten umgehen kann
test_that("describe handles missing values correctly", {
  df <- data.frame(a = c(1, 2, NA), b = c(4, NA, 6))
  result <- describe(df)
  expect_true(all(sapply(result$columns_info$a$col_stats, is.numeric)))
  expect_true(all(sapply(result$columns_info$b$col_stats, is.numeric)))
})

