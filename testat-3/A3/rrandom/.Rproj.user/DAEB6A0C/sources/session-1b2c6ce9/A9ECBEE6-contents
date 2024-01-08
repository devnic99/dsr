library(testthat)
# note for presentation: run devtools::test() to run test

# Test 1: Grundlegende Funktionalität testen
test_that("choice returns a single element with default parameters", {
  result <- choice(c(0, 1))
  expect_true(result %in% c(0, 1))
})


# Test 2: Überprüfe, ob die Ausgabe die richtige Länge hat
test_that("choice returns the correct number of elements", {
  result <- choice(c(0, 1, 2), n = 5)
  expect_length(result, 5)
})


# Test 3: Überprüfe, ob die Wahrscheinlichkeiten berücksichtigt werden
test_that("choice takes specified probabilities into account", {
  set.seed(42)
  choices <- choice(c(0, 1), p = c(0.9, 0.1), n = 100)
  # berechne Häufigkeit der choices
  p_0 <- sum(choices == 0) / length(choices)
  p_1 <- sum(choices == 1) / length(choices)

  # erwarte, dass ca. 90% der Werte 0 und 10% der Werte 1 sind
  expect_equal(p_0, 0.9, tolerance = 0.1)  # Toleranz wegen Zufälligkeit
  expect_equal(p_1, 0.1, tolerance = 0.1)
})


# Testfall 4: Überprüfe, ob die Funktion korrekt mit einer einzelnen Auswahl umgeht
test_that("choice handles a single element selection correctly", {
  result <- choice(42)
  expect_equal(result, 42)
})


# Test 5: Überprüfe auf Fehler, wenn Längen von x und p unterschiedlich sind
test_that("choice throws an error when lengths of x and p are different", {
  expect_error(choice(c(0, 1), p = c(0.5, 0.5, 0.5)))
})
