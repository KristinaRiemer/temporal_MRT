# Run all of initial_cleaning_analysis.Rmd by selecting Run -> Run All

context("exploring Portal and Fray Jorge datasets")

test_that("generate correct month and year columns in Fray Jorge mammal data", {
  expect_equal(unique(nchar(fray_occurrences$year)), 4)
  expect_equal(min(fray_occurrences$year), 1989)
  expect_equal(max(fray_occurrences$year), 2005)
  expect_equal(min(fray_occurrences$month), 1)
  expect_equal(max(fray_occurrences$month), 12)
})

# Run all tests with test_file("scripts/test_initial_cleaning_analysis.R")
