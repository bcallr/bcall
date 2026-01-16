# Tests for bcall_auto function

test_that("bcall_auto works with basic input", {
  # Create simple test data
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  result <- bcall_auto(rollcall, verbose = FALSE)

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("bcall_object" %in% names(result))
  expect_true("clustering_object" %in% names(result))
  expect_true("metadata" %in% names(result))
})

test_that("bcall_auto returns correct structure", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:6])
  )

  result <- bcall_auto(rollcall, verbose = FALSE)

  # Check results data frame
  expect_s3_class(result$results, "data.frame")
  expect_true("d1" %in% colnames(result$results))
  expect_true("d2" %in% colnames(result$results))
  expect_true("auto_cluster" %in% colnames(result$results))
  expect_true("legislator" %in% colnames(result$results))
})

test_that("bcall_auto handles NAs correctly", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, NA, 1),
    vote2 = c(1, -1, NA, -1, 1),
    vote3 = c(-1, 1, -1, 1, NA),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  expect_silent(result <- bcall_auto(rollcall, verbose = FALSE))
  expect_type(result, "list")
})

test_that("bcall_auto fails with invalid input", {
  # Not a data.frame
  expect_error(bcall_auto(matrix(1:10, nrow = 2)))

  # No rownames
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1)
  )
  expect_error(bcall_auto(rollcall))
})

test_that("bcall_auto accepts custom pivot", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  result <- bcall_auto(rollcall, pivot = "Leg_A", verbose = FALSE)

  expect_equal(result$metadata$pivot, "Leg_A")
})

test_that("bcall_auto respects threshold parameter", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  result <- bcall_auto(rollcall, threshold = 0.5, verbose = FALSE)

  expect_equal(result$metadata$threshold, 0.5)
})
