# Tests for bcall function

test_that("bcall works with user-provided clustering", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right", "left"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  result <- bcall(rollcall, clustering, pivot = "Leg_B", verbose = FALSE)

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("bcall_object" %in% names(result))
  expect_true("metadata" %in% names(result))
})

test_that("bcall accepts any 2 cluster values", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  # Test with Spanish labels
  clustering <- data.frame(
    cluster = c("izquierda", "derecha", "izquierda", "derecha"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  expect_silent(result <- bcall(rollcall, clustering, pivot = "Leg_B", verbose = FALSE))
  expect_type(result, "list")
})

test_that("bcall requires pivot parameter", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C")
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = c("Leg_A", "Leg_B", "Leg_C")
  )

  expect_error(bcall(rollcall, clustering))
})

test_that("bcall validates clustering has exactly 2 unique values", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  # Only 1 unique value
  clustering_bad <- data.frame(
    cluster = c("A", "A", "A", "A"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  expect_error(bcall(rollcall, clustering_bad, pivot = "Leg_A"))

  # 3 unique values
  clustering_bad2 <- data.frame(
    cluster = c("A", "B", "C", "A"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  expect_error(bcall(rollcall, clustering_bad2, pivot = "Leg_A"))
})

test_that("bcall validates pivot exists in data", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C")
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = c("Leg_A", "Leg_B", "Leg_C")
  )

  expect_error(bcall(rollcall, clustering, pivot = "Leg_Z"))
})

test_that("bcall accepts clustering as named vector", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C")
  )

  clustering_vec <- c("A" = "left", "B" = "right", "C" = "left")
  names(clustering_vec) <- c("Leg_A", "Leg_B", "Leg_C")

  result <- bcall(rollcall, clustering_vec, pivot = "Leg_B", verbose = FALSE)

  expect_type(result, "list")
})
