test_that("bcall_auto works with valid input", {
  # Create sample rollcall data
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    vote4 = c(1, 1, -1, -1, 1, -1),
    row.names = paste0("Legislator_", LETTERS[1:6])
  )

  # Run analysis
  results <- bcall_auto(rollcall, verbose = FALSE)

  # Check structure
  expect_type(results, "list")
  expect_named(results, c("results", "bcall_object", "clustering_object", "metadata"))

  # Check results data.frame
  expect_s3_class(results$results, "data.frame")
  expect_true(all(c("d1", "d2", "legislator", "auto_cluster") %in% colnames(results$results)))
  expect_equal(nrow(results$results), 6)

  # Check d1 and d2 are numeric
  expect_type(results$results$d1, "double")
  expect_type(results$results$d2, "double")

  # Check metadata
  expect_type(results$metadata, "list")
  expect_equal(results$metadata$principle, "PRINCIPLE 1: Auto-clustering")
  expect_equal(results$metadata$threshold, 0.1)
  expect_equal(results$metadata$n_legislators_analyzed, 6)
  expect_equal(results$metadata$n_votes, 4)
})

test_that("bcall_auto works with custom pivot", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = paste0("Leg_", LETTERS[1:5])
  )

  results <- bcall_auto(rollcall, pivot = "Leg_A", verbose = FALSE)

  expect_equal(results$metadata$pivot, "Leg_A")
  expect_type(results, "list")
})

test_that("bcall_auto works with different distance methods", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = paste0("Leg_", 1:4)
  )

  # Manhattan distance
  results1 <- bcall_auto(rollcall, distance_method = 1, verbose = FALSE)
  expect_equal(results1$metadata$clustering_method, "Manhattan")

  # Euclidean distance
  results2 <- bcall_auto(rollcall, distance_method = 2, verbose = FALSE)
  expect_equal(results2$metadata$clustering_method, "Euclidean")
})

test_that("bcall_auto works with different thresholds", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, NA),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = paste0("Leg_", LETTERS[1:5])
  )

  # Low threshold (include more legislators)
  results1 <- bcall_auto(rollcall, threshold = 0.1, verbose = FALSE)

  # High threshold (stricter)
  results2 <- bcall_auto(rollcall, threshold = 0.5, verbose = FALSE)

  expect_type(results1, "list")
  expect_type(results2, "list")
  expect_equal(results1$metadata$threshold, 0.1)
  expect_equal(results2$metadata$threshold, 0.5)
})

test_that("bcall_auto handles NA values correctly", {
  rollcall <- data.frame(
    vote1 = c(1, -1, NA, -1, 1),
    vote2 = c(NA, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, NA, -1),
    row.names = paste0("Leg_", LETTERS[1:5])
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall_auto handles abstentions (0 values)", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 0, -1, 1),
    vote2 = c(0, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 0, -1),
    row.names = paste0("Leg_", LETTERS[1:5])
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall_auto returns proper cluster distribution", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:6])
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  # Check cluster distribution exists
  expect_true("cluster_distribution" %in% names(results$metadata))
  cluster_dist <- results$metadata$cluster_distribution

  # Should have exactly 2 clusters
  expect_equal(length(cluster_dist), 2)

  # Total should equal number of legislators
  expect_equal(sum(cluster_dist), 6)
})

# ============================================================================
# ERROR HANDLING TESTS
# ============================================================================

test_that("bcall_auto fails with invalid rollcall type", {
  # Not a data.frame
  rollcall <- matrix(c(1, -1, 1, -1), nrow = 2)

  expect_error(
    bcall_auto(rollcall, verbose = FALSE),
    "rollcall must be a data.frame"
  )
})

test_that("bcall_auto fails without rownames", {
  # Missing rownames
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1)
  )

  expect_error(
    bcall_auto(rollcall, verbose = FALSE),
    "rollcall must have rownames"
  )
})

test_that("bcall_auto fails with invalid pivot", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  expect_error(
    bcall_auto(rollcall, pivot = "NonExistent", verbose = FALSE),
    "not found"
  )
})

test_that("bcall_auto fails with invalid distance_method", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  # distance_method should be 1 or 2
  expect_error(
    bcall_auto(rollcall, distance_method = 5, verbose = FALSE)
  )
})

test_that("bcall_auto fails with invalid threshold", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  # Threshold > 1
  expect_error(
    bcall_auto(rollcall, threshold = 1.5, verbose = FALSE),
    "threshold must be between 0 and 1"
  )

  # Threshold < 0
  expect_error(
    bcall_auto(rollcall, threshold = -0.1, verbose = FALSE),
    "threshold must be between 0 and 1"
  )
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("bcall_auto works with minimum viable data", {
  # Minimum: 2 legislators, 1 vote
  rollcall <- data.frame(
    vote1 = c(1, -1),
    row.names = c("Leg_A", "Leg_B")
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall_auto works with many legislators", {
  # Large dataset
  n_legs <- 100
  rollcall <- data.frame(
    vote1 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote2 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote3 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote4 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    row.names = paste0("Leg_", 1:n_legs)
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall_auto works with all same votes (unanimous)", {
  # All legislators vote the same
  rollcall <- data.frame(
    vote1 = c(1, 1, 1, 1),
    vote2 = c(1, 1, 1, 1),
    vote3 = c(-1, -1, -1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  # Should still run but may have warnings
  expect_type(
    suppressWarnings(bcall_auto(rollcall, verbose = FALSE)),
    "list"
  )
})

test_that("bcall_auto verbose mode works", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  # Capture output from verbose mode
  expect_output(
    bcall_auto(rollcall, verbose = TRUE),
    "B-CALL WITH AUTOMATIC CLUSTERING"
  )

  # Silent mode should not produce output
  expect_silent(
    bcall_auto(rollcall, verbose = FALSE)
  )
})
