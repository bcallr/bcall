test_that("bcall works with valid input", {
  # Create sample rollcall data
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E", "Leg_F")
  )

  # Create clustering
  clustering <- data.frame(
    cluster = c("left", "right", "left", "right", "left", "right"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E", "Leg_F")
  )

  # Run analysis (suppress BCall class output)
  results <- bcall(rollcall, clustering, pivot = "Leg_B",verbose = FALSE)

  # Check structure
  expect_type(results, "list")
  expect_named(results, c("results", "bcall_object", "metadata"))

  # Check results data.frame
  expect_s3_class(results$results, "data.frame")
  expect_true(all(c("d1", "d2", "legislator", "cluster") %in% colnames(results$results)))
  expect_equal(nrow(results$results), 6)

  # Check d1 and d2 are numeric
  expect_type(results$results$d1, "double")
  expect_type(results$results$d2, "double")

  # Check metadata
  expect_type(results$metadata, "list")
  expect_equal(results$metadata$principle, "PRINCIPLE 2: User-provided clustering")
  expect_equal(results$metadata$pivot, "Leg_B")
  expect_equal(results$metadata$threshold, 0.1)
})

test_that("bcall works with clustering as named vector", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  # Clustering as named vector
  clustering <- c(Leg_A = "A", Leg_B = "B", Leg_C = "A", Leg_D = "B")

  results <- bcall(rollcall, clustering, pivot = "Leg_B",verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall works with different cluster labels", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  # Test different cluster naming schemes
  test_cases <- list(
    c("Coalition", "Opposition", "Coalition", "Opposition"),
    c(1, 2, 1, 2),
    c("A", "B", "A", "B"),
    c("izquierda", "derecha", "izquierda", "derecha")
  )

  for (cluster_values in test_cases) {
    clustering <- data.frame(
      cluster = cluster_values,
      row.names = paste0("Leg_", 1:4)
    )

    results <- bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE)
    expect_type(results, "list")
    expect_s3_class(results$results, "data.frame")
  }
})

test_that("bcall works with different thresholds", {
  rollcall <- data.frame(
    vote1 = c(1, -1, NA, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(1, -1, 1, NA),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  # Low threshold
  results1 <- bcall(rollcall, clustering, pivot = "Leg_A", threshold = 0.1,verbose = FALSE)

  # High threshold
  results2 <- bcall(rollcall, clustering, pivot = "Leg_A", threshold = 0.5,verbose = FALSE)

  expect_equal(results1$metadata$threshold, 0.1)
  expect_equal(results2$metadata$threshold, 0.5)
})

test_that("bcall handles NA values correctly", {
  rollcall <- data.frame(
    vote1 = c(1, -1, NA, -1),
    vote2 = c(NA, -1, 1, -1),
    vote3 = c(-1, 1, -1, NA),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A", "B"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  results <- bcall(rollcall, clustering, pivot = "Leg_A",verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall handles abstentions (0 values)", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 0, -1),
    vote2 = c(0, -1, 1, -1),
    vote3 = c(-1, 1, -1, 0),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  results <- bcall(rollcall, clustering, pivot = "Leg_A",verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall handles partial overlap between rollcall and clustering", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:5])
  )

  # Clustering only includes 3 legislators
  clustering <- data.frame(
    cluster = c("left", "right", "left"),
    row.names = paste0("Leg_", LETTERS[1:3])
  )

  # Should work with only the common legislators
  results <- bcall(rollcall, clustering, pivot = "Leg_A",verbose = FALSE)

  expect_type(results, "list")
  expect_equal(nrow(results$results), 3)  # Only 3 legislators in common
})

test_that("bcall preserves cluster information in results", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  clustering <- data.frame(
    cluster = c("Coalition", "Opposition", "Coalition", "Opposition"),
    row.names = paste0("Leg_", 1:4)
  )

  results <- bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE)

  # Check that cluster column exists and has correct values
  expect_true("cluster" %in% colnames(results$results))
  expect_equal(sort(unique(results$results$cluster)), c("Coalition", "Opposition"))
})

# ============================================================================
# ERROR HANDLING TESTS
# ============================================================================

test_that("bcall fails with invalid rollcall type", {
  rollcall <- matrix(c(1, -1, 1, -1), nrow = 2)
  clustering <- data.frame(
    cluster = c("A", "B"),
    row.names = c("Leg_1", "Leg_2")
  )

  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE),
    "rollcall must be a data.frame"
  )
})

test_that("bcall fails without rollcall rownames", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = paste0("Leg_", 1:3)
  )

  # Pivot validation happens before rownames validation
  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE)
  )
})

test_that("bcall fails with invalid clustering type", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  # Invalid clustering (not data.frame or vector)
  clustering <- list(c("A", "B", "A"))

  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE)
  )
})

test_that("bcall fails with multi-column clustering", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  # Clustering with 2 columns
  clustering <- data.frame(
    cluster1 = c("A", "B", "A"),
    cluster2 = c("X", "Y", "X"),
    row.names = paste0("Leg_", 1:3)
  )

  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE),
    "clustering must have exactly one column"
  )
})

test_that("bcall fails without clustering rownames", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A")
    # No rownames
  )

  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE)
  )
})

test_that("bcall fails with wrong number of clusters", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, 0),
    vote2 = c(1, -1, 1, 0),
    row.names = paste0("Leg_", 1:4)
  )

  # 3 clusters instead of 2
  clustering <- data.frame(
    cluster = c("A", "B", "C", "A"),
    row.names = paste0("Leg_", 1:4)
  )

  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE),
    "clustering must have exactly 2 unique values"
  )

  # Only 1 cluster
  clustering_one <- data.frame(
    cluster = c("A", "A", "A", "A"),
    row.names = paste0("Leg_", 1:4)
  )

  expect_error(
    bcall(rollcall, clustering_one, pivot = "Leg_1",verbose = FALSE),
    "clustering must have exactly 2 unique values"
  )
})

test_that("bcall fails without pivot", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = paste0("Leg_", 1:3)
  )

  expect_error(
    bcall(rollcall, clustering,verbose = FALSE),
    "pivot must be specified"
  )
})

test_that("bcall fails with invalid pivot", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = paste0("Leg_", 1:3)
  )

  # Pivot not in rollcall
  expect_error(
    bcall(rollcall, clustering, pivot = "NonExistent",verbose = FALSE),
    "not found in rollcall"
  )

  # Pivot not in clustering
  rollcall2 <- data.frame(
    vote1 = c(1, -1, 1, 1),
    vote2 = c(1, -1, 1, 1),
    row.names = c("Leg_1", "Leg_2", "Leg_3", "Leg_4")
  )

  expect_error(
    bcall(rollcall2, clustering, pivot = "Leg_4",verbose = FALSE),
    "not found in clustering"
  )
})

test_that("bcall fails with no matching legislators", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C")
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = c("Leg_X", "Leg_Y", "Leg_Z")  # Different names
  )

  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_X",verbose = FALSE),
    "No matching legislators|not found"
  )
})

test_that("bcall fails with invalid threshold", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = paste0("Leg_", 1:3)
  )

  # Threshold > 1
  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1", threshold = 1.5, verbose = FALSE),
    "threshold must be between 0 and 1"
  )

  # Threshold < 0
  expect_error(
    bcall(rollcall, clustering, pivot = "Leg_1", threshold = -0.1, verbose = FALSE),
    "threshold must be between 0 and 1"
  )
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("bcall works with minimum viable data", {
  rollcall <- data.frame(
    vote1 = c(1, -1),
    vote2 = c(-1, 1),
    row.names = c("Leg_A", "Leg_B")
  )

  clustering <- data.frame(
    cluster = c("left", "right"),
    row.names = c("Leg_A", "Leg_B")
  )

  results <- bcall(rollcall, clustering, pivot = "Leg_A", verbose = FALSE)

  expect_type(results, "list")
  expect_equal(nrow(results$results), 2)
})

test_that("bcall works with large dataset", {
  n_legs <- 100
  rollcall <- data.frame(
    vote1 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote2 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote3 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    row.names = paste0("Leg_", 1:n_legs)
  )

  clustering <- data.frame(
    cluster = sample(c("A", "B"), n_legs, replace = TRUE),
    row.names = paste0("Leg_", 1:n_legs)
  )

  results <- bcall(rollcall, clustering, pivot = "Leg_1",verbose = FALSE)

  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("bcall verbose mode works", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    row.names = paste0("Leg_", 1:3)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A"),
    row.names = paste0("Leg_", 1:3)
  )

  # Verbose should print output
  expect_output(
    bcall(rollcall, clustering, pivot = "Leg_1", verbose = TRUE),
    "B-CALL WITH USER-PROVIDED CLUSTERING"
  )
})

test_that("bcall returns correct metadata", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering <- data.frame(
    cluster = c("Coalition", "Opposition", "Coalition", "Opposition"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  results <- bcall(rollcall, clustering, pivot = "Leg_B", threshold = 0.2,verbose = FALSE)

  # Check all expected metadata fields
  expect_true("principle" %in% names(results$metadata))
  expect_true("pivot" %in% names(results$metadata))
  expect_true("pivot_cluster" %in% names(results$metadata))
  expect_true("threshold" %in% names(results$metadata))
  expect_true("n_legislators_analyzed" %in% names(results$metadata))
  expect_true("n_votes" %in% names(results$metadata))
  expect_true("cluster_distribution" %in% names(results$metadata))

  # Check specific values
  expect_equal(results$metadata$pivot, "Leg_B")
  expect_equal(results$metadata$pivot_cluster, "Opposition")
  expect_equal(results$metadata$threshold, 0.2)
  expect_equal(results$metadata$n_votes, 3)
})
