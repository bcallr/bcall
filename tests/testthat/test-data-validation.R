# ============================================================================
# DATA VALIDATION AND INPUT CHECKING TESTS
# ============================================================================

test_that("rollcall data validates vote values correctly", {
  # Valid vote values: 1, -1, 0, NA
  rollcall_valid <- data.frame(
    vote1 = c(1, -1, 0, NA),
    vote2 = c(1, -1, 0, NA),
    row.names = paste0("Leg_", 1:4)
  )

  results <- bcall_auto(rollcall_valid, verbose = FALSE)
  expect_type(results, "list")
})

test_that("rollcall handles mixed vote patterns", {
  rollcall <- data.frame(
    unanimous_yes = c(1, 1, 1, 1),
    unanimous_no = c(-1, -1, -1, -1),
    split_vote = c(1, -1, 1, -1),
    with_abstentions = c(1, 0, -1, 0),
    with_absences = c(1, NA, -1, NA),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  results <- bcall_auto(rollcall, verbose = FALSE)
  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
})

test_that("rollcall handles mostly NA column", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(NA, NA, NA, 1),  # Mostly absent
    vote3 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  # Should still work
  results <- bcall_auto(rollcall, verbose = FALSE)
  expect_type(results, "list")
})

test_that("rollcall handles all NA row (legislator)", {
  rollcall <- data.frame(
    vote1 = c(1, -1, NA, -1),
    vote2 = c(1, -1, NA, -1),
    vote3 = c(1, -1, NA, -1),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  # With high threshold, Leg_C should be filtered out
  results <- bcall_auto(rollcall, threshold = 0.5, verbose = FALSE)
  expect_type(results, "list")

  # Leg_C should not be in results (0% participation)
  expect_false("Leg_C" %in% results$results$legislator)
})

test_that("clustering validates correct number of unique values", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  # Valid: exactly 2 clusters
  clustering_valid <- data.frame(
    cluster = c("A", "B", "A", "B"),
    row.names = paste0("Leg_", 1:4)
  )

  results <- bcall(rollcall, clustering_valid, pivot = "Leg_1", verbose = FALSE)
  expect_type(results, "list")

  # Invalid: 3 clusters
  clustering_invalid_3 <- data.frame(
    cluster = c("A", "B", "C", "A"),
    row.names = paste0("Leg_", 1:4)
  )

  expect_error(
    bcall(rollcall, clustering_invalid_3, pivot = "Leg_1", verbose = FALSE),
    "exactly 2 unique values"
  )

  # Invalid: only 1 cluster
  clustering_invalid_1 <- data.frame(
    cluster = c("A", "A", "A", "A"),
    row.names = paste0("Leg_", 1:4)
  )

  expect_error(
    bcall(rollcall, clustering_invalid_1, pivot = "Leg_1", verbose = FALSE),
    "exactly 2 unique values"
  )
})

test_that("threshold parameter validation works", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1),
    vote2 = c(1, -1, 1),
    vote3 = c(-1, 1, -1),
    row.names = paste0("Leg_", 1:3)
  )

  # Valid thresholds (excluding 1.0 which would filter everyone)
  valid_thresholds <- c(0, 0.1, 0.5, 0.9)

  for (thresh in valid_thresholds) {
    results <- bcall_auto(rollcall, threshold = thresh, verbose = FALSE)
    expect_type(results, "list")
  }

  # Invalid thresholds (outside 0-1 range)
  expect_error(bcall_auto(rollcall, threshold = -0.1, verbose = FALSE))
  expect_error(bcall_auto(rollcall, threshold = 1.5, verbose = FALSE))
})

test_that("rownames are preserved throughout analysis", {
  legislator_names <- c("Alice", "Bob", "Charlie", "Diana")

  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = legislator_names
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  # All original names should be in results
  expect_true(all(legislator_names %in% results$results$legislator))
})

test_that("special characters in legislator names are handled", {
  special_names <- c(
    "María García",
    "José O'Brien",
    "François-Pierre",
    "Müller, Hans"
  )

  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = special_names
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  # Names should be preserved
  expect_true(all(special_names %in% results$results$legislator))
})

test_that("numeric rownames are handled correctly", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("1", "2", "3", "4")
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  expect_type(results, "list")
  expect_true(all(c("1", "2", "3", "4") %in% results$results$legislator))
})

test_that("duplicate rownames are not allowed", {
  # data.frame won't allow duplicate rownames by default
  # This test ensures we rely on that behavior

  # Attempting to create a data.frame with duplicate rownames will fail
  expect_error({
    rollcall <- data.frame(
      vote1 = c(1, -1, 1),
      vote2 = c(1, -1, 1),
      row.names = c("Leg_A", "Leg_A", "Leg_B")  # Duplicate
    )
  })
})

# ============================================================================
# INTEGRATION TESTS - FULL WORKFLOW
# ============================================================================

test_that("full workflow: bcall_auto + plotting works", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    vote4 = c(1, 1, -1, -1, 1, -1),
    row.names = paste0("Legislator_", LETTERS[1:6])
  )

  # Run analysis
  results <- bcall_auto(rollcall, verbose = FALSE)

  # Create plots
  plot_static <- plot_bcall_analysis(results)
  plot_interactive <- plot_bcall_analysis_interactive(results)

  # All should succeed
  expect_type(results, "list")
  expect_s3_class(plot_static, "ggplot")
  expect_s3_class(plot_interactive, "plotly")
})

test_that("full workflow: bcall + plotting works", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("Coalition", "Opposition", "Coalition", "Opposition"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  # Run analysis
  results <- bcall(rollcall, clustering, pivot = "Leg_B", verbose = FALSE)

  # Create plots
  plot_static <- plot_bcall_analysis(results, color_by = "cluster")
  plot_interactive <- plot_bcall_analysis_interactive(results, color_by = "cluster")

  # All should succeed
  expect_type(results, "list")
  expect_s3_class(plot_static, "ggplot")
  expect_s3_class(plot_interactive, "plotly")
})

test_that("results structure is consistent between bcall_auto and bcall", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  # bcall_auto results
  results_auto <- bcall_auto(rollcall, verbose = FALSE)

  # bcall results
  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )
  results_manual <- bcall(rollcall, clustering, pivot = "Leg_A", verbose = FALSE)

  # Both should have similar structure
  expect_true("results" %in% names(results_auto))
  expect_true("results" %in% names(results_manual))

  expect_true("metadata" %in% names(results_auto))
  expect_true("metadata" %in% names(results_manual))

  expect_true("bcall_object" %in% names(results_auto))
  expect_true("bcall_object" %in% names(results_manual))

  # Both results should have d1 and d2
  expect_true(all(c("d1", "d2") %in% colnames(results_auto$results)))
  expect_true(all(c("d1", "d2") %in% colnames(results_manual$results)))
})

test_that("consistent results with same data and parameters", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:6])
  )

  # Run same analysis twice
  results1 <- bcall_auto(rollcall, distance_method = 1, pivot = "Leg_A",
                         threshold = 0.1, verbose = FALSE)
  results2 <- bcall_auto(rollcall, distance_method = 1, pivot = "Leg_A",
                         threshold = 0.1, verbose = FALSE)

  # Should produce identical results
  expect_equal(results1$results$d1, results2$results$d1)
  expect_equal(results1$results$d2, results2$results$d2)
  expect_equal(results1$results$auto_cluster, results2$results$auto_cluster)
})

test_that("metadata contains all expected fields", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  results_auto <- bcall_auto(rollcall, verbose = FALSE)

  # Check metadata fields for bcall_auto
  expected_fields_auto <- c(
    "principle", "clustering_method", "distance_method", "pivot",
    "pivot_participation", "threshold", "n_legislators_original",
    "n_legislators_analyzed", "n_votes", "cluster_distribution", "date_analyzed"
  )

  expect_true(all(expected_fields_auto %in% names(results_auto$metadata)))

  # Check metadata for bcall
  clustering <- data.frame(
    cluster = c("A", "B", "A", "B"),
    row.names = paste0("Leg_", 1:4)
  )

  results_manual <- bcall(rollcall, clustering, pivot = "Leg_1", verbose = FALSE)

  expected_fields_manual <- c(
    "principle", "pivot", "pivot_cluster", "pivot_participation",
    "threshold", "n_legislators_original", "n_legislators_analyzed",
    "n_votes", "cluster_distribution", "date_analyzed"
  )

  expect_true(all(expected_fields_manual %in% names(results_manual$metadata)))
})

test_that("analysis works with real-world style data patterns", {
  # Simulate real legislative data with:
  # - Mix of partisan and bipartisan votes
  # - Some legislators miss many votes
  # - Some abstentions

  set.seed(123)  # For reproducibility

  n_legs <- 30
  n_votes <- 50

  # Create semi-realistic voting patterns
  rollcall <- as.data.frame(
    matrix(
      sample(c(1, -1, 0, NA), n_legs * n_votes,
             replace = TRUE,
             prob = c(0.4, 0.4, 0.1, 0.1)),
      nrow = n_legs,
      ncol = n_votes,
      dimnames = list(
        paste0("Legislator_", 1:n_legs),
        paste0("Vote_", 1:n_votes)
      )
    )
  )

  # Run analysis
  results <- bcall_auto(rollcall, verbose = FALSE)

  # Should produce valid results
  expect_type(results, "list")
  expect_s3_class(results$results, "data.frame")
  expect_true(nrow(results$results) > 0)
  expect_true(all(c("d1", "d2") %in% colnames(results$results)))

  # Plots should work
  expect_s3_class(plot_bcall_analysis(results), "ggplot")
  expect_s3_class(plot_bcall_analysis_interactive(results), "plotly")
})
