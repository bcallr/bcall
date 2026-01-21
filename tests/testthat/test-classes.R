# ============================================================================
# TESTS FOR R6 CLASSES (Clustering and BCall)
# ============================================================================

test_that("Clustering class can be instantiated", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = paste0("Leg_", 1:4)
  )

  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = 1,
    pivot = NULL
  )

  expect_s3_class(clustering_obj, "Clustering")
  expect_s3_class(clustering_obj, "R6")
})

test_that("Clustering object has expected fields", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = 1
  )

  # Should have clustering data.frame
  expect_true("clustering" %in% names(clustering_obj))
  expect_s3_class(clustering_obj$clustering, "data.frame")
})

test_that("Clustering produces two clusters", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:6])
  )

  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = 1
  )

  clusters <- unique(clustering_obj$clustering$cluster)

  # Should have exactly 2 unique clusters
  expect_equal(length(clusters), 2)
})

test_that("Clustering works with Manhattan distance", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = 1  # Manhattan
  )

  expect_s3_class(clustering_obj, "Clustering")
})

test_that("Clustering works with Euclidean distance", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = 2  # Euclidean
  )

  expect_s3_class(clustering_obj, "Clustering")
})

test_that("BCall class can be instantiated", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_A",
    threshold = 0.1
  )

  expect_s3_class(bcall_obj, "BCall")
  expect_s3_class(bcall_obj, "R6")
})

test_that("BCall object has expected fields", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A", "B"),
    row.names = paste0("Leg_", 1:4)
  )

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_1",
    threshold = 0.1
  )

  # Should have stats data.frame
  expect_true("stats" %in% names(bcall_obj))
  expect_s3_class(bcall_obj$stats, "data.frame")
})

test_that("BCall stats contain d1 and d2 columns", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_A",
    threshold = 0.1
  )

  # Check for d1 and d2 columns
  expect_true("d1" %in% colnames(bcall_obj$stats))
  expect_true("d2" %in% colnames(bcall_obj$stats))

  # Check they are numeric
  expect_type(bcall_obj$stats$d1, "double")
  expect_type(bcall_obj$stats$d2, "double")
})

test_that("BCall handles threshold correctly", {
  rollcall <- data.frame(
    vote1 = c(1, -1, NA, -1),
    vote2 = c(1, -1, 1, -1),
    vote3 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering <- data.frame(
    cluster = c("A", "B", "A", "B"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  # Low threshold - should include most legislators
  bcall_obj_low <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_D",
    threshold = 0.1,
    verbose = FALSE
  )

  # Medium threshold - should filter out some
  bcall_obj_high <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_D",
    threshold = 0.5,
    verbose = FALSE
  )

  # High threshold should result in fewer or equal legislators
  expect_true(nrow(bcall_obj_high$stats) <= nrow(bcall_obj_low$stats))
})

test_that("BCall respects pivot legislator", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  # Create with pivot
  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_B",
    threshold = 0.1
  )

  # Pivot should be in results
  expect_true("Leg_B" %in% rownames(bcall_obj$stats))
})

test_that("Clustering and BCall integration works", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    vote4 = c(1, 1, -1, -1, 1, -1),
    row.names = paste0("Legislator_", LETTERS[1:6])
  )

  # Step 1: Create clustering
  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = 1
  )

  clustering_df <- clustering_obj$clustering

  # Step 2: Use clustering in BCall
  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering_df,
    pivot = rownames(clustering_df)[1],
    threshold = 0.1
  )

  # Both should be valid objects
  expect_s3_class(clustering_obj, "Clustering")
  expect_s3_class(bcall_obj, "BCall")

  # BCall stats should have results
  expect_s3_class(bcall_obj$stats, "data.frame")
  expect_true(nrow(bcall_obj$stats) > 0)
})

test_that("R6 classes handle edge cases", {
  # Minimum viable data (need at least 2 votes)
  rollcall_min <- data.frame(
    vote1 = c(1, -1),
    vote2 = c(-1, 1),
    row.names = c("Leg_A", "Leg_B")
  )

  clustering_min <- data.frame(
    cluster = c("A", "B"),
    row.names = c("Leg_A", "Leg_B")
  )

  # Should work with minimal data
  clustering_obj <- Clustering$new(rollcalls = rollcall_min, N = 1)
  expect_s3_class(clustering_obj, "Clustering")

  bcall_obj <- BCall$new(
    rollcall = rollcall_min,
    clustering = clustering_min,
    pivot = "Leg_A",
    threshold = 0.1,
    verbose = FALSE
  )
  expect_s3_class(bcall_obj, "BCall")
})

test_that("R6 classes handle NA values properly", {
  rollcall_na <- data.frame(
    vote1 = c(1, -1, NA, -1),
    vote2 = c(NA, -1, 1, -1),
    vote3 = c(-1, 1, -1, NA),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering_na <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  # Should handle NA gracefully
  clustering_obj <- Clustering$new(rollcalls = rollcall_na, N = 1)
  expect_s3_class(clustering_obj, "Clustering")

  bcall_obj <- BCall$new(
    rollcall = rollcall_na,
    clustering = clustering_na,
    pivot = "Leg_A",
    threshold = 0.1
  )
  expect_s3_class(bcall_obj, "BCall")
})

test_that("R6 classes handle abstentions (zeros)", {
  rollcall_abs <- data.frame(
    vote1 = c(1, -1, 0, -1),
    vote2 = c(0, -1, 1, -1),
    vote3 = c(-1, 1, -1, 0),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  clustering_abs <- data.frame(
    cluster = c("A", "B", "A", "B"),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  # Should handle zeros (abstentions) gracefully
  clustering_obj <- Clustering$new(rollcalls = rollcall_abs, N = 1)
  expect_s3_class(clustering_obj, "Clustering")

  bcall_obj <- BCall$new(
    rollcall = rollcall_abs,
    clustering = clustering_abs,
    pivot = "Leg_A",
    threshold = 0.1
  )
  expect_s3_class(bcall_obj, "BCall")
})
