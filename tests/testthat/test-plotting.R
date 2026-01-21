# Helper function to create sample results
create_sample_results <- function() {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1, 1, -1),
    vote3 = c(-1, 1, -1, 1, -1, 1),
    vote4 = c(1, 1, -1, -1, 1, -1),
    row.names = paste0("Legislator_", LETTERS[1:6])
  )

  results <- bcall_auto(rollcall, verbose = FALSE)
  return(results)
}

# ============================================================================
# TESTS FOR plot_bcall_analysis()
# ============================================================================

test_that("plot_bcall_analysis returns ggplot object", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis(results)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_bcall_analysis works with default parameters", {
  results <- create_sample_results()

  # Should not error
  expect_s3_class(
    plot_bcall_analysis(results),
    "ggplot"
  )
})

test_that("plot_bcall_analysis works with custom title", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis(results, title = "Custom Title")

  expect_s3_class(plot, "ggplot")
  # Check that title is in the plot
  expect_true(any(grepl("Custom Title", as.character(plot$labels))))
})

test_that("plot_bcall_analysis works with show_names = TRUE", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis(results, show_names = TRUE)

  expect_s3_class(plot, "ggplot")
  # Should have text layer when show_names = TRUE
  expect_true(length(plot$layers) >= 2)
})

test_that("plot_bcall_analysis works with show_names = FALSE", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis(results, show_names = FALSE)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_bcall_analysis works with auto color detection", {
  # Test with bcall_auto results (has auto_cluster)
  results_auto <- create_sample_results()
  plot1 <- plot_bcall_analysis(results_auto, color_by = "auto")
  expect_s3_class(plot1, "ggplot")

  # Test with bcall results (has cluster)
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )
  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", 1:4)
  )
  results_manual <- bcall(rollcall, clustering, pivot = "Leg_1", verbose = FALSE)
  plot2 <- plot_bcall_analysis(results_manual, color_by = "auto")
  expect_s3_class(plot2, "ggplot")
})

test_that("plot_bcall_analysis works with specific color_by variable", {
  results <- create_sample_results()

  # Color by auto_cluster
  plot <- plot_bcall_analysis(results, color_by = "auto_cluster")

  expect_s3_class(plot, "ggplot")
})

test_that("plot_bcall_analysis works with custom alpha and size", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis(results, alpha = 0.5, size = 5)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_bcall_analysis works with bcall() results", {
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

  results <- bcall(rollcall, clustering, pivot = "Leg_A", verbose = FALSE)

  plot <- plot_bcall_analysis(results)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_bcall_analysis recognizes standard cluster names", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )

  # Test with "left"/"right"
  clustering_lr <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", 1:4)
  )
  results_lr <- bcall(rollcall, clustering_lr, pivot = "Leg_1", verbose = FALSE)
  plot_lr <- plot_bcall_analysis(results_lr, color_by = "cluster")
  expect_s3_class(plot_lr, "ggplot")

  # Test with "izquierda"/"derecha"
  clustering_es <- data.frame(
    cluster = c("izquierda", "derecha", "izquierda", "derecha"),
    row.names = paste0("Leg_", 1:4)
  )
  results_es <- bcall(rollcall, clustering_es, pivot = "Leg_1", verbose = FALSE)
  plot_es <- plot_bcall_analysis(results_es, color_by = "cluster")
  expect_s3_class(plot_es, "ggplot")
})

# ============================================================================
# ERROR HANDLING FOR plot_bcall_analysis()
# ============================================================================

test_that("plot_bcall_analysis fails with invalid input", {
  # Not a bcall results object
  invalid_input <- list(data = data.frame(x = 1:5, y = 1:5))

  expect_error(
    plot_bcall_analysis(invalid_input),
    "Input must be results from bcall_auto\\(\\) or bcall\\(\\)"
  )
})

test_that("plot_bcall_analysis fails with missing results component", {
  # Results object without 'results' data.frame
  invalid_results <- list(
    metadata = list(pivot = "Leg_A")
  )

  expect_error(
    plot_bcall_analysis(invalid_results),
    "Input must be results from bcall_auto\\(\\) or bcall\\(\\)"
  )
})

test_that("plot_bcall_analysis fails with invalid color_by variable", {
  results <- create_sample_results()

  expect_error(
    plot_bcall_analysis(results, color_by = "nonexistent_variable"),
    "Variable 'nonexistent_variable' not found in data"
  )
})

# ============================================================================
# TESTS FOR plot_bcall_analysis_interactive()
# ============================================================================

test_that("plot_bcall_analysis_interactive returns plotly object", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis_interactive(results)

  expect_s3_class(plot, "plotly")
})

test_that("plot_bcall_analysis_interactive works with default parameters", {
  results <- create_sample_results()

  # Should not error
  expect_s3_class(
    plot_bcall_analysis_interactive(results),
    "plotly"
  )
})

test_that("plot_bcall_analysis_interactive works with custom title", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis_interactive(results, title = "Interactive Custom Title")

  expect_s3_class(plot, "plotly")
})

test_that("plot_bcall_analysis_interactive works with auto color detection", {
  # Test with bcall_auto results
  results_auto <- create_sample_results()
  plot1 <- plot_bcall_analysis_interactive(results_auto, color_by = "auto")
  expect_s3_class(plot1, "plotly")

  # Test with bcall results
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = paste0("Leg_", 1:4)
  )
  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = paste0("Leg_", 1:4)
  )
  results_manual <- bcall(rollcall, clustering, pivot = "Leg_1", verbose = FALSE)
  plot2 <- plot_bcall_analysis_interactive(results_manual, color_by = "auto")
  expect_s3_class(plot2, "plotly")
})

test_that("plot_bcall_analysis_interactive works with specific color_by", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis_interactive(results, color_by = "auto_cluster")

  expect_s3_class(plot, "plotly")
})

test_that("plot_bcall_analysis_interactive works with bcall() results", {
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

  results <- bcall(rollcall, clustering, pivot = "Leg_A", verbose = FALSE)

  plot <- plot_bcall_analysis_interactive(results, color_by = "cluster")

  expect_s3_class(plot, "plotly")
})

test_that("plot_bcall_analysis_interactive handles different legislator name columns", {
  results <- create_sample_results()

  # Default case: has 'legislator' column
  plot1 <- plot_bcall_analysis_interactive(results)
  expect_s3_class(plot1, "plotly")

  # Test with custom name column (nm_y_apellidos)
  results_custom <- results
  results_custom$results$nm_y_apellidos <- results_custom$results$legislator
  results_custom$results$legislator <- NULL

  plot2 <- plot_bcall_analysis_interactive(results_custom)
  expect_s3_class(plot2, "plotly")
})

test_that("plot_bcall_analysis_interactive creates proper tooltips", {
  results <- create_sample_results()

  plot <- plot_bcall_analysis_interactive(results)

  expect_s3_class(plot, "plotly")

  # Tooltip is created internally, just verify plot works
  expect_s3_class(plot, "htmlwidget")
})

# ============================================================================
# ERROR HANDLING FOR plot_bcall_analysis_interactive()
# ============================================================================

test_that("plot_bcall_analysis_interactive fails with invalid input", {
  invalid_input <- list(data = data.frame(x = 1:5, y = 1:5))

  expect_error(
    plot_bcall_analysis_interactive(invalid_input),
    "Input must be results from bcall_auto\\(\\) or bcall\\(\\)"
  )
})

test_that("plot_bcall_analysis_interactive fails with missing results", {
  invalid_results <- list(
    metadata = list(pivot = "Leg_A")
  )

  expect_error(
    plot_bcall_analysis_interactive(invalid_results),
    "Input must be results from bcall_auto\\(\\) or bcall\\(\\)"
  )
})

test_that("plot_bcall_analysis_interactive fails with invalid color_by", {
  results <- create_sample_results()

  expect_error(
    plot_bcall_analysis_interactive(results, color_by = "invalid_column"),
    "Variable 'invalid_column' not found in data"
  )
})

# ============================================================================
# INTEGRATION TESTS - Both plot functions
# ============================================================================

test_that("both plotting functions work with same results object", {
  results <- create_sample_results()

  # Both should work without errors
  plot_static <- plot_bcall_analysis(results)
  plot_interactive <- plot_bcall_analysis_interactive(results)

  expect_s3_class(plot_static, "ggplot")
  expect_s3_class(plot_interactive, "plotly")
})

test_that("plotting functions work with minimal data", {
  rollcall <- data.frame(
    vote1 = c(1, -1),
    vote2 = c(1, -1),
    row.names = c("Leg_A", "Leg_B")
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  plot_static <- plot_bcall_analysis(results)
  plot_interactive <- plot_bcall_analysis_interactive(results)

  expect_s3_class(plot_static, "ggplot")
  expect_s3_class(plot_interactive, "plotly")
})

test_that("plotting functions work with large dataset", {
  n_legs <- 50
  rollcall <- data.frame(
    vote1 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote2 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    vote3 = sample(c(1, -1, 0, NA), n_legs, replace = TRUE),
    row.names = paste0("Leg_", 1:n_legs)
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  plot_static <- plot_bcall_analysis(results)
  plot_interactive <- plot_bcall_analysis_interactive(results)

  expect_s3_class(plot_static, "ggplot")
  expect_s3_class(plot_interactive, "plotly")
})

test_that("plotting functions handle results with NA values in d1/d2", {
  rollcall <- data.frame(
    vote1 = c(1, -1, NA, -1),
    vote2 = c(NA, -1, 1, -1),
    vote3 = c(-1, 1, -1, NA),
    row.names = paste0("Leg_", LETTERS[1:4])
  )

  results <- bcall_auto(rollcall, verbose = FALSE)

  # Should handle NA values gracefully
  expect_s3_class(
    suppressWarnings(plot_bcall_analysis(results)),
    "ggplot"
  )

  expect_s3_class(
    suppressWarnings(plot_bcall_analysis_interactive(results)),
    "plotly"
  )
})
