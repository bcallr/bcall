# Tests for plot functions

test_that("plot_bcall_analysis creates ggplot object", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  result <- bcall_auto(rollcall, verbose = FALSE)
  plot_obj <- plot_bcall_analysis(result)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("plot_bcall_analysis works with bcall results", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("izquierda", "derecha", "izquierda", "derecha"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  result <- bcall(rollcall, clustering, pivot = "Leg_B", verbose = FALSE)
  plot_obj <- plot_bcall_analysis(result)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("plot_bcall_analysis_interactive creates plotly object", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1, 1),
    vote2 = c(1, -1, 1, -1, 1),
    vote3 = c(-1, 1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E")
  )

  result <- bcall_auto(rollcall, verbose = FALSE)
  plot_obj <- plot_bcall_analysis_interactive(result)

  expect_s3_class(plot_obj, "plotly")
})

test_that("plot functions fail with invalid input", {
  expect_error(plot_bcall_analysis(list(wrong = "structure")))
  expect_error(plot_bcall_analysis_interactive(list(wrong = "structure")))
})

test_that("plot_bcall_analysis accepts color_by parameter", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  result <- bcall_auto(rollcall, verbose = FALSE)

  # Should work with auto
  expect_silent(plot_obj <- plot_bcall_analysis(result, color_by = "auto"))
  expect_s3_class(plot_obj, "ggplot")

  # Should work with specific column
  expect_silent(plot_obj2 <- plot_bcall_analysis(result, color_by = "auto_cluster"))
  expect_s3_class(plot_obj2, "ggplot")
})
