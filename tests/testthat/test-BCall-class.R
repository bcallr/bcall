# Tests for BCall R6 class

test_that("BCall class initializes correctly", {
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, -1),
    vote2 = c(1, -1, 1, -1),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_B",
    threshold = 0.1
  )

  expect_s3_class(bcall_obj, "BCall")
  expect_true(!is.null(bcall_obj$stats))
})

test_that("BCall filters by participation threshold", {
  # Create rollcall with one legislator with low participation
  rollcall <- data.frame(
    vote1 = c(1, -1, 1, NA),
    vote2 = c(1, -1, 1, NA),
    vote3 = c(1, -1, 1, NA),
    vote4 = c(1, -1, 1, NA),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  # With high threshold, Leg_D should be excluded (0% participation)
  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_B",
    threshold = 0.5
  )

  expect_true(nrow(bcall_obj$stats) < 4)
})

test_that("BCall filters votes with zero variance", {
  # Create rollcall with unanimous vote
  rollcall <- data.frame(
    vote1 = c(1, 1, 1, 1),  # Unanimous - should be filtered
    vote2 = c(1, -1, 1, -1),  # Divided - should be kept
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  clustering <- data.frame(
    cluster = c("left", "right", "left", "right"),
    row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D")
  )

  expect_output(
    bcall_obj <- BCall$new(
      rollcall = rollcall,
      clustering = clustering,
      pivot = "Leg_B",
      threshold = 0.1
    ),
    "Filtered.*votes with zero variance"
  )
})

test_that("BCall computes d1 and d2", {
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

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = "Leg_B",
    threshold = 0.1
  )

  expect_true("d1" %in% colnames(bcall_obj$stats))
  expect_true("d2" %in% colnames(bcall_obj$stats))
  expect_true(all(is.numeric(bcall_obj$stats$d1)))
  expect_true(all(is.numeric(bcall_obj$stats$d2)))
})
