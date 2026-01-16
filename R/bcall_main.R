#' @title Main B-Call Functions
#' @description Core functions for B-Call analysis with legislative voting data
#' @author Daniel Alcatruz
#' @import R6
#' @export

# ==============================================================================
# PRINCIPLE 1: bcall_auto() - Automatic Clustering
# ==============================================================================

#' B-Call Analysis with Automatic Clustering
#'
#' @description
#' PRINCIPLE 1: User provides only rollcall data.
#' This function automatically clusters legislators into two groups and runs B-Call analysis.
#'
#' @param rollcall data.frame with rollcall voting data (legislators as rows, votes as columns)
#'   Values: 1 (Yes), -1 (No), 0 (Abstention), NA (Absent)
#' @param distance_method Integer, distance metric for clustering (1 = Manhattan, 2 = Euclidean)
#' @param pivot Character, optional pivot legislator name (NULL for automatic selection from 'right' cluster)
#' @param threshold Numeric, minimum participation threshold (0.0-1.0)
#' @param verbose Logical, whether to print progress messages
#'
#' @return List with:
#'   - results: data.frame with d1, d2, auto_cluster for each legislator
#'   - bcall_object: BCall R6 object
#'   - clustering_object: Clustering R6 object
#'   - metadata: Analysis metadata
#'
#' @examples
#' # Create sample rollcall data
#' rollcall <- data.frame(
#'   vote1 = c(1, -1, 1, -1, 1, -1),
#'   vote2 = c(1, -1, 1, -1, 1, -1),
#'   vote3 = c(-1, 1, -1, 1, -1, 1),
#'   vote4 = c(1, 1, -1, -1, 1, -1),
#'   row.names = paste0("Legislator_", LETTERS[1:6])
#' )
#'
#' # Run automatic clustering analysis
#' results <- bcall_auto(rollcall, distance_method = 1, threshold = 0.1, verbose = FALSE)
#'
#' # View results
#' head(results$results)
#'
#' # Visualize
#' \donttest{
#' plot_bcall_analysis(results)
#' plot_bcall_analysis_interactive(results)
#' }
#'
#' @export
bcall_auto <- function(rollcall,
                       distance_method = 1,
                       pivot = NULL,
                       threshold = 0.1,
                       verbose = TRUE) {

  if (verbose) cat("=== B-CALL WITH AUTOMATIC CLUSTERING ===\n\n")

  # Validate rollcall
  if (!is.data.frame(rollcall)) {
    stop("rollcall must be a data.frame")
  }

  if (is.null(rownames(rollcall))) {
    stop("rollcall must have rownames (legislator names)")
  }

  if (verbose) {
    cat("Input validation:\n")
    cat("  \u2713 Rollcall: ", nrow(rollcall), " legislators \u00d7 ", ncol(rollcall), " votes\n", sep = "")
    cat("  \u2713 Distance method: ", distance_method, " (",
        ifelse(distance_method == 1, "Manhattan", "Euclidean"), ")\n", sep = "")
    cat("  \u2713 Threshold: ", threshold * 100, "%\n\n", sep = "")
  }

  # Step 1: Automatic Clustering
  if (verbose) cat("Step 1: Running automatic clustering...\n")

  clustering_obj <- Clustering$new(
    rollcalls = rollcall,
    N = distance_method,
    pivot = pivot
  )

  clustering_df <- clustering_obj$clustering

  if (verbose) {
    cat("  \u2713 Clustering completed\n")
    cat("  - Distribution:\n")
    cluster_table <- table(clustering_df$cluster)
    for (cluster_name in names(cluster_table)) {
      cat("    * ", cluster_name, ": ", cluster_table[cluster_name], " legislators\n", sep = "")
    }
    cat("\n")
  }

  # Step 2: Auto-select pivot if not provided
  if (is.null(pivot)) {
    right_legislators <- rownames(clustering_df)[clustering_df$cluster == "right"]

    if (length(right_legislators) > 0) {
      # Select legislator with highest participation from right cluster
      participation <- 1 - rowSums(is.na(rollcall[right_legislators, , drop = FALSE])) / ncol(rollcall)
      pivot <- names(participation)[which.max(participation)]

      if (verbose) {
        cat("Step 2: Auto-selecting pivot...\n")
        cat("  \u2713 Pivot: ", pivot, " (", round(participation[pivot] * 100, 1), "% participation)\n\n", sep = "")
      }
    } else {
      stop("No legislators found in 'right' cluster for pivot selection")
    }
  }

  # Step 3: Run B-Call analysis
  if (verbose) cat("Step 3: Running B-Call analysis...\n")

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering_df,
    pivot = pivot,
    threshold = threshold
  )

  if (verbose) cat("  \u2713 B-Call analysis completed\n\n")

  # Step 4: Prepare results
  results_df <- bcall_obj$stats
  results_df$legislator <- rownames(results_df)
  results_df$auto_cluster <- clustering_df[rownames(results_df), "cluster"]

  # Calculate participation for metadata
  participation_all <- 1 - rowSums(is.na(rollcall)) / ncol(rollcall)

  # Metadata
  metadata <- list(
    principle = "PRINCIPLE 1: Auto-clustering",
    clustering_method = ifelse(distance_method == 1, "Manhattan", "Euclidean"),
    distance_method = distance_method,
    pivot = pivot,
    pivot_participation = sprintf("%.1f%%", participation_all[pivot] * 100),
    threshold = threshold,
    n_legislators_original = nrow(rollcall),
    n_legislators_analyzed = nrow(results_df),
    n_votes = ncol(rollcall),
    cluster_distribution = table(results_df$auto_cluster),
    date_analyzed = Sys.Date()
  )

  if (verbose) {
    cat("Results summary:\n")
    cat("  - d1 range: [", round(min(results_df$d1, na.rm = TRUE), 3),
        ", ", round(max(results_df$d1, na.rm = TRUE), 3), "]\n", sep = "")
    cat("  - d2 range: [", round(min(results_df$d2, na.rm = TRUE), 3),
        ", ", round(max(results_df$d2, na.rm = TRUE), 3), "]\n", sep = "")
    cat("  - Legislators analyzed: ", nrow(results_df), "\n\n", sep = "")
  }

  # Return structure
  result <- list(
    results = results_df,
    bcall_object = bcall_obj,
    clustering_object = clustering_obj,
    metadata = metadata
  )

  if (verbose) {
    cat("\u2713 Analysis complete!\n")
    cat("  Use plot_bcall_analysis_interactive(results) to visualize\n\n")
  }

  return(result)
}


# ==============================================================================
# PRINCIPLE 2: bcall() - User-Provided Clustering
# ==============================================================================

#' B-Call Analysis with User-Provided Clustering
#'
#' @description
#' PRINCIPLE 2: User provides rollcall data AND clustering assignment.
#' This function runs B-Call analysis using the user's clustering.
#'
#' @param rollcall data.frame with rollcall voting data (legislators as rows, votes as columns)
#'   Values: 1 (Yes), -1 (No), 0 (Abstention), NA (Absent)
#' @param clustering data.frame with one column OR named vector with exactly 2 unique values
#' @param pivot Character, pivot legislator name
#' @param threshold Numeric, minimum participation threshold (0.0-1.0)
#' @param verbose Logical, whether to print progress messages
#'
#' @return List with:
#'   - results: data.frame with d1, d2, cluster for each legislator
#'   - bcall_object: BCall R6 object
#'   - metadata: Analysis metadata
#'
#' @examples
#' # Create sample rollcall data
#' rollcall <- data.frame(
#'   vote1 = c(1, -1, 1, -1, 1, -1),
#'   vote2 = c(1, -1, 1, -1, 1, -1),
#'   vote3 = c(-1, 1, -1, 1, -1, 1),
#'   row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E", "Leg_F")
#' )
#'
#' # Create user-defined clustering (can use any 2 values)
#' clustering <- data.frame(
#'   cluster = c("left", "right", "left", "right", "left", "right"),
#'   row.names = c("Leg_A", "Leg_B", "Leg_C", "Leg_D", "Leg_E", "Leg_F")
#' )
#'
#' # Run analysis with user clustering
#' results <- bcall(rollcall, clustering, pivot = "Leg_B", threshold = 0.1, verbose = FALSE)
#'
#' # View results
#' head(results$results)
#'
#' # Visualize
#' \donttest{
#' plot_bcall_analysis(results)
#' plot_bcall_analysis_interactive(results)
#' }
#'
#' @export
bcall <- function(rollcall,
                  clustering,
                  pivot,
                  threshold = 0.1,
                  verbose = TRUE) {

  if (verbose) cat("=== B-CALL WITH USER-PROVIDED CLUSTERING ===\n\n")

  # Validate rollcall
  if (!is.data.frame(rollcall)) {
    stop("rollcall must be a data.frame")
  }

  if (is.null(rownames(rollcall))) {
    stop("rollcall must have rownames (legislator names)")
  }

  # Validate clustering
  if (is.vector(clustering)) {
    # Convert named vector to data.frame
    clustering <- data.frame(cluster = clustering, row.names = names(clustering))
  }

  if (!is.data.frame(clustering)) {
    stop("clustering must be a data.frame or named vector")
  }

  if (ncol(clustering) != 1) {
    stop("clustering must have exactly one column")
  }

  if (is.null(rownames(clustering))) {
    stop("clustering must have rownames (legislator names)")
  }

  # Check that clustering has exactly 2 unique values
  unique_clusters <- unique(clustering[, 1])
  if (length(unique_clusters) != 2) {
    stop("clustering must have exactly 2 unique values (current: ", length(unique_clusters), ")")
  }

  # Validate pivot
  if (missing(pivot) || is.null(pivot)) {
    stop("pivot must be specified for user-provided clustering")
  }

  if (!pivot %in% rownames(rollcall)) {
    stop("pivot '", pivot, "' not found in rollcall rownames")
  }

  if (!pivot %in% rownames(clustering)) {
    stop("pivot '", pivot, "' not found in clustering rownames")
  }

  # Get pivot cluster (any value is OK, pivot defines which is which)
  pivot_cluster <- clustering[pivot, 1]

  if (verbose) {
    cat("Input validation:\n")
    cat("  \u2713 Rollcall: ", nrow(rollcall), " legislators \u00d7 ", ncol(rollcall), " votes\n", sep = "")
    cat("  \u2713 Clustering: ", nrow(clustering), " legislators\n", sep = "")
    cat("  \u2713 Pivot: ", pivot, " (cluster: ", pivot_cluster, ")\n", sep = "")
    cat("  \u2713 Threshold: ", threshold * 100, "%\n\n", sep = "")

    cat("Cluster distribution:\n")
    cluster_table <- table(clustering[, 1])
    for (cluster_name in names(cluster_table)) {
      cat("  - ", cluster_name, ": ", cluster_table[cluster_name], " legislators\n", sep = "")
    }
    cat("\n")
  }

  # Ensure matching legislators
  common_legislators <- intersect(rownames(rollcall), rownames(clustering))

  if (length(common_legislators) == 0) {
    stop("No matching legislators between rollcall and clustering")
  }

  if (length(common_legislators) < nrow(rollcall)) {
    if (verbose) {
      cat("Note: ", nrow(rollcall) - length(common_legislators),
          " legislators in rollcall not found in clustering (will be excluded)\n\n", sep = "")
    }
  }

  # Filter to common legislators
  rollcall <- rollcall[common_legislators, ]
  clustering <- clustering[common_legislators, , drop = FALSE]

  # Run B-Call analysis
  if (verbose) cat("Running B-Call analysis...\n")

  bcall_obj <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = pivot,
    threshold = threshold
  )

  if (verbose) cat("  \u2713 B-Call analysis completed\n\n")

  # Prepare results
  results_df <- bcall_obj$stats
  results_df$legislator <- rownames(results_df)
  results_df$cluster <- clustering[rownames(results_df), 1]

  # Calculate participation for metadata
  participation_all <- 1 - rowSums(is.na(rollcall)) / ncol(rollcall)

  # Metadata
  metadata <- list(
    principle = "PRINCIPLE 2: User-provided clustering",
    pivot = pivot,
    pivot_cluster = pivot_cluster,
    pivot_participation = sprintf("%.1f%%", participation_all[pivot] * 100),
    threshold = threshold,
    n_legislators_original = length(common_legislators),
    n_legislators_analyzed = nrow(results_df),
    n_votes = ncol(rollcall),
    cluster_distribution = table(results_df$cluster),
    date_analyzed = Sys.Date()
  )

  if (verbose) {
    cat("Results summary:\n")
    cat("  - d1 range: [", round(min(results_df$d1, na.rm = TRUE), 3),
        ", ", round(max(results_df$d1, na.rm = TRUE), 3), "]\n", sep = "")
    cat("  - d2 range: [", round(min(results_df$d2, na.rm = TRUE), 3),
        ", ", round(max(results_df$d2, na.rm = TRUE), 3), "]\n", sep = "")
    cat("  - Legislators analyzed: ", nrow(results_df), "\n\n", sep = "")
  }

  # Return structure
  result <- list(
    results = results_df,
    bcall_object = bcall_obj,
    metadata = metadata
  )

  if (verbose) {
    cat("\u2713 Analysis complete!\n")
    cat("  Use plot_bcall_analysis_interactive(results, color_by = 'cluster') to visualize\n\n")
  }

  return(result)
}
