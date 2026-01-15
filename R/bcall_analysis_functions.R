#' @title B-Call Analysis Functions
#' @description Main functions for B-Call analysis with legislative voting data from CSV files
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import R6
#' @export

# ===============================================================================
# FUNCTION: PROCESS CSV DATA
# ===============================================================================

#' Process Legislative Data from CSV Files
#'
#' @description Processes legislative data from CSV files
#' @param legislators_file Character, path to legislators CSV file
#' @param votes_file Character, path to votes CSV file
#' @param verbose Logical, whether to print progress messages
#' @return List with rollcall matrix, clustering, and legislators info
# FUNCIÓN DESHABILITADA - PROBLEMA: Solo funciona con CSV, no Excel
# REEMPLAZO: Usar excel_to_rollcall() para archivos Excel
#
# process_legislative_data <- function(legislators_file = "data/legislators.csv",
#                                    votes_file = "data/votes.csv",
#                                    verbose = TRUE) {

#  if (verbose) cat("Processing legislative data from CSV files...\n")

#  # Read CSV files
#  if (verbose) cat("Reading legislators CSV file:", legislators_file, "\n")
#  legist_data <- utils::read.csv(legislators_file, stringsAsFactors = FALSE)

#  if (verbose) cat("Reading votes CSV file:", votes_file, "\n")
#  votes_data <- utils::read.csv(votes_file, stringsAsFactors = FALSE)

#  if (verbose) {
#    cat(sprintf("Legislators: %d rows\n", nrow(legist_data)))
#    cat(sprintf("Votes: %d rows\n", nrow(votes_data)))
#  }
#
#  # Create clustering from party column
#  legislators_processed <- data.frame(
#    nm_y_apellidos = legist_data$nm_y_apellidos,
#    partido_alias = if("partido_alias" %in% colnames(legist_data)) legist_data$partido_alias else NA,
#    cluster = legist_data$party,
#    stringsAsFactors = FALSE
#  )
#
#  # Convert votes to numeric format
#  if (is.character(votes_data$voto) || is.factor(votes_data$voto)) {
#    votes_data$voto_numerico <- dplyr::case_when(
#      votes_data$voto == "Afirmativo" | votes_data$voto == "1" | votes_data$voto == 1 ~ 1.0,
#      votes_data$voto == "En Contra" | votes_data$voto == "-1" | votes_data$voto == -1 ~ -1.0,
#      TRUE ~ NA_real_
#    )
#  } else {
#    votes_data$voto_numerico <- as.numeric(votes_data$voto)
#  }
#
#  # Create rollcall matrix
#  unique_votaciones <- sort(unique(votes_data$votacion_id))
#  unique_legislators <- sort(unique(votes_data$nm_y_apellidos))
#
#  rollcall_matrix <- matrix(NA,
#                           nrow = length(unique_legislators),
#                           ncol = length(unique_votaciones))
#  rownames(rollcall_matrix) <- unique_legislators
#  colnames(rollcall_matrix) <- paste0("vote_", unique_votaciones)
#
#  # Fill matrix
#  for (i in 1:nrow(votes_data)) {
#    leg <- votes_data$nm_y_apellidos[i]
#    votacion <- paste0("vote_", votes_data$votacion_id[i])
#    voto <- votes_data$voto_numerico[i]
#
#    if (leg %in% rownames(rollcall_matrix) && votacion %in% colnames(rollcall_matrix)) {
#      rollcall_matrix[leg, votacion] <- voto
#    }
#  }
#
#  rollcall_processed <- as.data.frame(rollcall_matrix)
#
#  # Synchronize data
#  common_legislators <- intersect(legislators_processed$nm_y_apellidos, rownames(rollcall_processed))
#
#  final_legislators <- legislators_processed[legislators_processed$nm_y_apellidos %in% common_legislators, ]
#  final_rollcall <- rollcall_processed[common_legislators, ]
#
#  # Create clustering dataframe
#  clustering_bcall <- data.frame(
#    cluster = final_legislators$cluster,
#    row.names = final_legislators$nm_y_apellidos
#  )
#
#  if (verbose) {
#    cat(sprintf("Final data: %d legislators x %d votes\n",
#               nrow(final_rollcall), ncol(final_rollcall)))
#    cat("Clustering distribution:\n")
#    print(table(clustering_bcall$cluster))
#  }
#
#  return(list(
#    rollcall = final_rollcall,
#    clustering = clustering_bcall,
#    legislators_info = final_legislators
#  ))
#}

# ===============================================================================
# FUNCTION: RUN B-CALL ANALYSIS
# ===============================================================================

#' Run B-Call Analysis (Central Function)
#'
#' @description Central function to run B-Call analysis on prepared rollcall and clustering data.
#' This is the convergence point for all data preparation workflows.
#' @param rollcall Data frame with rollcall voting matrix (legislators as rows, votes as columns)
#' @param clustering Data frame with clustering assignments (single column named 'cluster')
#' @param pivot Character, pivot legislator name (must be in cluster 'right')
#' @param threshold Numeric, minimum participation threshold (0.0-1.0)
#' @param auto_pivot Logical, whether to auto-select pivot if not provided
#' @param verbose Logical, whether to print progress messages
#' @return List with results, metadata, and bcall_object
#' @export
run_bcall_analysis <- function(rollcall,
                              clustering,
                              pivot = NULL,
                              threshold = 0.1,
                              auto_pivot = TRUE,
                              verbose = TRUE) {

  # Validate required inputs
  if (is.null(rollcall) || is.null(clustering)) {
    stop("Both rollcall and clustering data must be provided.")
  }

  # Ensure data.frame format
  rollcall <- as.data.frame(rollcall)
  clustering <- as.data.frame(clustering)

  # Validate pivot
  if (!is.null(pivot)) {
    if (pivot %in% rownames(rollcall)) {
      # Check if pivot is from "right" cluster
      pivot_cluster <- clustering[pivot, "cluster"]

      if (pivot_cluster != "right") {
        if (verbose) {
          cat(sprintf("WARNING: Pivot '%s' is from '%s' cluster, not 'right' cluster.\n",
                     pivot, pivot_cluster))
          cat("This may cause d1 dimension to be inverted (liberal ↔ conservative).\n")
        }
      }

      # Calculate participation for pivot
      participation <- rowSums(!is.na(rollcall)) / ncol(rollcall)
      if (verbose) {
        cat(sprintf("Using pivot: %s (%.1f%% participation, cluster: %s)\n",
                   pivot, participation[pivot] * 100, pivot_cluster))
      }
    } else {
      if (verbose) {
        cat(sprintf("Pivot '%s' not found in data, using auto-selection\n", pivot))
      }
      pivot <- NULL  # Reset to trigger auto-selection
    }
  }

  if (is.null(pivot) && auto_pivot) {
    participation <- 1 - rowSums(is.na(rollcall)) / ncol(rollcall)
    right_legislators <- rownames(clustering)[clustering$cluster == "right"]

    if (length(right_legislators) > 0) {
      pivot_participation <- participation[right_legislators]
      pivot <- names(sort(pivot_participation, decreasing = TRUE))[1]

      if (verbose) {
        cat(sprintf("Auto-selected pivot: %s (%.1f%% participation)\n",
                   pivot, participation[pivot] * 100))
      }
    }
  }

  if (is.null(pivot)) {
    stop("No pivot specified and auto-selection failed")
  }

  # Run B-Call
  if (verbose) cat("Running B-Call analysis...\n")

  bcall_result <- BCall$new(
    rollcall = rollcall,
    clustering = clustering,
    pivot = pivot,
    threshold = threshold
  )

  # Calculate pivot participation for metadata
  participation <- rowSums(!is.na(rollcall)) / ncol(rollcall)
  pivot_participation <- participation[pivot] * 100
  pivot_cluster <- clustering[pivot, "cluster"]

  # Add metadata
  metadata <- list(
    pivot = pivot,
    pivot_cluster = pivot_cluster,
    pivot_participation = sprintf("%.1f%%", pivot_participation),
    threshold = threshold,
    n_legislators_original = nrow(rollcall),
    n_legislators_analyzed = nrow(bcall_result$stats),
    n_votes = ncol(rollcall),
    participation_filter = sprintf("%.1f%%", threshold * 100),
    cluster_distribution = table(clustering[rownames(bcall_result$stats), 1]),
    missing_data_pct = round(sum(is.na(rollcall)) / (nrow(rollcall) * ncol(rollcall)) * 100, 1),
    date_analyzed = Sys.Date(),
    data_format = "CSV"
  )

  # Prepare results with cluster info
  results_df <- bcall_result$stats
  results_df$cluster <- clustering[rownames(results_df), 1]
  results_df$legislator <- rownames(results_df)

  # legislators_info is not available in this function context
  # This section is only for compatibility with legacy workflows

  if (verbose) {
    cat(sprintf("Analysis completed: %d legislators analyzed\n", nrow(results_df)))
    cat("Cluster distribution in results:\n")
    print(table(results_df$cluster))
  }

  # Return structured results
  return(list(
    results = results_df,
    bcall_object = bcall_result,
    metadata = metadata,
    raw_data = list(rollcall = rollcall, clustering = clustering)
  ))
}

# ===============================================================================
# FUNCTION: LIST LEGISLATORS BY CLUSTER
# ===============================================================================

#' List Legislators by Cluster
#'
#' @description Shows legislators grouped by cluster with participation rates
#' @param legislators_file Character, path to legislators SSB file
#' @param votes_file Character, path to votes SSB file
#' @param threshold Numeric, minimum participation threshold
#' @export
list_legislators_by_cluster <- function(legislators_file = "data/legislators.csv",
                                       votes_file = "data/votes.csv",
                                       threshold = 0.3) {

  processed_data <- process_legislative_data(legislators_file, votes_file, verbose = FALSE)
  rollcall <- processed_data$rollcall
  clustering <- processed_data$clustering

  participation <- 1 - rowSums(is.na(rollcall)) / ncol(rollcall)

  legislators_summary <- data.frame(
    legislator = rownames(clustering),
    cluster = clustering$cluster,
    participation = participation[rownames(clustering)],
    stringsAsFactors = FALSE
  )

  legislators_summary <- legislators_summary[legislators_summary$participation >= threshold, ]
  legislators_summary <- legislators_summary[order(legislators_summary$cluster, -legislators_summary$participation), ]

  cat("LEGISLATORS BY CLUSTER (meeting", threshold * 100, "% participation threshold):\n\n")

  for (cluster_name in unique(legislators_summary$cluster)) {
    cluster_legs <- legislators_summary[legislators_summary$cluster == cluster_name, ]

    cat(sprintf("%s CLUSTER (%s for pivot):\n",
               toupper(cluster_name),
               ifelse(cluster_name == "right", "Recommended", "Not recommended")))

    for (i in 1:nrow(cluster_legs)) {
      cat(sprintf("  %s (%.1f%% participation)\n",
                 cluster_legs$legislator[i],
                 cluster_legs$participation[i] * 100))
    }
    cat("\n")
  }

  invisible(legislators_summary)
}

# ===============================================================================
# FUNCTION: PLOT B-CALL RESULTS
# ===============================================================================

#' Plot B-Call Analysis Results
#'
#' @description Creates static ggplot2 visualization of B-Call results
#' @param bcall_results List, output from run_bcall_analysis()
#' @param title Character, plot title
#' @param color_by Character, variable to color by ("cluster" or "partido_alias")
#' @param show_names Logical, whether to show legislator names
#' @param alpha Numeric, point transparency (0-1)
#' @param size Numeric, point size
#' @return ggplot2 object
#' @export
plot_bcall_analysis <- function(bcall_results,
                               title = "B-Call Analysis: Legislative Voting (CSV Data)",
                               color_by = "auto",
                               show_names = FALSE,
                               alpha = 0.7,
                               size = 2.5) {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from run_bcall_analysis()")
  }

  df <- bcall_results$results

  # Auto-detect best color variable based on data source
  if (color_by == "auto") {
    if ("auto_cluster" %in% colnames(df)) {
      color_by <- "auto_cluster"
      cat("Using auto_cluster for coloring (automatic clustering results)\n")
    } else if ("party" %in% colnames(df)) {
      color_by <- "party"
      cat("Using party for coloring (Excel with party info)\n")
    } else if ("cluster" %in% colnames(df)) {
      color_by <- "cluster"
      cat("Using cluster for coloring (external clustering)\n")
    } else if ("partido_alias" %in% colnames(df)) {
      color_by <- "partido_alias"
      cat("Using partido_alias for coloring\n")
    } else {
      df$default_group <- "all"
      color_by <- "default_group"
      cat("No clustering info found, using default grouping\n")
    }
  } else if (!color_by %in% colnames(df)) {
    stop(sprintf("Variable '%s' not found in data. Available: %s",
                color_by, paste(colnames(df), collapse=", ")))
  }

  # Create base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = d1, y = d2)) +
    ggplot2::geom_point(ggplot2::aes_string(color = color_by), alpha = alpha, size = size) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Pivot: %s (%s cluster) | Threshold: %s | n=%d legislators",
                        bcall_results$metadata$pivot,
                        bcall_results$metadata$pivot_cluster,
                        bcall_results$metadata$participation_filter,
                        bcall_results$metadata$n_legislators_analyzed),
      x = "d1 (Posición Política: ← Izquierda | Derecha →)",
      y = "d2 (Political Cohesion: ← More cohesive | Less cohesive →)",
      color = tools::toTitleCase(gsub("_", " ", color_by))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
      axis.title = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    )

  # Add names if requested
  if (show_names) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = legislator),
                               size = 2.5, vjust = -0.5, check_overlap = TRUE)
  }

  # Add color scheme
  if (color_by == "cluster" || color_by == "auto_cluster") {
    p <- p + ggplot2::scale_color_manual(values = c("left" = "#2E86AB", "right" = "#A23B72"))
  }

  return(p)
}

# ===============================================================================
# FUNCTION: INTERACTIVE PLOT
# ===============================================================================

#' Interactive B-Call Analysis Plot
#'
#' @description Creates interactive plotly visualization
#' @param bcall_results List, output from run_bcall_analysis()
#' @param title Character, plot title
#' @param color_by Character, variable to color by
#' @return plotly object
#' @export
plot_bcall_analysis_interactive <- function(bcall_results,
                                           title = "B-Call Analysis: Legislative Voting (CSV Data)",
                                           color_by = "auto") {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from run_bcall_analysis()")
  }

  df <- bcall_results$results

  # Auto-detect best color variable based on data source
  if (color_by == "auto") {
    if ("auto_cluster" %in% colnames(df)) {
      color_by <- "auto_cluster"
      cat("Using auto_cluster for coloring (automatic clustering results)\n")
    } else if ("party" %in% colnames(df)) {
      color_by <- "party"
      cat("Using party for coloring (Excel with party info)\n")
    } else if ("cluster" %in% colnames(df)) {
      color_by <- "cluster"
      cat("Using cluster for coloring (external clustering)\n")
    } else if ("partido_alias" %in% colnames(df)) {
      color_by <- "partido_alias"
      cat("Using partido_alias for coloring\n")
    } else {
      df$default_group <- "all"
      color_by <- "default_group"
      cat("No clustering info found, using default grouping\n")
    }
  } else if (!color_by %in% colnames(df)) {
    stop(sprintf("Variable '%s' not found in data. Available: %s",
                color_by, paste(colnames(df), collapse=", ")))
  }

  # Detect legislator name column
  leg_name_col <- if("legislator" %in% colnames(df)) {
    "legislator"
  } else if("nm_y_apellidos" %in% colnames(df)) {
    "nm_y_apellidos"
  } else {
    rownames(df)
  }

  # Create custom labels for tooltip
  df$tooltip_text <- paste0(
    "Legislador: ", df[[leg_name_col]], "<br>",
    "d1 (Posición): ", round(df$d1, 3), "<br>",
    "d2 (Cohesión): ", round(df$d2, 3), "<br>",
    if(color_by == "party") "Orientación: " else paste0(tools::toTitleCase(color_by), ": "),
    df[[color_by]]
  )

  # Create plotly plot directly with custom tooltips
  interactive_plot <- plotly::plot_ly(
    data = df,
    x = ~d1,
    y = ~d2,
    color = ~get(color_by),
    text = ~tooltip_text,
    hovertemplate = "%{text}<extra></extra>",
    type = "scatter",
    mode = "markers",
    marker = list(size = 8)
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = "d1 (Posición Política: ← Izquierda | Derecha →)"),
      yaxis = list(title = "d2 (Cohesión Política: ← Más Cohesivo | Menos Cohesivo →)"),
      coloraxis = list(colorbar = list(title = tools::toTitleCase(color_by)))
    )

  return(interactive_plot)
}

# ===============================================================================
# FUNCTION: SUMMARIZE ANALYSIS
# ===============================================================================

#' Summarize B-Call Analysis
#'
#' @description Prints comprehensive summary of analysis results
#' @param bcall_results List, output from run_bcall_analysis()
#' @export
summarize_bcall_analysis <- function(bcall_results) {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from run_bcall_analysis()")
  }

  results <- bcall_results$results
  metadata <- bcall_results$metadata

  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("B-CALL ANALYSIS SUMMARY (CSV DATA)\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")

  # Analysis metadata
  cat("ANALYSIS METADATA:\n")
  cat(sprintf("• Date: %s\n", metadata$date_analyzed))
  cat(sprintf("• Data format: %s\n", metadata$data_format))
  cat(sprintf("• Pivot legislator: %s (%s cluster, %s participation)\n",
             metadata$pivot, metadata$pivot_cluster, metadata$pivot_participation))
  cat(sprintf("• Participation threshold: %s\n", metadata$participation_filter))
  cat(sprintf("• Legislators: %d analyzed (from %d original)\n",
             metadata$n_legislators_analyzed, metadata$n_legislators_original))
  cat(sprintf("• Votes: %d\n", metadata$n_votes))
  cat(sprintf("• Missing data: %s%%\n", metadata$missing_data_pct))

  # Cluster distribution
  cat("\nCLUSTER DISTRIBUTION:\n")
  cluster_table <- table(results$cluster)
  for (cluster in names(cluster_table)) {
    pct <- round(cluster_table[cluster] / sum(cluster_table) * 100, 1)
    cat(sprintf("• %s: %d legislators (%s%%)\n",
               tools::toTitleCase(cluster), cluster_table[cluster], pct))
  }

  # Statistical summary
  cat("\nSTATISTICAL SUMMARY:\n")
  cat("d1 (Ideological Position):\n")
  cat(sprintf("  Min: %.3f | Max: %.3f | Mean: %.3f | SD: %.3f\n",
             min(results$d1, na.rm = TRUE), max(results$d1, na.rm = TRUE),
             mean(results$d1, na.rm = TRUE), sd(results$d1, na.rm = TRUE)))

  cat("d2 (Political Cohesion):\n")
  cat(sprintf("  Min: %.3f | Max: %.3f | Mean: %.3f | SD: %.3f\n",
             min(results$d2, na.rm = TRUE), max(results$d2, na.rm = TRUE),
             mean(results$d2, na.rm = TRUE), sd(results$d2, na.rm = TRUE)))

  # Extreme legislators
  cat("\nEXTREME POSITIONS:\n")
  cat("Most Liberal (lowest d1):\n")
  most_liberal <- results[which.min(results$d1), ]
  cat(sprintf("  %s (d1=%.3f, d2=%.3f, %s cluster)\n",
             most_liberal$legislator, most_liberal$d1, most_liberal$d2, most_liberal$cluster))

  cat("Most Conservative (highest d1):\n")
  most_conservative <- results[which.max(results$d1), ]
  cat(sprintf("  %s (d1=%.3f, d2=%.3f, %s cluster)\n",
             most_conservative$legislator, most_conservative$d1, most_conservative$d2, most_conservative$cluster))

  cat("Most Cohesive (lowest d2):\n")
  most_cohesive <- results[which.min(results$d2), ]
  cat(sprintf("  %s (d1=%.3f, d2=%.3f, %s cluster)\n",
             most_cohesive$legislator, most_cohesive$d1, most_cohesive$d2, most_cohesive$cluster))

  cat("Least Cohesive (highest d2):\n")
  least_cohesive <- results[which.max(results$d2), ]
  cat(sprintf("  %s (d1=%.3f, d2=%.3f, %s cluster)\n",
             least_cohesive$legislator, least_cohesive$d1, least_cohesive$d2, least_cohesive$cluster))

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
}

# ===============================================================================
# FUNCTION: EXPORT RESULTS
# ===============================================================================

#' Export B-Call Analysis Results
#'
#' @description Exports analysis results to CSV files and plots
#' @param bcall_results List, output from run_bcall_analysis()
#' @param output_dir Character, output directory path
#' @param prefix Character, file prefix for exports
#' @return Character vector of exported file paths
#' @export
export_bcall_analysis <- function(bcall_results,
                                 output_dir = "output",
                                 prefix = "bcall_analysis") {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from run_bcall_analysis()")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Export results CSV
  results_file <- file.path(output_dir, sprintf("%s_results_%s.csv", prefix, timestamp))
  utils::write.csv(bcall_results$results, results_file, row.names = TRUE)

  # Export metadata CSV
  metadata_file <- file.path(output_dir, sprintf("%s_metadata_%s.csv", prefix, timestamp))
  metadata_df <- data.frame(
    parameter = names(bcall_results$metadata),
    value = sapply(bcall_results$metadata, as.character),
    stringsAsFactors = FALSE
  )
  utils::write.csv(metadata_df, metadata_file, row.names = FALSE)

  # Export main plot
  plot_file <- file.path(output_dir, sprintf("%s_plot_%s.png", prefix, timestamp))
  main_plot <- plot_bcall_analysis(bcall_results)
  ggplot2::ggsave(plot_file, main_plot, width = 12, height = 8, dpi = 300)

  exported_files <- c(results_file, metadata_file, plot_file)

  cat("Exported files:\n")
  for (file in exported_files) {
    cat(sprintf("• %s\n", file))
  }

  return(exported_files)
}