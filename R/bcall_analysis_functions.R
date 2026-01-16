#' @title B-Call Analysis Functions
#' @description Visualization and analysis functions for B-Call results
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import R6
#' @export

# ===============================================================================
# VISUALIZATION FUNCTIONS
# ===============================================================================

#' Plot B-Call Analysis Results
#'
#' @description Creates static ggplot2 visualization of B-Call results
#' @param bcall_results List, output from bcall_auto() or bcall()
#' @param title Character, plot title
#' @param color_by Character, variable to color by ("auto" for automatic detection)
#' @param show_names Logical, whether to show legislator names
#' @param alpha Numeric, point transparency (0-1)
#' @param size Numeric, point size
#' @return ggplot2 object
#' @export
plot_bcall_analysis <- function(bcall_results,
                               title = "B-Call Analysis: Legislative Voting",
                               color_by = "auto",
                               show_names = FALSE,
                               alpha = 0.7,
                               size = 2.5) {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from bcall_auto() or bcall()")
  }

  df <- bcall_results$results

  # Auto-detect best color variable
  if (color_by == "auto") {
    if ("auto_cluster" %in% colnames(df)) {
      color_by <- "auto_cluster"
    } else if ("cluster" %in% colnames(df)) {
      color_by <- "cluster"
    } else {
      df$default_group <- "all"
      color_by <- "default_group"
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
      subtitle = sprintf("Pivot: %s | Threshold: %.0f%% | n=%d legislators",
                        bcall_results$metadata$pivot,
                        bcall_results$metadata$threshold * 100,
                        bcall_results$metadata$n_legislators_analyzed),
      x = "d1 (Ideological Position)",
      y = "d2 (Political Cohesion)",
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
    cluster_values <- unique(df[[color_by]])

    color_map <- c(
      "left" = "#2E86AB",
      "right" = "#A23B72",
      "izquierda" = "#2E86AB",
      "derecha" = "#A23B72"
    )

    if (all(cluster_values %in% names(color_map))) {
      colors_to_use <- color_map[cluster_values]
      p <- p + ggplot2::scale_color_manual(values = colors_to_use)
    }
  }

  return(p)
}

#' Interactive B-Call Analysis Plot
#'
#' @description Creates interactive plotly visualization
#' @param bcall_results List, output from bcall_auto() or bcall()
#' @param title Character, plot title
#' @param color_by Character, variable to color by ("auto" for automatic detection)
#' @return plotly object
#' @export
plot_bcall_analysis_interactive <- function(bcall_results,
                                           title = "B-Call Analysis: Legislative Voting",
                                           color_by = "auto") {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from bcall_auto() or bcall()")
  }

  df <- bcall_results$results

  # Auto-detect best color variable
  if (color_by == "auto") {
    if ("auto_cluster" %in% colnames(df)) {
      color_by <- "auto_cluster"
    } else if ("cluster" %in% colnames(df)) {
      color_by <- "cluster"
    } else {
      df$default_group <- "all"
      color_by <- "default_group"
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
    "Legislator: ", df[[leg_name_col]], "<br>",
    "d1: ", round(df$d1, 3), "<br>",
    "d2: ", round(df$d2, 3), "<br>",
    tools::toTitleCase(color_by), ": ", df[[color_by]]
  )

  # Create plotly plot
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
      xaxis = list(title = "d1 (Ideological Position)"),
      yaxis = list(title = "d2 (Political Cohesion)"),
      coloraxis = list(colorbar = list(title = tools::toTitleCase(color_by)))
    )

  return(interactive_plot)
}

# ===============================================================================
# UTILITY FUNCTIONS
# ===============================================================================

#' Summarize B-Call Analysis
#'
#' @description Prints comprehensive summary of analysis results
#' @param bcall_results List, output from bcall_auto() or bcall()
#' @export
summarize_bcall_analysis <- function(bcall_results) {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from bcall_auto() or bcall()")
  }

  results <- bcall_results$results
  metadata <- bcall_results$metadata

  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("B-CALL ANALYSIS SUMMARY\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")

  # Analysis metadata
  cat("ANALYSIS METADATA:\n")
  cat(sprintf("• Date: %s\n", metadata$date_analyzed))
  cat(sprintf("• Pivot: %s\n", metadata$pivot))
  cat(sprintf("• Threshold: %.0f%%\n", metadata$threshold * 100))
  cat(sprintf("• Legislators: %d analyzed (from %d original)\n",
             metadata$n_legislators_analyzed, metadata$n_legislators_original))
  cat(sprintf("• Votes: %d\n", metadata$n_votes))

  # Detect cluster column
  cluster_col <- if("auto_cluster" %in% colnames(results)) "auto_cluster" else "cluster"

  # Cluster distribution
  if (cluster_col %in% colnames(results)) {
    cat("\nCLUSTER DISTRIBUTION:\n")
    cluster_table <- table(results[[cluster_col]])
    for (cluster in names(cluster_table)) {
      pct <- round(cluster_table[cluster] / sum(cluster_table) * 100, 1)
      cat(sprintf("• %s: %d legislators (%.1f%%)\n",
                 tools::toTitleCase(cluster), cluster_table[cluster], pct))
    }
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

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
}

#' Export B-Call Analysis Results
#'
#' @description Exports analysis results to CSV files and plots
#' @param bcall_results List, output from bcall_auto() or bcall()
#' @param output_dir Character, output directory path
#' @param prefix Character, file prefix for exports
#' @return Character vector of exported file paths
#' @export
export_bcall_analysis <- function(bcall_results,
                                 output_dir = "output",
                                 prefix = "bcall_analysis") {

  if (!"results" %in% names(bcall_results)) {
    stop("Input must be results from bcall_auto() or bcall()")
  }

  # Create output directory
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
