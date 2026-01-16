#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import R6

# Declare global variables used in dplyr/ggplot2 to avoid R CMD check NOTEs
utils::globalVariables(c("d1", "d2", "legislator"))

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
#'
#' @examples
#' # Create sample data and run analysis
#' rollcall <- data.frame(
#'   vote1 = c(1, -1, 1, -1, 1),
#'   vote2 = c(1, -1, 1, -1, 1),
#'   vote3 = c(-1, 1, -1, 1, -1),
#'   row.names = paste0("Leg_", LETTERS[1:5])
#' )
#'
#' results <- bcall_auto(rollcall, verbose = FALSE)
#'
#' # Create static plot
#' plot_bcall_analysis(results)
#'
#' # With legislator names
#' plot_bcall_analysis(results, show_names = TRUE)
#'
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
#'
#' @examples
#' \donttest{
#' # Create sample data and run analysis
#' rollcall <- data.frame(
#'   vote1 = c(1, -1, 1, -1, 1),
#'   vote2 = c(1, -1, 1, -1, 1),
#'   vote3 = c(-1, 1, -1, 1, -1),
#'   row.names = paste0("Leg_", LETTERS[1:5])
#' )
#'
#' results <- bcall_auto(rollcall, verbose = FALSE)
#'
#' # Create interactive plot (hover over points to see legislator names)
#' plot_bcall_analysis_interactive(results)
#' }
#'
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
