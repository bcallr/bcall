# ===============================================================================
# BCALL CLASS
# ===============================================================================

#' @import R6

# BCall R6 Class - Internal use only
# Users should use bcall_auto() or bcall() instead
BCall <- R6::R6Class("BCall",
  public = list(
    #' @field rollcall Rollcall voting matrix
    rollcall = NULL,
    #' @field clustering Clustering assignment
    clustering = NULL,
    #' @field pivot Pivot legislator name
    pivot = NULL,
    #' @field threshold Participation threshold
    threshold = NULL,
    #' @field stats Analysis results
    stats = NULL,

    #' @description Initialize BCall object
    #' @param rollcall Data frame with rollcall voting data
    #' @param clustering Data frame with clustering assignments
    #' @param pivot Character, pivot legislator name
    #' @param threshold Numeric, minimum participation threshold
    initialize = function(rollcall, clustering, pivot = "", threshold = 0.1) {
      cat(sprintf('Rollcall dataframe contains %d legislators and %d votes.\n',
                 nrow(rollcall), ncol(rollcall)))

      self$rollcall <- self$validate_dataframe(rollcall, 'rollcall')
      self$clustering <- self$validate_dataframe(clustering, 'clustering')
      self$pivot <- pivot
      self$threshold <- threshold

      self$validate_inputs()
      self$stats <- self$calculate()
    },

    #' @description Validate dataframe input
    #' @param df Data frame to validate
    #' @param name Name for error messages
    validate_dataframe = function(df, name) {
      if (!is.data.frame(df) && !is.matrix(df)) {
        stop(sprintf("%s must be a DataFrame or matrix.", name))
      }
      return(as.data.frame(df))
    },

    #' @description Validate all inputs
    validate_inputs = function() {
      if (nrow(self$clustering) != nrow(self$rollcall)) {
        stop("Length of clustering must match the number of rows in rollcall.")
      }

      if (ncol(self$clustering) != 1) {
        stop("clustering must only have one column.")
      }

      if (ncol(self$clustering) == 1) {
        cluster_values <- self$clustering[, 1]
        names(cluster_values) <- rownames(self$clustering)
        self$clustering <- cluster_values
      }

      if (length(unique(self$clustering)) != 2) {
        stop("clustering must have only two unique values.")
      }

      if (!(self$pivot %in% rownames(self$rollcall))) {
        stop("pivot must be an element of the rollcall's index.")
      }
    },

    #' @description Calculate B-Call statistics
    calculate = function() {
      rollcall_matrix <- as.matrix(self$rollcall)

      participation <- 1 - rowSums(is.na(rollcall_matrix)) / ncol(rollcall_matrix)

      keep_legislators <- participation > self$threshold
      rollcall_filtered <- rollcall_matrix[keep_legislators, ]
      clustering_filtered <- self$clustering[keep_legislators]

      cat(sprintf('%d legislators meet the participation threshold.\n', nrow(rollcall_filtered)))

      if (!(self$pivot %in% names(clustering_filtered))) {
        stop("Choose another pivot as the previous one does not meet the participation threshold.")
      }

      pivot_cluster <- clustering_filtered[self$pivot]
      left_legislators <- names(clustering_filtered)[clustering_filtered != pivot_cluster]
      right_legislators <- names(clustering_filtered)[clustering_filtered == pivot_cluster]

      if (length(left_legislators) == 0) {
        stop("There must be at least one legislator in the left cluster.")
      }

      left_mean <- colMeans(rollcall_filtered[left_legislators, , drop = FALSE], na.rm = TRUE)
      right_mean <- colMeans(rollcall_filtered[right_legislators, , drop = FALSE], na.rm = TRUE)

      overall_mean <- colMeans(rollcall_filtered, na.rm = TRUE)
      overall_std <- apply(rollcall_filtered, 2, sd, na.rm = TRUE)

      # FILTRAR VOTACIONES CON VARIANZA CERO (como en Python pandas)
      valid_votes <- overall_std > 0
      invalid_votes_count <- sum(!valid_votes)
      if (invalid_votes_count > 0) {
        cat(sprintf('Filtered %d votes with zero variance (unanimous votes).\n', invalid_votes_count))
      }
      if (sum(valid_votes) == 0) {
        stop("No votes with variance > 0 available for analysis")
      }

      # Use only valid votes
      rollcall_valid <- rollcall_filtered[, valid_votes, drop = FALSE]
      left_mean_valid <- left_mean[valid_votes]
      right_mean_valid <- right_mean[valid_votes]
      overall_mean_valid <- overall_mean[valid_votes]
      overall_std_valid <- overall_std[valid_votes]

      direction <- 2 * as.numeric(left_mean_valid < right_mean_valid) - 1

      # Vectorized standardization calculation (EXACTO como Python pandas)
      standardization <- sweep(rollcall_valid, 2, overall_mean_valid, "-")
      standardization <- sweep(standardization, 2, overall_std_valid, "/")
      standardization <- sweep(standardization, 2, direction, "*")

      d1 <- rowMeans(standardization, na.rm = TRUE)
      d2 <- apply(standardization, 1, sd, na.rm = TRUE)

      stats <- data.frame(
        d1 = d1,
        d2 = d2,
        row.names = rownames(rollcall_filtered)
      )

      return(stats)
    }
  )
)

# ===============================================================================
# CLUSTERING CLASS
# ===============================================================================

# Clustering R6 Class - Internal use only
# Users should use bcall_auto() instead
Clustering <- R6::R6Class("Clustering",
  public = list(
    #' @field N Distance metric (1=Manhattan, 2=Euclidean)
    N = NULL,
    #' @field X Rollcall data matrix
    X = NULL,
    #' @field clusters List of cluster assignments
    clusters = NULL,
    #' @field clustering Final clustering dataframe
    clustering = NULL,

    #' @description Initialize Clustering object
    #' @param rollcalls Data frame with rollcall data
    #' @param N Integer, distance metric (1 or 2)
    #' @param pivot Character, pivot legislator name
    initialize = function(rollcalls, N = 1, pivot = NULL) {
      self$N <- N
      self$X <- as.matrix(rollcalls)

      if (is.null(pivot)) {
        pivot <- rownames(self$X)[1]
      }

      self$clusters <- list("0" = c(), "1" = c())
      self$initialize_clusters(pivot)
      self$iterate_clustering()
      self$assign_clusters(pivot)
      self$create_clustering_dataframe()
      self$reclassify_legislators()
    },

    #' @description Initialize clusters with farthest legislators
    #' @param pivot Character, pivot legislator name
    initialize_clusters = function(pivot) {
      distances <- self$get_distances(self$X, self$X)
      max_idx <- which(distances == max(distances, na.rm = TRUE), arr.ind = TRUE)[1, ]
      leg0 <- rownames(self$X)[max_idx[1]]
      leg1 <- rownames(self$X)[max_idx[2]]

      self$clusters[["0"]] <- c(leg0)
      self$clusters[["1"]] <- c(leg1)
    },

    #' @description Iterate clustering assignment
    iterate_clustering = function() {
      total_legs <- nrow(self$X)
      assigned_legs <- length(self$clusters[["0"]]) + length(self$clusters[["1"]])

      while (assigned_legs < total_legs) {
        self$iteration()
        assigned_legs <- length(self$clusters[["0"]]) + length(self$clusters[["1"]])
      }
    },

    #' @description Single clustering iteration
    iteration = function() {
      assigned_legs <- c(self$clusters[["0"]], self$clusters[["1"]])
      remaining_legs <- setdiff(rownames(self$X), assigned_legs)

      if (length(remaining_legs) == 0) return()

      if (length(self$clusters[["0"]]) > 0) {
        centroid0 <- colMeans(self$X[self$clusters[["0"]], , drop = FALSE], na.rm = TRUE)
      } else {
        centroid0 <- rep(0, ncol(self$X))
      }

      if (length(self$clusters[["1"]]) > 0) {
        centroid1 <- colMeans(self$X[self$clusters[["1"]], , drop = FALSE], na.rm = TRUE)
      } else {
        centroid1 <- rep(0, ncol(self$X))
      }

      centroids <- rbind(centroid0, centroid1)

      remaining_data <- self$X[remaining_legs, , drop = FALSE]
      distances <- self$get_distances(remaining_data, centroids)

      min_idx <- which(distances == min(distances, na.rm = TRUE), arr.ind = TRUE)[1, ]
      closest_leg <- remaining_legs[min_idx[1]]
      closest_cluster <- as.character(min_idx[2] - 1)

      self$clusters[[closest_cluster]] <- c(self$clusters[[closest_cluster]], closest_leg)
    },

    #' @description Calculate distances between tensors
    #' @param tensor1 First data matrix
    #' @param tensor2 Second data matrix
    get_distances = function(tensor1, tensor2) {
      tensor1 <- as.matrix(tensor1)
      tensor2 <- as.matrix(tensor2)

      qty1 <- (!is.na(tensor1)) * 1
      qty2 <- (!is.na(tensor2)) * 1
      norm_matrix <- qty1 %*% t(qty2)

      distances <- array(NA, dim = c(nrow(tensor1), nrow(tensor2)))

      for (i in 1:nrow(tensor1)) {
        for (j in 1:nrow(tensor2)) {
          if (self$N == 1) {
            diff <- abs(tensor1[i, ] - tensor2[j, ])
            distances[i, j] <- sum(diff, na.rm = TRUE)
            norm_val <- norm_matrix[i, j] * 2
          } else if (self$N == 2) {
            diff <- (tensor1[i, ] - tensor2[j, ])^2
            distances[i, j] <- sqrt(sum(diff, na.rm = TRUE))
            norm_val <- 2 * sqrt(norm_matrix[i, j])
          }

          if (norm_val > 0) {
            distances[i, j] <- distances[i, j] / norm_val
          } else {
            distances[i, j] <- 0
          }
        }
      }

      return(distances)
    },

    #' @description Assign left and right clusters based on pivot
    #' @param pivot Character, pivot legislator name
    assign_clusters = function(pivot) {
      if (pivot %in% self$clusters[["0"]]) {
        self$clusters[["right"]] <- self$clusters[["0"]]
        self$clusters[["left"]] <- self$clusters[["1"]]
      } else if (pivot %in% self$clusters[["1"]]) {
        self$clusters[["left"]] <- self$clusters[["0"]]
        self$clusters[["right"]] <- self$clusters[["1"]]
      } else {
        stop("Pivot legislator not found in either cluster.")
      }
    },

    #' @description Create clustering dataframe
    create_clustering_dataframe = function() {
      clustering_data <- data.frame(
        legislators = c(self$clusters[["left"]], self$clusters[["right"]]),
        cluster = c(rep("left", length(self$clusters[["left"]])),
                   rep("right", length(self$clusters[["right"]])))
      )
      rownames(clustering_data) <- clustering_data$legislators
      clustering_data$legislators <- NULL

      self$clustering <- clustering_data[rownames(self$X), , drop = FALSE]
    },

    #' @description Reclassify legislators to optimal clusters
    reclassify_legislators = function() {
      adjustments_needed <- TRUE

      while (adjustments_needed) {
        centroid_left <- colMeans(self$X[self$clusters[["left"]], , drop = FALSE], na.rm = TRUE)
        centroid_right <- colMeans(self$X[self$clusters[["right"]], , drop = FALSE], na.rm = TRUE)

        centroids <- rbind(centroid_left, centroid_right)
        distances <- self$get_distances(self$X, centroids)

        optimal_assignment <- ifelse(distances[, 1] < distances[, 2], "left", "right")
        names(optimal_assignment) <- rownames(self$X)

        current_assignment <- self$clustering[, 1]
        names(current_assignment) <- rownames(self$clustering)

        misclassified <- names(current_assignment)[current_assignment != optimal_assignment[names(current_assignment)]]

        if (length(misclassified) == 0) {
          adjustments_needed <- FALSE
        } else {
          for (leg in misclassified) {
            current_cluster <- current_assignment[leg]
            new_cluster <- optimal_assignment[leg]

            self$clusters[[current_cluster]] <- setdiff(self$clusters[[current_cluster]], leg)
            self$clusters[[new_cluster]] <- c(self$clusters[[new_cluster]], leg)
          }

          self$create_clustering_dataframe()
        }
      }
    }
  )
)