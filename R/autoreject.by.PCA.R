#' Identify outlier observations based on PCA loadings
#'
#' This function performs Principal Component Analysis (PCA) on a matrix or data frame,
#' scales the data, and identifies outlier observations to reject based on their contributions
#' to the principal components. The observations with the highest contribution to the principal
#' components are considered outliers.
#'
#' @param x A matrix or data frame with the observations.
#' @param pct.keep The proportion of observations to keep based on their PCA loadings (default is 0.75).
#'
#' @return A logical vector indicating which observations to reject.
#'
#' @details The function performs PCA on the input matrix 'x', scales the data, and calculates
#' the loadings of the observations. It then identifies the top contributing observations to the
#' principal components based on a specified proportion to keep. Observations with lower contributions
#' are marked for rejection.
#'
#' @examples
#' # data <- matrix(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ncol = 3)
#' # rejected <- autoreject.by.PCA(data, pct.keep = 0.75)
#'
#' @importFrom stats prcomp
#' @importFrom units drop_units
#' @keywords internal
autoreject.by.PCA <- function(x, pct.keep = 0.75) {
  message("EXPERIMENTAL")
  # Check for the class of x and remove units if necessary
  if ("units" %in% class(x)) {
    x <- drop_units(x)
  }

  # Ensure x is a matrix
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  # Perform PCA with scaling
  pca_result <- prcomp(x, scale. = TRUE)

  # Extract loadings and calculate absolute loadings
  loadings <- pca_result$rotation
  abs_loadings <- abs(loadings)

  # Determine the number of components to consider (up to 90% cumulative proportion)
  cumulative_proportion <- summary(pca_result)$importance["Cumulative Proportion", ]
  num_components_to_consider <- sum(cumulative_proportion < 0.90)

  # Ensure we consider at least one component
  if (num_components_to_consider == 0) {
    num_components_to_consider <- 1
  }

  # Calculate the contribution of each column
  column_contributions <- rowSums(abs_loadings[, 1:num_components_to_consider])

  # Sort columns by their contributions
  sorted_columns <- order(column_contributions, decreasing = TRUE)

  # Determine the number of top columns to keep
  num_columns_to_keep <- ceiling(ncol(x) * pct.keep)

  # Get the top columns
  top_columns <- sorted_columns[1:num_columns_to_keep]

  # Identify columns to reject
  reject <- !(1:ncol(x) %in% top_columns)

  return(reject)
}
