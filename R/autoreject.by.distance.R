#' Identify outlier observations  based on distances
#'
#' This function calculates the Euclidean distance between observations in a matrix,
#' and identifies outlier observations to reject based on a given threshold.
#'
#' @inheritParams autoreject.by.signalfree
#' @param threshold The distance threshold for rejection (default is 1).
#'
#' @return A logical vector indicating which observations to reject.
#'
#' @details The function calculates the Euclidean distance between all pairs of
#' observations in the input matrix 'x'. It then scales the distance matrix and
#' identifies observations with mean distances greater than the specified threshold
#' for rejection.
#'
#' @examples
#' data <- matrix(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)
#' rejected <- autoreject.by.distance(data, threshold = 1)
#'
#' @importFrom stats dist
#' @importFrom units drop_units
#' @export
autoreject.by.distance <- function(x, threshold = 1) {
  if("units" %in% class(x)){
    x <- drop_units(x);
  }
  dist_matrix <- dist(t(x), method = "euclidean");
  dist_df <- as.matrix(dist_matrix);
  reject <- scale(apply(dist_df, 1, mean)) > threshold;
  return(reject)
}
