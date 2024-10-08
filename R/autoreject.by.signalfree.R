#' Detect and Outlier Traces in Time Series Data with multiple trials (e.g. repeated measurements)
#'
#' This function detects and rejects outlier traces in time series data based on signal-free areas in the trace (e.g. before s signal-causing even occurred or when the response to it has ended).
#'
#' @param x A matrix or data frame containing the time series data. Each column represents
#' a different repeat, and each row represents a time point.
#' @param rejection.cutoff A numeric value representing the threshold for outlier rejection.
#'
#' @return A logical vector indicating whether each time series is rejected (TRUE) or not (FALSE).
#'
#' @details The function applies a series of data processing steps, namely scaling and
#' calculation of areas of maximum normalized spread, to identify
#' areas of high variance. Under the assumption that the signal on the data is larger than the noise. these areas of high trace-to-trace variance should be those with least signal contribution. These areas are then used to identify outlier traces  using the \link{autoreject.by.distance} function.
#' @examples
#' # Example usage
#' data_matrix <- matrix(rnorm(30000), ncol = 30)
#' rejected <- autoreject.by.signalfree(data_matrix, rejection.cutoff = 1)
#'
#' @export
#'
#' @rdname autoreject.by.signalfree
autoreject.by.signalfree <- function(x, rejection.cutoff = 1) {
  # scale the data
  if("units" %in% class(x)){
    x <- drop_units(x);
  }
  scale <- apply(x, 2, function(x) {
    scale(filter.detrend(x), center = F);
  })
  # get areas of maximum normalized spread
  CoV <- apply(scale, 1, function(x) {
    log1p(sd(x) / abs(mean(x)));
  })
  #smoothen and scale, to identify areas of high variance
  smoother <- (2 / floor((length(CoV) / 10) / 2)) + 1;
  CoV <- scale(runmed(CoV, smoother), center = T);
  RoV <- CoV > 0;
  # take the regions of high variance
  dat_ROV <- x[RoV, ];

  dat_ROV <- apply(dat_ROV, 1, function(x) {
    abs(x - mean(x)) / sd(x);
  })
  #calculate spread from normal based on this

  rejected <- autoreject.by.distance(x, threshold = rejection.cutoff)
  return(rejected);
}
