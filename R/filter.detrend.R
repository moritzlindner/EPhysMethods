#' Apply Detrending Filter
#'
#' This function is a wrapper for the \code{\link[pracma:detrend]{pracma::detrend}} function in the pracma package.
#' It applies a detrending filter to a given time series.
#'
#' @param x A numeric vector representing the time series data.
#' @param ... Additional arguments to be passed to \code{\link[pracma:detrend]{pracma::detrend}} function.
#'
#' @return A numeric vector representing the detrended time series.
#'
#' @seealso \code{\link[pracma:detrend]{detrend::detrend}}
#'
#' @examples
#' x <- c(1, 2, 3, 4, 3, 2, 1)
#' detrended <- filter.detrend(x)
#'
#' @importFrom pracma detrend
#'
#' @family filter functions
#' @export filter.detrend
filter.detrend <- function(x, ...) {
  detrend(x, ...);
}
