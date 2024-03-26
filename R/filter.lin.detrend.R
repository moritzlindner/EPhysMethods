#' Apply Linear Detrending Filter
#'
#' This function applies a linear detrending filter to the input time series data.
#' Linear detrending involves removing the linear trend from the data by subtracting
#' the estimated trend component.
#'
#' @param x A numeric vector representing the time series data.
#'
#' @return A numeric vector representing the detrended time series.
#'
#' @details Linear detrending is achieved by estimating the linear trend of the
#' input time series data. The function calculates the mean values of the first
#' and last few observations (default is 10 observations each) to estimate the
#' initial and final values of the linear trend line. Then, it generates a
#' linearly spaced sequence of values between these initial and final values to
#' represent the trend component. Finally, it subtracts this estimated trend
#' component from the original data, yielding the detrended time series.
#'
#'
#' @examples
#' x <- c(1, 2, 3, 4, 3, 2, 1)
#' detrended <- filter.detrend(x)
#'
#' @family filter functions
#' @export filter.lin.detrend
filter.lin.detrend <- function(x) {
  start<-mean(x[1:10]);
  end<-mean(x[(length(x)-10):length(x)]);
  trend<-seq(start,end,length.out=length(x));
  return(x-trend);
}
