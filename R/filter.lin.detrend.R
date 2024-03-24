#' Apply Linear Detrending Filter
#'
#' This function substracts the linear trend from the data.
#'
#' @param x A numeric vector representing the time series data.
#'
#' @return A numeric vector representing the detrended time series.
#'
#' @seealso \code{\link[pracma]{detrend}}
#'
#' @examples
#' x <- c(1, 2, 3, 4, 3, 2, 1)
#' detrended <- filter.detrend(x)
#'
#'@family filter functions
#' @export filter.lin.detrend
filter.lin.detrend <- function(x) {
  start<-mean(x[1:10]);
  end<-mean(x[(length(x)-10):length(x)]);
  trend<-seq(start,end,length.out=length(x));
  return(x-trend);
}
