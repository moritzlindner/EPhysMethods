% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.lin.detrend.R
\name{filter.lin.detrend}
\alias{filter.lin.detrend}
\title{Apply Linear Detrending Filter}
\usage{
\method{filter}{lin.detrend}(x)
}
\arguments{
\item{x}{A numeric vector representing the time series data.}
}
\value{
A numeric vector representing the detrended time series.
}
\description{
This function applies a linear detrending filter to the input time series data.
Linear detrending involves removing the linear trend from the data by subtracting
the estimated trend component.
}
\details{
Linear detrending is achieved by estimating the linear trend of the
input time series data. The function calculates the mean values of the first
and last few observations (default is 10 observations each) to estimate the
initial and final values of the linear trend line. Then, it generates a
linearly spaced sequence of values between these initial and final values to
represent the trend component. Finally, it subtracts this estimated trend
component from the original data, yielding the detrended time series.
}
\examples{
x <- c(1, 2, 3, 4, 3, 2, 1)
detrended <- filter.detrend(x)

}
\seealso{
Other filter functions: 
\code{\link{filter.detrend}()}
}
\concept{filter functions}
