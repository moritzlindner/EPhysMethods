#' Filter a signal with a bandpass filter.
#'
#' This function filters a signal using a fourth order Butterworth bandpass filter with
#' specified low and high frequency cutoffs. This function uses the \link[signal:butter]{signal::butter} function from the \link[signal:signal]{signal::signal} package.
#'
#' @param x Numeric vector or matrix of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param low Numeric scalar, the lower cutoff frequency in Hertz.
#' @param high Numeric scalar, the upper cutoff frequency in Hertz.
#' @import units
#'
#' @return Filtered signal with the same dimensions as input 'x'.
#'
#' @examples
#' filtered_signal <- filter.bandpass(signal, samp.freq = 1000, low = 5, high = 100)
#' @seealso \link[signal:butter]{signal::butter}
#'
#' @export
filter.bandpass <- function(x, samp.freq, low, high) {
  # Check if input parameters are numeric
  if (!all(sapply(list(x, samp.freq, low, high), is.numeric))) {
    stop("All input parameters must be numeric.")
  }

  # Check if the sampling frequency is positive
  if (samp.freq <= 0) {
    stop("Sampling frequency must be greater than zero.")
  }

  # Check if the low and high frequencies are in the correct range
  if (low >= high || low < 0 || high >= 0.5 * samp.freq) {
    stop("Invalid low and high frequency values.")
  }

  un <- NULL
  if ("units" %in% class(x)) {
    un <- units(x)
    units(x) <- NULL
    x<-drop_units(x)
  }
  units(samp.freq) <- NULL
  units(low) <- NULL
  units(high) <- NULL
  if (is.null(dim(x))) {
    out <- filter.bandpass.core(x, samp.freq, low, high)
  } else{
    out <- apply(x, 2, function(x) {
      filter.bandpass(x
                      , samp.freq, low, high)
    })
  }
  if (!is.null(un)) {
    return(as_units(out, un))
  } else{
    return(out)
  }
}

#' Core bandpass filter function.
#'
#' This function performs the core bandpass filtering operation using a
#' Butterworth filter.
#'
#' @param x Numeric vector of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param low Numeric scalar, the lower cutoff frequency in Hertz.
#' @param high Numeric scalar, the upper cutoff frequency in Hertz.
#'
#' @return Filtered signal.
#'
#' @importFrom signal butter filtfilt
#' @noRd
filter.bandpass.core <- function(x, samp.freq, low, high) {
  bf <- butter(4, c(low, high)  * (2 / samp.freq), type = "pass")
  x.filtered <- filtfilt(bf, x)
  return(x.filtered)
}
