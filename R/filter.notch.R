#' Filter a signal with a notch filter.
#'
#' This function filters a signal using a Butterworth bandstop (notch) filter
#' with specified low and high frequency cutoffs. It utilizes the
#' \link[signal:butter]{signal::butter} function from the
#' \link[signal:signal]{signal::signal} package.
#'
#' @paramFrom filter.bandpass x, low, high, order
#'
#' @return Filtered signal with the same dimensions as input \code{x}.
#'
#' @details
#' * A notch filter is used to remove specific frequency components while
#'   preserving the surrounding frequencies.
#' * This function applies a zero-phase filtering approach using
#'   \code{\link[signal:filtfilt]{signal::filtfilt}} to avoid phase distortion.
#'
#' @examples
#' fs <- 2000  # Sampling rate in Hz
#' f1 <- 20  # Desired frequency in Hz
#' f2 <- 6  # Desired frequency in Hz
#' duration <- 1  # Duration in seconds
#' t <- seq(0, duration, by = 1/fs)  # Time vector
#' signal <- sin(2 * pi * f1 * t) + sin(2 * pi * f2 * t)
#' plot(t, signal, type = "l", xlab = "Time (s)", ylab = "Amplitude",
#' col = "blue", xlim = c(0, 1))
#' filtered_signal <- filter.notch(signal, freq.to.w(x=18, samp.freq=fs),
#'                                 freq.to.w(x=22, samp.freq=fs))
#' plot(t, filtered_signal, type = "l", xlab = "Time (s)", ylab = "Amplitude",
#' col = "blue", xlim = c(0, 1))
#'
#' @seealso \link[signal:butter]{signal::butter}, \link[signal:filtfilt]{signal::filtfilt}
#'
#' @docType Function
#' @export
filter.notch <- function(x, low, high, order = 4) {
  # Check that all input parameters are numeric
  if (!all(sapply(list(x, low, high), is.numeric))) {
    stop("All input parameters must be numeric.")
  }

  # Handle units if present
  un <- NULL
  if ("units" %in% class(x)) {
    un <- units(x)
    units(x) <- NULL
  }
  if ("units" %in% class(low)) {
    units(low) <- NULL
    warning("Unit of 'low' is ignored.")
  }
  if ("units" %in% class(high)) {
    units(high) <- NULL
    warning("Unit of 'high' is ignored.")
  }

  # Validate frequency bounds (normalized frequencies: 0 to 1)
  if (low >= high || low < 0 || high > 1) {
    stop("Invalid low or high frequency values.")
  }

  # Apply the notch filter to a vector or each column of a matrix
  if (is.null(dim(x))) {
    out <- filter.notch.core(x, low, high, order = order)
  } else {
    out <- apply(x, 2, function(col) {
      filter.notch(col, low, high, order = order)
    })
  }

  # Reapply units if they were present originally
  if (!is.null(un)) {
    out <- as_units(out, un)
  }

  return(out)
}

#' Core notch filter function.
#'
#' This function performs the core notch filtering operation using a
#' Butterworth bandstop filter.
#'
#' @param x Numeric vector of signal data.
#' @param low Numeric scalar, the lower cutoff frequency W.
#' @param high Numeric scalar, the upper cutoff frequency W.
#' @param order Order of the filter to be applied. Default is 4.
#'
#' @return Filtered signal.
#'
#' @importFrom signal butter filtfilt
#' @keywords internal
#' @noRd
filter.notch.core <- function(x, low, high, order = 4) {
  # Ensure that the signal package is available
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop("The 'signal' package is required for notch filtering. Please install it using install.packages('signal').")
  }

  # Design a Butterworth bandstop (notch) filter.
  # 'c(low, high)' specifies the stop-band (normalized frequencies)
  filt <- signal::butter(order, c(low, high), type = "stop")

  # Apply zero-phase filtering using filtfilt to avoid phase distortion
  out <- signal::filtfilt(filt, x)
  #out <- as.numeric(signal::filter(filt, x))

  return(out)
}
