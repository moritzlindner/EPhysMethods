#' Compute the Fast Fourier Transform (FFT) of a signal.
#'
#' This function calculates the FFT of a signal and returns the frequency domain
#' representation.
#'
#' @param x Numeric vector or matrix of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param ... Additional arguments to pass to \code{\link[stats]{fft}}.
#'
#' @return A data frame with two columns: 'fur' representing the Fourier
#' coefficients and 'freq' representing the corresponding frequencies.
#'
#' @examples
#' fft_result <- fastfourier(signal, samp.freq = 1000)
#'
#' @export
fastfourier <- function(x, samp.freq, ...) {
  if (!is.null(dim(x))) {
    f <- apply(x, 2, function(y) {
      fastfourier.core(y, samp.freq, ...)
    })
  } else{
    f <- fastfourier.core(x, samp.freq, ...)
  }
  return(f)
}

#' Internal function to compute the FFT of a signal.
#'
#' @param x Numeric vector of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#'
#' @return A data frame with 'fur' and 'freq' columns.
#'
#' @importFrom stats fft
#' @import units
#'
#' @noRd
fastfourier.core <- function(x, samp.freq, ...) {
  if ("units" %in% class(x)) {
    units(x) <- NULL
  }
  N <- length(x)
  fk <- fft(x)
  fk <- fk[2:length(fk) / 2 + 1]
  fk <- 2 * fk[seq(1, length(fk), by = 2)] / N
  freq <- (1:(length(fk))) * samp.freq / (2 * length(fk))
  return(data.frame(fur = fk, freq = freq))
}
