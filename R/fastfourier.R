#' Compute the Fast Fourier Transform (FFT) of a signal.
#'
#' This function calculates the FFT of a signal and returns the frequency domain
#' representation.
#'
#' @param x Numeric vector or matrix of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param ... Additional arguments to pass to \code{\link[stats]{fft}}.
#' @importFrom units units set_units
#'
#' @return A data frame with two columns: 'fur' representing the Fourier
#' coefficients and 'freq' representing the corresponding frequencies.
#'
#' @examples
#' signal<-sin((1:1000)/33)*sin((1:1000)/20)
#' plot(signal)
#' fft_result <- fastfourier(signal, samp.freq = as_units(1000,"Hz"))
#' plot(fft_result$freq[1:20],abs(fft_result$fur[1:20]))
#' @export
fastfourier <- function(x, samp.freq, ...) {
  if ("units" %in% class(x)) {
    units(x) <- NULL
  }

  convertibel.to.Hz <- tryCatch({
    set_units(samp.freq, "Hz")
    TRUE
  }, error = function(e) {
    FALSE
  })
  if ("units" %in% class(samp.freq)) {
    if (!convertibel.to.Hz) {
      stop("'samp.freq' must be of convertible to Hertz")
    }
    samp.freq<-set_units(samp.freq, "Hz")
    units(samp.freq) <- NULL
  }

  if (!is.null(dim(x))) {
    f <- apply(x, 2, function(y) {
      fastfourier.core(y, samp.freq, ...)
    })
  } else{
    f <- fastfourier.core(x, samp.freq, ...)
    f$freq<-as_units(f$freq,"Hz")
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
#' @importFrom units units
#'
#' @noRd
fastfourier.core <- function(x, samp.freq, ...) {
  if ("units" %in% class(x)) {
    units(x) <- NULL
  }
  if ("units" %in% class(samp.freq)) {
    units(samp.freq) <- NULL
  }
  N <- length(x);
  fk <- fft(x, ...);
  fk <- fk[2:length(fk) / 2 + 1];
  fk <- 2 * fk[seq(1, length(fk), by = 2)] / N;
  freq <- (1:(length(fk))) * samp.freq / (2 * length(fk));
  return(data.frame(fur = fk, freq = freq));
}
