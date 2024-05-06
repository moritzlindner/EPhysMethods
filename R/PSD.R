#' Calculate the Power Spectral Density (PSD) for Electrophysiological Data
#'
#' This method calculates the Power Spectral Density (PSD) of electrophysiological data
#' using a fast Fourier transform (FFT) and subsequent power calculations.
#'
#' @importFrom EPhysData GetData newEPhysData TimeTrace AverageFunction
#' @importFrom units set_units as_units deparse_unit
#' @param X An object of class `EPhysData` data.
#' @param bandpass A numeric vector of two elements specifying the lower
#'        and upper bounds of the frequency band of interest, expressed in Hz.
#' @return An object of class `EPhysData` containing the PSD and corresponding frequency vector.
#' @examples
#' require(EPhysData)
#' X<-makeExampleEPhysData()
#' ggEPhysData(X)
#' psd<-PSD(X)
#' ggEPhysData(psd)
#' @details
#' This method calculates the Power Spectral Density (PSD) of electrophysiological data
#' using a fast Fourier transform (FFT) and subsequent power calculations.
#' The data is first bandpass filtered to retain only frequencies of interest.
#' Then, a FFT is applied to the filtered data to transform it into the frequency domain.
#' The PSD is computed from the squared magnitude of the FFT results.
#'
#' The power spectral density is computed using the formula:
#' \deqn{PSD(f) = 2 \times \frac{1}{T \times N} \times |X(f)|^2}
#' where |X(f)|^2 is the squared magnitude of the FFT, \eqn{T} is the sampling period,
#' and \eqn{N} is the number of points in the FFT.
#' The factor of 2 accounts for the loss of power in the negative frequency components
#' which are not included in the single-sided PSD.
#'
#' @exportMethod PSD
setGeneric(
  name = "PSD",
  def = function(X,
                 bandpass = as_units(c(.5, 300), "Hz")) {
    standardGeneric("PSD")
  }
)
#' @noMd
setMethod(
  "PSD",
  signature = "EPhysData",
  definition = function(X,
                        bandpass = as_units(c(.5, 300), "Hz")) {
    bandpass <-
      convert_to_unit(bandpass, "s")
    cutoff <-
      freq.to.w(x = bandpass, time.trace <-
                  TimeTrace(X))
    sample.rate <- mean(diff(TimeTrace(X)))
    sample.rate <- set_units(sample.rate, "s")

    dat <- GetData(X, Raw = T)
    dat <- filter.bandpass(dat, cutoff[1], cutoff[2])
    fft <- fastfourier(dat, samp.freq = 1 / sample.rate)
    fft_short <- lapply(fft, function(x) {
      x$freq <- set_units(x$freq, "Hz")
      x$fur <- Mod(x$fur)
      x$fur <- as_units(x$fur, deparse_unit(dat))
      x <-
        x[x$freq < bandpass[2] * 0.75,] # flicker freq can only be detected if at least three fourth the max bandpass frequency
      x <-
        x[x$freq > max(bandpass[1] * 2, set_units(1 / max(TimeTrace(X)), "Hz") *
                         2),] # flicker freq can only be detected if at least double the min bandpass frequency and if at least two peaks fit into one trace (Niquist)
      x$freq_rounded <- round(x$freq)
      averaged_data <-
        aggregate(fur ~ freq_rounded, data = x, FUN = mean)
      names(averaged_data)[names(averaged_data) == "freq_rounded"] <-
        "freq"
      x <- averaged_data
      x
    })
    fur <- lapply(fft_short, function(x) {
      x$fur
    })
    freq <- fft_short[[1]]$freq

    psd <- lapply(fur, function(x) {
      2 * (1 / (set_units(1 / sample.rate , "Hz") * length(x)) * x ^ 2) #https://de.mathworks.com/help/signal/ug/power-spectral-density-estimates-using-fft.html
    })
    psd.units <- deparse_unit(psd[[1]])
    psd <- do.call(cbind, psd)
    psd <- as_units(psd, psd.units)

    out <- newEPhysData(psd, freq)
    AverageFunction(out) <- mean
    out
  }
)
