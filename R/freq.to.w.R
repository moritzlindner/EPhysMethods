#' Convert Frequency to 'Angular' Frequency (input to signalbutterworth filter)
#'
#' This function converts a frequency to 'angular' frequency \code{W}, as required by \link[signal:butter]{signal::butter}. Note this is not truly an angular frequency but in fact 'x / (samp.freq / 2)'.
#'
#' @param x Numeric vector. Frequency values to be converted. If no units (\link[units:units]{units::units}) are provided, assumes Herz.
#' @param time.trace Numeric vector of at least length 2 representing a time trace to calculate the sample frequency from. Must be of class 'units' (\link[units:units]{units::units}), with a unit convertible to 's'.
#' @param samp.freq Numeric. Sample frequency in Hz, or, if of class 'units' convertibel into 'Hz'.
#'
#' @return Numeric vector of the converted angular frequencies.
#'
#' @details This function checks if the input units are correct and converts the given frequency values to angular frequency using the provided time values or sample frequency. The function understands units (\link{units::units}), so values can be provided with units.
#'
#' If both \code{time.trace} and \code{samp.freq} are provided, an error is thrown.
#'
#' If only \code{time.trace} is provided, the function calculates the sample frequency from it. If only \code{samp.freq} is provided, it is directly used.
#'
#' @importFrom units set_units as_units drop_units
#' @examples
#' freq.to.w(x = 10, samp.freq = 100) # Returns 0.314159265358979
#' require(units)
#' freq.to.w(x = 10, time.trace = as_units(c(0, 1, 2),"s")) # Returns 3.14159265358979
#'
#' @export freq.to.w
freq.to.w <- function(x, time.trace = NULL, samp.freq = NULL) {
  if (!(xor(is.null(time.trace), is.null(samp.freq)))) {
    stop("Only one of 'time.trace' and 'samp.freq' may be provided")
  }

  if(!is.null(samp.freq)){ # samp.freq mode.
    if ("units" %in% class(samp.freq)) {
      tryCatch({
        set_units(samp.freq, "Hz")
      }, error = function(e) {
        stop("Unit of 'samp.freq' must convertible to 'Hz'.")
      })
    } else{
      samp.freq<-as_units(samp.freq, "Hz")
    }
  }

  if(!is.null(time.trace)){ # Time Trace mode
    if (length(time.trace) <= 1) {
      stop("'time.trace' must have at least length 2.")
    }
    # check units are correct
    if (!("units" %in% class(time.trace))) {
      stop("time.trace must be of class 'units'.")
    }
    if ("units" %in% class(time.trace)) {
      tryCatch({
        set_units(time.trace, "s")
      }, error = function(e) {
        stop("Unit of 'time.trace' must convertible to 's'.")
      })
    }
    tt <- drop_units(time.trace)
    tt.var <-
      max(abs(range(diff(tt)) - mean(diff(tt)))) / mean(diff(tt))
    if (tt.var> 0.1) {
      # if max deviation in time trace > 10 %
      stop(
        "Values inside the time vector 'time.trace' deviate by > 10% from their mean. Can't calculate sample frequency."
      )
    }
    # set samp.freq
    samp.freq <- set_units(mean(diff(time.trace)), "Hz")
  }

  if ("units" %in% class(x)) {
    tryCatch({
      x<-set_units(x, "Hz")
    }, error = function(e) {
      stop("Unit of 'x' must convertible to 'Hz' or must be none.")
    })
  }

  return(drop_units(x / (samp.freq / 2)))

}

