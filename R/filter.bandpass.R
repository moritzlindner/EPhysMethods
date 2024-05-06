#' Filter a signal with a bandpass filter.
#'
#' This function filters a signal using a forth order Butterworth band pass filter with
#' specified low and high frequency cutoffs. This function uses the \link[signal:butter]{signal::butter} function from the \link[signal:signal]{signal::signal} package.
#'
#' @param x Numeric vector or matrix of signal data.
#' @param low,high Numeric, Lower and upper filter bands, passed on as \code{W} to the \link[signal:butter]{signal::butter} function. Use \link{freq.to.w} to convert temporal frequencies into the required format.
#' @import units
#'
#' @return Filtered signal with the same dimensions as input 'x'.
#' @details
#' * For Mouse ERG recordings, low=1 and high=300 is recommended (https://open.fau.de/items/2548a085-59c9-45f7-9136-02a85b17ea51)
#' * For Mouse VEP recordings, low=0.5 and high=100 is recommended (doi:10.1016/j.visres.2005.09.006)
#' @examples
#' signal<-sin((1:1000)/30)*sin((1:1000)/60)
#' plot(signal)
#' filtered_signal <- filter.bandpass(signal, freq.to.w(x=6,samp.freq=1000), freq.to.w(x=20,samp.freq=1000))
#' plot(filtered_signal)
#' @seealso \link[signal:butter]{signal::butter}
#'
#' @docType Function
#' @export filter.bandpass
filter.bandpass <- function(x, low, high) {
  # Check if input parameters are numeric
  if (!all(sapply(list(x, low, high), is.numeric))) {
    stop("All input parameters must be numeric.");
  }

  un <- NULL
  if ("units" %in% class(x)) {
    un <- units(x);
    units(x) <- NULL;
  }
  if ("units" %in% class(x)) {
    units(low) <- NULL;
    warning("Unit of 'low' is ignored.")
  }
  if ("units" %in% class(high)) {
    units(high) <- NULL;
    warning("Unit of 'high' is ignored.")
  }

  # Check if the low and high frequencies are in the correct range
  if (low >= high || low < 0 || high > 1) {
    stop("Invalid low or high frequency values.");
  }

  if (is.null(dim(x))) {
    out <- filter.bandpass.core(x, low, high);
  } else{
    out <- apply(x, 2, function(x) {
      filter.bandpass(x, low, high);
    })
  }
  if (!is.null(un)) {
    return(as_units(out, un));
  } else{
    return(out);
  }
}

#' Core bandpass filter function.
#'
#' This function performs the core bandpass filtering operation using a
#' Butterworth filter.
#'
#' @param x Numeric vector of signal data.
#' @param samp.freq Numeric scalar, the sampling frequency in Hertz.
#' @param low Numeric scalar, the lower cutoff frequency W.
#' @param high Numeric scalar, the upper cutoff frequency W.
#'
#' @return Filtered signal.
#'
#' @importFrom signal butter filter
#' @keywords internal
#' @noRd
filter.bandpass.core <- function(x, low, high) {
  bf <- butter(4, c(low, high), type = "pass",plane="z");
  #filter.order<-200
  #bf <- fir1(filter.order, c(low, high), type = "pass")
  #delay <- filter.order/2
  x.filtered <- as.numeric(filter(bf, x))
  #print(head(x.filtered))
  #x.filtered <- c(x.filtered[(delay+1):length(x.filtered)], rep(NA, delay))
  return(x.filtered);

  # https://cran.r-project.org/web/packages/gsignal/vignettes/gsignal.html
}
