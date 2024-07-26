#' Identify outlier observations based on absolute threshold
#'
#' This function rejects entire columns in a matrix of class units if any value
#' in the column is above or below a specified absolute threshold.
#'
#' @inheritParams autoreject.by.signalfree
#' @param threshold A single numeric value of class units specifying the threshold for rejection.
#' @param dir A string value indicating whether to reject columns with measurements above ("above") or below ("below") the specified threshold, or with an absolute value above the threshold ("both").
#'
#' @return A logical vector indicating which observations to reject.
#'
#' @details The function iterates through each column of the matrix 'x' and marks
#' columns for rejection if any measurement in the column is above or below the specified threshold,
#' based on the 'dir' parameter.
#'
#' @examples
#' require(units)
#' data <- set_units(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3), "m")
#' result <- autoreject.by.absolute.threshold(data, threshold = set_units(5, "m"), dir = "both")
#'
#' @importFrom units drop_units
#' @importFrom units set_units
#' @export
autoreject.by.absolute.threshold <-
  function(x, threshold, dir) {
    if ("units" %in% class(x)) {
      if (!("units" %in% class(threshold))) {
        stop("If 'x' is of class 'units', 'threshold also has to be of that class. '")
      }
      tryCatch(
        threshold <- set_units(threshold, units(x), mode = "standard"),
        error = function(e) {
          stop("Unit conversion faild with error message: ", e)
        }
      )
      x <- drop_units(x)

      threshold <- drop_units(threshold)

    }

    # Convert threshold to the same unit as x for comparison

    # Function to check if a column should be rejected
    reject_function <- function(column, threshold, dir) {
      range_col <- range(column)
      if (dir == "above") {
        out <- max(range_col) > threshold
      }
      if (dir == "below") {
        out <- min(range_col) < threshold
      }
      if (dir == "both") {
        out <- max(abs(range_col)) > threshold
      }
      out
    }

    # Apply the rejection function to each column of the matrix
    reject <-
      apply(x, 2, function(y) {
        reject_function(y, threshold = threshold, dir = dir)
      })

    return(reject)
  }
