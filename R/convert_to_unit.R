#' Convert a variable to a specified unit
#'
#' This function attempts to convert a variable of class "units" to a specified unit.
#' It checks if the input variable has the correct class and handles errors during conversion.
#'
#' @param variable A variable of class "units" to be converted.
#' @param target_unit A string specifying the target unit to which the conversion should be attempted.
#' @importFrom units set_units
#'
#' @return Returns the variable converted to the specified unit.
#'
#' @examples
#' require(units)
#' my_variable <- set_units(1, "minutes")
#' tryCatch({
#'   converted_variable <- convert_to_unit(my_variable, "seconds")
#'   print(converted_variable)
#' }, error = function(e) {
#'   print(e$message)
#' })
#' @export
convert_to_unit <- function(variable, target_unit) {
  # Check if the input variable is of the correct class
  if (!inherits(variable, "units")) {
    stop("Input 'variable' must be of class 'units'.")
  }
  if (!inherits(target_unit, "character")) {
    stop("Input 'target_unit' must be of class 'character'.")
  }

  # Attempt to convert the units with error handling
  convertible <- tryCatch({
    set_units(variable, target_unit, mode = "standard")
    TRUE  # Return TRUE if conversion is successful
  }, error = function(e) {
    FALSE  # Return FALSE if an error occurs during conversion
  })

  # Check if the conversion was successful
  if (!convertible) {
    stop(paste("The variable cannot be converted to", target_unit, "."))
  }

  # Return the converted variable if successful
  return( set_units(variable, target_unit, mode = "standard"))
}
