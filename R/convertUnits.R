#' Measurement Units Conversion (volume, weight and length)
#'
#' The \code{convertUnits} function is used to convert imperial units to international, and vice versa.
#' It takes as input a data frame which contains values and units in volume, weight and height,
#' and converts those units to the desired system.
#' @param data A dataframe
#' @param conversion_direction A string to define conversion direction
#'    (imperial_to_international or international_to_imperial)
#'
#' @return A new dataframe convert to either imperial or international units
#' @export
#'
#' @examples
#' convertUnits(cocktails, "imperial_to_international")
convertUnits <- function(data, conversion_direction) {
  # Define conversion factors
  imperial_to_international <- c(29.5735, 3785.41, 946.353, 568, 0.453592, 946.353, 2.54)
  international_to_imperial <- c(0.033814, 0.33814, 3.3814, 33.814, 0.00220462, 2.20462, 0.393701)
  # Define units for conversion
  imperial_units <- c("oz", "gal", "qt", "pint", "lb", "quart", "inch")
  international_units <- c("cl", "ml", "dl", "l", "gr", "kg", "cm")

  # Perform conversion based on the specified direction
  if (conversion_direction == "imperial_to_international") {
    for (i in 1:nrow(data)) {
      for (j in 7:ncol(data)) {
        if (!is.na(data[i, j])) {
          parts <- stringr::str_match(data[i, j], "([0-9.]+)\\s*([a-zA-Z]+)")
          if (!is.null(parts) && length(parts) == 3 && parts[3] %in% imperial_units) {
            value <- as.numeric(parts[2])
            unit <- parts[3]
            if (unit %in% c("oz", "gal", "qt", "pint", "quart")) {
              # Convert volume to ml
              converted_value <- round(value * imperial_to_international[which(imperial_units == unit)], -1)
              new_unit <- "ml"
            } else if (unit %in% c("lb")) {
              # Convert mass to kg
              converted_value <- round(value * imperial_to_international[which(imperial_units == "lb")], 1)
              new_unit <- "kg"
            } else if (unit %in% c("inch")) {
              # Convert distance to cm
              converted_value <- round(value * imperial_to_international[which(imperial_units == "inch")], 1)
              new_unit <- "cm"
            } else {
              # Keep the original value if not in the specified units
              converted_value <- value
              new_unit <- unit
            }
            # Update the dataset
            data[i, j] <- paste(converted_value, new_unit)
          }
        }
      }
    }
  } else if (conversion_direction == "international_to_imperial") {
    for (i in 1:nrow(data)) {
      for (j in 7:ncol(data)) {
        if (!is.na(data[i, j])) {
          parts <- stringr::str_match(data[i, j], "([0-9.]+)\\s*([a-zA-Z]+)")
          if (!is.null(parts) && length(parts) == 3 && parts[3] %in% international_units) {
            value <- as.numeric(parts[2])
            unit <- parts[3]
            if (unit %in% c("cl", "ml", "dl", "l")) {
              # Convert volume to oz
              converted_value <- value * international_to_imperial[which(international_units == unit)]
              new_unit <- "oz"
            } else if (unit %in% c("gr", "kg")) {
              # Convert mass to lb
              converted_value <- value * international_to_imperial[which(international_units == unit)]
              new_unit <- "lb"
            } else if (unit %in% c("cm")) {
              # Convert distance to inch
              converted_value <- value * international_to_imperial[which(international_units == "cm")]
              new_unit <- "inch"
            } else {
              # Keep the original value if not in the specified units
              converted_value <- value
              new_unit <- unit
            }

            # Round the converted value to 1 to avoid having i.e. 10ml converted to 0 ounces
            rounded_value <- round(converted_value, 1)

            # Update the dataset
            data[i, j] <- paste(rounded_value, new_unit)
          }
        }
      }
    }
  } else {
    stop("Invalid conversion direction. Please specify 'imperial_to_international' or 'international_to_imperial'.")
  }

  # return the final version of the dataset
  return(data)
}
