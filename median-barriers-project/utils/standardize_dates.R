# ==============================
# Function: standardize_dates
# ==============================
# Purpose:
#   This function standardizes date strings by ensuring that the month 
#   is always two digits and the year is formatted as a four-digit year.
#   It processes a vector of date strings formatted as "m/yyyy" or "mm/yy"
#   and returns them in the standardized format "mm/yyyy".
#
# Parameters:
#   date_strings: A character vector containing date strings to be standardized.
#     Expected format is "m/yyyy", "mm/yyyy", or "m/yy".
#
# Returns:
#   A character vector of standardized date strings formatted as "mm/yyyy".
#
# Example Usage:
#   dates <- c("4/21", "10/22", "3/2022", "12/23")
#   standardized_dates <- standardize_dates(dates)
#   print(standardized_dates)  # Outputs: c("04/2021", "10/2022", "03/2022", "12/2023")
#
# ==============================
standardize_dates <- function(date_strings) {
  standardized_dates <- sapply(date_strings, function(date) {
    # Split the date string by "/"
    parts <- strsplit(date, "/")[[1]]
    
    # Check and modify the month part
    if (nchar(parts[1]) == 1) {
      parts[1] <- paste0("0", parts[1])  # Add "0" in front of single-digit month
    }
    
    # Check and modify the year part
    if (nchar(parts[2]) == 2) {
      parts[2] <- paste0("20", parts[2])  # Add "20" in front of two-digit year
    }
    
    # Return the modified date string in "mm/yyyy" format
    return(paste(parts, collapse = "/"))
  })
  
  return(standardized_dates)
}
