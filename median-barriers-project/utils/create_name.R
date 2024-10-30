# ==============================
# Function: create_name
# ==============================
# Purpose:
#   Create a dynamic variable name based on a specified input string and 
#   an additional suffix. The function extracts a specific part of the 
#   input string, combines it with the provided suffix, and assigns 
#   an empty dataframe to that name in the global environment.
#
# Parameters:
#   input_string: A string containing underscores that can be split into parts.
#   post_string: A string to be appended to the extracted part of the input_string.
#
# Returns:
#   A string representing the constructed variable name.
#
# Example Usage:
#   output_name <- create_name("2024-10-26_d2_random_raw", "test")
#   print(output_name)  # Outputs: "random_test"
#   print(ls())         # Check if "random_test" has been created as a dataframe.
#
# ==============================

create_name <- function(input_string, post_string) {
  # Extract the base name from the file path
  file_name <- basename(input_string)
  
  # Split the filename by underscore
  parts <- strsplit(file_name, "[_.]")[[1]]
  print(parts)
  
  # Extract the relevant part
  relevant_part <- parts[3]  # Ensure this index is correct for your needs
  
  # Construct the output name by appending post_string
  output_name <- paste0(relevant_part, "_", post_string)
  
  # Dynamically assign an empty dataframe to the parsed output name
  assign(output_name, data.frame(), envir = .GlobalEnv)
  
  return(output_name)
}
