# ==============================
# Function: process_and_name_xlsx
# ==============================
# Purpose:
#   Create a dynamic variable name based on a specified input string extracted 
#   from the filename and an additional suffix. The function processes the specified 
#   Excel file's sheets and assigns the combined data frame to the constructed 
#   name in the global environment.
#
# Parameters:
#   xlsx_file: The path to the Excel file to be processed.
#   post_string: A string to be appended to the extracted part of the input string.
#   exclude_sheets: A character vector of sheet names to exclude from processing.
#
# Returns:
#   A string representing the constructed variable name.
#
# Example Usage:
#   output_name <- process_and_name_xlsx("data/raw/2024-10-26_d2_random_raw.xlsx", "test")
#   print(output_name)  # Outputs: "random_test"
#   print(ls())         # Check if "random_test" has been created as a dataframe.
#
# ==============================

process_and_name_xlsx <- function(xlsx_file, post_string, exclude_sheets = c("MedianTypes")) {
  # Extract the base name from the file path
  file_name <- basename(xlsx_file)
  
  # Split the filename by underscore
  parts <- strsplit(file_name, "_")[[1]]
  
  # Extract the relevant part
  relevant_part <- parts[3]  # Ensure this index is correct for your needs
  
  # Construct the output name by appending post_string
  output_name <- paste0(relevant_part, "_", post_string)
  
  # Read in xlsx sheets
  sheets <- readxl::excel_sheets(xlsx_file)
  
  # Filter out the sheets to exclude
  selected_sheets <- sheets[!sheets %in% exclude_sheets]
  
  # Read the selected sheets into a list of data frames
  data_frames <- lapply(selected_sheets, function(sheet) {
    readxl::read_xlsx(xlsx_file, sheet = sheet)
  }) 
  
  # Combine all data frames into one using bind_rows from dplyr, adding a column for sheet name
  combined_df <- dplyr::bind_rows(data_frames, .id = "Sheet") %>%
    dplyr::filter(!is.na(MedianType))  # Filter out NA's
  
  # Dynamically assign the combined data frame to the specified variable name in the parent environment
  assign(output_name, combined_df, envir = .GlobalEnv)
  
  # Print the output name to confirm the assignment
  print(paste("Created object:", output_name))
  
  return(output_name)
}

# Example Usage:
# output_name <- process_and_name_xlsx("data/raw/2024-10-26_d2_random_raw.xlsx", "test")
# print(output_name)  # Outputs: "random_test"
