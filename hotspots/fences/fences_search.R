library(readxl)
library(dplyr)

# Read the CSV file
fences_df <- read.csv("D:\\hotspots\\fences\\output_data\\wcc_bridges.csv")

# Initialize a variable to store the count of records above the found annl_nc > 2
count_records_above <- 0

# Initialize a variable to store the count of records below the found annl_nc > 2 until VCU == 0
count_records_below <- 0

# Initialize a flag to indicate if the first record with annl_nc > 2 is found
record_found_above <- FALSE

# Initialize a flag to indicate if a record with VCU == 0 is found after annl_nc > 2
record_found_below <- FALSE

# Loop through the dataframe to find the first annl_nc > 2 and count records above and below
for (i in 1:nrow(fences_df)) {
  
  # Check for annl_nc > 2
  if (!is.na(fences_df$annl_nc[i]) && fences_df$annl_nc[i] > 2) {
    
    # Start counting records above this point
    for (j in seq(i-1, 1, -1)) {
      count_records_above <- count_records_above + 1
      if (!is.na(fences_df$VCU[j]) && fences_df$VCU[j] == 0) {
        # Stop counting once a VCU == 0 record is found
        record_found_above <- TRUE
        break
      }
    }
    
    # Continue searching downwards from annl_nc > 2
    for (k in seq(i+1, nrow(fences_df))) {
      if (!is.na(fences_df$VCU[k]) && fences_df$VCU[k] == 0) {
        # Found a record with VCU == 0 below annl_nc > 2
        record_found_below <- TRUE
        break
      }
      count_records_below <- count_records_below + 1
    }
    
    # Break the outer loop if both records are found
    if (record_found_above && record_found_below) {
      break
    }
  }
}

# Output the counts of records above and below the found annl_nc > 2
print(paste("Records above annl_nc > 2:", count_records_above))
print(paste("Records below annl_nc > 2 until VCU == 0:", count_records_below))



# --------------------------

library(dplyr)

# Read the CSV file
fences_df <- read.csv("D:\\hotspots\\fences\\output_data\\wcc_bridges.csv")

# Find the first occurrence of annl_nc > 2
index_above <- which(fences_df$annl_nc > 2)[1]

if (!is.na(index_above)) {
  # Extract the OBJECTID of the first occurrence
  objectid_first_occurrence <- fences_df$OBJECTID[index_above]
  
  # Find the index of the first VCU == 0 above the found index
  index_vcu_above <- which(fences_df$VCU[1:(index_above-1)] == 0, arr.ind = TRUE)[1]
  
  # Find the index of the first VCU == 0 below the found index
  index_vcu_below <- index_above + which(fences_df$VCU[(index_above+1):nrow(fences_df)] == 0, arr.ind = TRUE)[1]
  
  # Output the OBJECTID and indices
  print(paste("OBJECTID of first occurrence of annl_nc > 2:", objectid_first_occurrence))
  print(paste("Index of first VCU == 0 above annl_nc > 2:", index_vcu_above))
  print(paste("Index of first VCU == 0 below annl_nc > 2:", index_vcu_below))
  
  # Count records above annl_nc > 2 until VCU == 0
  records_above <- fences_df[1:(index_above-1), ]
  count_records_above <- sum(records_above$VCU == 0, na.rm = TRUE)
  
  # Count records below annl_nc > 2 until VCU == 0
  records_below <- fences_df[(index_above+1):nrow(fences_df), ]
  count_records_below <- sum(cumsum(records_below$VCU == 0) == 0, na.rm = TRUE)
  
  # Output the counts of records above and below the found annl_nc > 2
  print(paste("Records above annl_nc > 2 until VCU == 0:", count_records_above))
  print(paste("Records below annl_nc > 2 until VCU == 0:", count_records_below))
} else {
  print("No records with annl_nc > 2 found")
}


#-----------------------

library(dplyr)

# Read the CSV file
fences_df <- read.csv("D:\\hotspots\\fences\\output_data\\wcc_bridges.csv")

# Find the first occurrence of annl_nc > 2
index_above <- which(fences_df$annl_nc > 2)[1]

if (!is.na(index_above)) {
  # Extract the OBJECTID of the first occurrence
  objectid_first_occurrence <- fences_df$OBJECTID[index_above]
  
  # Find the OBJECTID of the first VCU == 0 above the found index
  objectid_vcu_above <- fences_df$OBJECTID[which(fences_df$VCU[1:(index_above-1)] == 0, arr.ind = TRUE)[1]]
  
  # Find the OBJECTID of the first VCU == 0 below the found index
  index_vcu_below <- which(fences_df$VCU[(index_above+1):nrow(fences_df)] == 0, arr.ind = TRUE)[1]
  objectid_vcu_below <- fences_df$OBJECTID[index_above + index_vcu_below]
  
  # Count records above annl_nc > 2 until VCU == 0
  count_records_above <- sum(fences_df$VCU[1:(index_above-1)] == 0, na.rm = TRUE)
  
  # Count records below annl_nc > 2 until VCU == 0
  count_records_below <- sum(cumsum(fences_df$VCU[(index_above+1):nrow(fences_df)] == 0) == 0, na.rm = TRUE)
  
  # Output the OBJECTID and counts of records above and below the found annl_nc > 2
  print(paste("OBJECTID of first occurrence of annl_nc > 2:", objectid_first_occurrence))
  print(paste("OBJECTID of first VCU == 0 above annl_nc > 2:", objectid_vcu_above))
  print(paste("OBJECTID of first VCU == 0 below annl_nc > 2:", objectid_vcu_below))
  print(paste("Records above annl_nc > 2 until VCU == 0:", count_records_above))
  print(paste("Records below annl_nc > 2 until VCU == 0:", count_records_below))
} else {
  print("No records with annl_nc > 2 found")
}


#-------------

library(dplyr)

# Read the CSV file
fences_df <- read.csv("D:\\hotspots\\fences\\output_data\\wcc_bridges.csv")

# Find the first occurrence of annl_nc > 2
index_above <- which(fences_df$annl_nc > 2)[1]

if (!is.na(index_above)) {
  # Extract the OBJECTID of the first occurrence
  objectid_first_occurrence <- fences_df$OBJECTID[index_above]
  
  # Find the closest OBJECTID with VCU == 0 above the found index
  index_vcu_above <- which(fences_df$VCU[1:(index_above-1)] == 0)
  if (length(index_vcu_above) > 0) {
    objectid_vcu_above <- fences_df$OBJECTID[max(index_vcu_above)]
  } else {
    objectid_vcu_above <- NA
  }
  
  # Find the closest OBJECTID with VCU == 0 below the found index
  index_vcu_below <- which(fences_df$VCU[(index_above+1):nrow(fences_df)] == 0)
  if (length(index_vcu_below) > 0) {
    objectid_vcu_below <- fences_df$OBJECTID[index_above + min(index_vcu_below)]
  } else {
    objectid_vcu_below <- NA
  }
  
  # Count records above annl_nc > 2 until VCU == 0
  count_records_above <- if (!is.na(objectid_vcu_above)) {
    sum(fences_df$OBJECTID[1:(index_above-1)] <= fences_df$OBJECTID[max(index_vcu_above)], na.rm = TRUE)
  } else {
    0
  }
  
  # Count records below annl_nc > 2 until VCU == 0
  count_records_below <- if (!is.na(objectid_vcu_below)) {
    sum(fences_df$OBJECTID[(index_above+1):nrow(fences_df)] <= fences_df$OBJECTID[index_above + min(index_vcu_below)], na.rm = TRUE)
  } else {
    0
  }
  
  # Output the OBJECTID and counts of records above and below the found annl_nc > 2
  print(paste("OBJECTID of first occurrence of annl_nc > 2:", objectid_first_occurrence))
  print(paste("OBJECTID of closest VCU == 0 above annl_nc > 2:", objectid_vcu_above))
  print(paste("OBJECTID of closest VCU == 0 below annl_nc > 2:", objectid_vcu_below))
  print(paste("Records above annl_nc > 2 until VCU == 0:", count_records_above))
  print(paste("Records below annl_nc > 2 until VCU == 0:", count_records_below))
} else {
  print("No records with annl_nc > 2 found")
}
