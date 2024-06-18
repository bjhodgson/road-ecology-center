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




# Initialize a variable to store the objectID of the record found
OBJECTID_annl_nc_above_2 <- NA

# Loop through the dataframe to find the first record where annl_nc > 2
for (i in 1:nrow(fences_df)) {
  if (!is.na(fences_df$annl_nc[i]) && fences_df$annl_nc[i] > 2) {
    # Found the first record where annl_nc > 2
    OBJECTID_annl_nc_above_2 <- fences_df$OBJECTID[i]
    break  # Exit the loop once found
  }
}

# Output the objectID of the record where annl_nc > 2
print(paste("ObjectID of record where annl_nc > 2:", OBJECTID_annl_nc_above_2))


# Initialize an empty vector to store OBJECTIDs
OBJECTIDs_vcu_zero <- c()

# Loop through the dataframe to find records where VCU == 0
for (i in 1:nrow(fences_df)) {
  if (!is.na(fences_df$VCU[i]) && fences_df$VCU[i] == 0) {
    # Found a record where VCU == 0
    OBJECTIDs_vcu_zero <- c(OBJECTIDs_vcu_zero, fences_df$OBJECTID[i])
  }
}

# Output the OBJECTIDs where VCU == 0
print(paste("OBJECTIDs where VCU == 0:", toString(OBJECTIDs_vcu_zero)))

#___________________________


# Read the CSV file
fences_df <- read.csv("D:\\hotspots\\fences\\output_data\\wcc_bridges.csv")

# Initialize variables to store OBJECTIDs
objectID_vcu_zero <- vector("numeric", 2)  # Assuming OBJECTID is numeric, change accordingly if different

# Initialize a counter for the number of VCU == 0 records found
count_vcu_zero <- 0

# Loop through the dataframe to find the first record where annl_nc > 2 and record OBJECTID
for (i in 1:nrow(fences_df)) {
  if (!is.na(fences_df$annl_nc[i]) && fences_df$annl_nc[i] > 2) {
    # Start counting records above this point
    for (j in seq(i-1, 1, -1)) {
      if (!is.na(fences_df$VCU[j]) && fences_df$VCU[j] == 0) {
        # Found a record with VCU == 0 above annl_nc > 2
        count_vcu_zero <- count_vcu_zero + 1
        if (count_vcu_zero <= 2) {
          objectID_vcu_zero[count_vcu_zero] <- fences_df$OBJECTID[j]
        }
        if (count_vcu_zero == 2) {
          break
        }
      }
    }
    
    # Break the outer loop if both OBJECTIDs are found
    if (count_vcu_zero == 2) {
      break
    }
  }
}

# Output the OBJECTIDs of the first two records where VCU == 0
print(paste("OBJECTIDs of the first two records where VCU == 0:", toString(objectID_vcu_zero)))

print(paste("Records above annl_nc > 2:", count_records_above))
print(paste("Records below annl_nc > 2 until VCU == 0:", count_records_below))

