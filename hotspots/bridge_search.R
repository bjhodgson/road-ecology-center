library(readxl)

points_100ft <- read_excel() %>%
  mutate(NearBridge)

points_100ft$

# Looping over the entire length of the data frame
df <- points_100ft
  
for (i in 1:nrow(df)) {
  row <- df[i, ]
  
  if row[[VCU]] == 0 {
    df[i+53, 'NearBridge'] = 1
    df[i-53, 'NearBridge'] = 1
    
    test
    
    
     }
  
 # else {df[i, 'NearBridge'] = 0
  
  }
}

