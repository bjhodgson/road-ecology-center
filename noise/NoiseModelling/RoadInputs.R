library(sf)
library(dplyr)

roadways_path <- "D:\\noise_modelling\\I8 Study Area\\input_data\\CA_HPMS_TMC2017_I8_study\\CA_HPMS_TMC2017_I8_study.shp"

# Read in shapefile
roadways <- st_read(roadways_path)
# Remove m dimension from geometry
roadways$geometry <- st_zm(roadways$geometry, drop = TRUE)

# Set roadway parameters
traffic_speed = 105 # kilometers/hour
elevation = 200 # UPDATE ROADWAYS DATA TO PULL FROM

# Create a new data frame and add the MultiLineString geometries
ROAD <- st_geometry(roadways) %>%
  as.data.frame() %>%
  setNames("THE_GEOM") %>%
  mutate(PK = row_number(), 
         TV_D = roadways$AADT/24, 
         TV_E = roadways$AADT/24,
         TV_N = roadways$AADT/24,
        
         TV_SPD_D = traffic_speed, 
         TV_SPD_E = traffic_speed,
         TV_SPD_N = traffic_speed,
         
         SLOPE = elevation
      )

output_path <- "D:\\noise_modelling\\I8 Study Area\\output_data\\ROAD\\ROAD.shp"

st_write(ROAD, output_path, append = FALSE)
