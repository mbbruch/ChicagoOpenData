library(data.table)

folder <- "C:\\Users\\Matthew\\Desktop\\Data Analysis\\LandUseDemo\\"

availability <- st_read(
     paste0(folder,"TransitAvailability2017CMAP201706\\TransitAvailability_2017_CMAP_201706.shp"))
availability <- st_transform(availability, "+proj=longlat +ellps=WGS84 +datum=WGS84")


write.csv(availability"C:\\Users\\Matthew\\Desktop\\Data Analysis\\LandUseDemo\\out.txt",)