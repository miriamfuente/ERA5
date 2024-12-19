###########################################################################################
###################################### HOTTEST MONTH ######################################
###########################################################################################
# * Define the libraries to be used
options(java.parameters = "-Xmx128g") 
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # needed for the biasCorrection (function biasCorrection)
library(climate4R.indices) # needed for calculate indices

# Years to study
years <- 1986:2005
# Variables Data
df <- "/lustre/gmeteo/PTICLIMA/DATA/REANALYSIS/ERA5/data/global/0.25/ERA5_025.ncml"

# Function to load and aggregate data by month
process_year <- function(year) {
  # Load the data by year
  dataset <- loadGridData(df, var="t2mx", years=year, latLim=latitude, lonLim=longitude)
  # Convert Kelvin to Celsius
  dataset <- gridArithmetics(dataset, 273.15, operator="-")
  # Aggregate by month
  tasmax.monthly <- aggregateGrid(grid = dataset, aggr.m = list(FUN = "mean", na.rm = TRUE))
  return(tasmax.monthly)
}

# Call the function for each year
list_of_years <- lapply(years, process_year)
# Bind the data in the time dimension
tmax.total <- do.call(bindGrid, c(list_of_years, list(dimension = "time")))
saveRDS(tmax.total, "tmax.monthly.rds", compress="xz")

tmax.total <- readRDS("tmax.monthly.rds")

############################### HOTTEST MONTH ################################
# Aggregation of each month
monthly_means <- list()
for (i in 1:12){
  month.subset <- subsetGrid(tmax.total, season=i)
  tmax.month.aggr <- climatology(month.subset)
  monthly_means[[i]] <- tmax.month.aggr
}
tmax.aggr.year <- do.call(bindGrid, c(monthly_means, list(dimension = "time")))

#######################################################################################################
################## Extract the data of the hottest month only and apply the mask land-sea #############
#######################################################################################################
# Extract the data
temp_data <- tmax.aggr.year$Data
# Apply the function which.max() in the "time" dimension and obtain the index of the maximum value
idx.hottest.month.data <- apply(temp_data, c(2, 3), which.max)

# FOR THE DATSET ERA5-LAND
idx.hottest.month.data <- apply(tmax.aggr.year$Data, c(2, 3), function(x) {
    if (all(is.na(x))) return(NA)  # Devuelve NA si todos los valores son NaN
    which.max(x)
})

# Apply the function max() in the "time" dimension to obtain the maximum value
value.hottest.month.data <- apply(temp_data, c(2, 3), max)

# INDEX
idx.hottest.month <- tmax.aggr.year
idx.hottest.month$Data <- idx.hottest.month.data
attr(idx.hottest.month$Data,"dimensions")<- c("lat", "lon")

# VALUE
value.hottest.month <- tmax.aggr.year
value.hottest.month$Data <- value.hottest.month.data
attr(value.hottest.month$Data,"dimensions")<- c("lat", "lon")

# Apply the mask land-sea
mask <- readRDS("mask.lsm.rds")
idx.hottest.month.masked <- gridArithmetics(idx.hottest.month, mask, operator="*")
saveRDS(idx.hottest.month.masked, "idx.hottest.month.masked.rds", compress="xz")
value.hottest.month.masked <- gridArithmetics(value.hottest.month, mask, operator="*")
saveRDS(value.hottest.month.masked, "value.hottest.month.masked.rds", compress="xz")


hottest.month.masked <- readRDS("idx.hottest.month.masked.rds")
my_palette <- colorRampPalette(c("yellow",  "red",  "orange", "lightyellow"))(12)

png("hottest.month.era5land.png", width = 1000, height = 600)
spatialPlot(idx.hottest.month, backdrop.theme = "coastline", color.theme ="YlOrRd", rev.colors = FALSE, 
          main = "Hottest month", color.key = TRUE, set.min = 0.5, set.max = 12.5, at = seq(0.5, 12.5, 1),
          colorkey = list(space = "right",
                title = list("Month", cex = 1))) 
dev.off()

png("hottest.month.masked1.png", width = 1000, height = 600)
spatialPlot(idx.hottest.month.masked, backdrop.theme = "coastline", col.regions =my_palette,
          main = "Hottest month", color.key = TRUE, set.min = 0.5, set.max = 12.5, at = seq(0.5, 12.5, 1),
          colorkey = list(space = "right",
                title = list("Month", cex = 1))) 
dev.off()

#################################################################################################################
############################ Difference between the hottest month and the second one ############################
# Extract the data
temp_data <- tmax.aggr.year$Data

#                                      INDEX
ciclo <- c(1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12)
# Extract the data of the hottest month and the second hottest month
temp_data <- tmax.aggr.year$Data
idx.hottest.month.data <- apply(temp_data, c(2, 3), function(x) {
  sorted_indices <- order(x, decreasing = TRUE)
  hottest <- sorted_indices[1]  # index of the hottest month
  second_hottest <- sorted_indices[2]  # index of the second hottest month
  return(c(hottest, second_hottest))  # return both indices
})

# Create separate objects for hottest and second hottest months
hottest_month_indices <- idx.hottest.month.data[1,,]  # index of the hottest month
second_hottest_month_indices <- idx.hottest.month.data[2,,]  # index of the second hottest month

# Minimum difference in positions in the extended cycle
difference_min_positions <- apply(idx.hottest.month.data, c(2, 3), function(indices) {
  hottest <- indices[1]
  second_hottest <- indices[2]
  
  # Find the positions in the extended cycle
  pos_hottest <- which(ciclo == hottest)
  pos_second_hottest <- which(ciclo == second_hottest)
  
  # Calculate the absolute differences between all combinations of positions
  min(abs(outer(pos_hottest, pos_second_hottest, "-")))
})

# Create a new object to store the minimum difference in positions
difference_in_positions <- tmax.aggr.year
difference_in_positions$Data <- difference_min_positions
attr(difference_in_positions$Data, "dimensions") <- c("lat", "lon")

# Apply the land-sea mask
mask <- readRDS("mask.lsm.rds")
difference_in_positions_masked <- gridArithmetics(difference_in_positions, mask, operator="*")

png("difference.hottest.second.idx.png", width = 1000, height = 600)
spatialPlot(difference_in_positions_masked, backdrop.theme = "coastline", color.theme ="YlOrRd", rev.colors = FALSE, 
          main = "Difference betweeen the hottest month and the second one", set.min = 0.5, set.max = 6.5, at = seq(0.5, 6.5, 1),
          colorkey = list(space = "right",
                title = list("Months", cex = 1)))
dev.off()


#                                      VALUES / ºC
# Apply the function max() in the "time" dimension to obtain the maximum and second maximum values
value.hottest.month.data <- apply(temp_data, c(2, 3), function(x) {
  # Order the values in decreasing order to get the indices of the two highest values
  sorted_indices <- order(x, decreasing = TRUE)
  hottest_value <- x[sorted_indices[1]]  # Value of the hottest month
  second_hottest_value <- x[sorted_indices[2]]  # Value of the second hottest month
  return(c(hottest_value, second_hottest_value))  # return both values
})

# Create separate objects for hottest and second hottest months
hottest_month_values <- value.hottest.month.data[1,,]  #  hottest months
second_hottest_month_values <- value.hottest.month.data[2, , ]  # second hottest months

# Input the data into an object
hottest.month <- tmax.aggr.year
hottest.month$Data <- hottest_month_values
attr(hottest.month$Data, "dimensions") <- c("lat", "lon")

# Create a new object for second hottest months
second_hottest.month <- tmax.aggr.year
second_hottest.month$Data <- second_hottest_month_values
attr(second_hottest.month$Data, "dimensions") <- c("lat", "lon")

# Apply the mask land-sea
mask <- readRDS("mask.lsm.rds")
hottest.month.masked <- gridArithmetics(hottest.month, mask, operator="*")
second_hottest.month.masked <- gridArithmetics(second_hottest.month, mask, operator="*")

difference <- gridArithmetics(hottest.month.masked, second_hottest.month.masked, operator="-")

# Save the results
saveRDS(hottest.month.masked, "hottest.month.masked.rds", compress="xz")
saveRDS(second_hottest.month.masked, "second.hottest.month.masked.rds", compress="xz")

png("difference.hottest.second.values.png", width = 1000, height = 600)
spatialPlot(difference, backdrop.theme = "coastline", color.theme ="YlOrRd", rev.colors = FALSE, 
          main = "Difference betweeen the values of the hottest month and the second one", 
          set.min = 0, set.max = 6.5, at = seq(0, 6.5, 0.2),           
          colorkey = list(space = "right",
                title = list("º C", cex = 1)))
dev.off()
