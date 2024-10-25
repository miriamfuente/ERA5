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
  dataset <- loadGridData(df, var="t2mx", years=year)
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

############################### HOTTEST MONTH
# Aggregation of each month
monthly_means <- list()
for (i in 1:12){
  month.subset <- subsetGrid(tmax.total, season=i)
  tmax.month.aggr <- climatology(month.subset)
  monthly_means[[i]] <- tmax.month.aggr
}
tmax.aggr.year <- do.call(bindGrid, c(monthly_means, list(dimension = "time")))

#######################################################################################################
# Extract the data
temp_data <- tmax.aggr.year$Data
# Apply the function which.max() in the "time" dimension and obtain the index of the maximum value
hottest.month.data <- apply(temp_data, c(2, 3), which.max)

# Input the data into an object
hottest.month <- tmax.aggr.year
hottest.month$Data <- hottest.month.data
attr(hottest.month$Data,"dimensions")<- c("lat", "lon")

# Apply the mask land-sea
mask <- readRDS("mask.lsm.rds")
hottest.month.masked <- gridArithmetics(hottest.month, mask, operator="*")

saveRDS(hottest.month.masked, "hottest.month.masked.rds", compress="xz")

hottest.month.masked <- readRDS("hottest.month.masked.rds")

my_palette <- colorRampPalette(c("yellow",  "red",  "orange", "lightyellow"))(12)

png("hottest.month.masked.png", width = 1000, height = 600)
spatialPlot(hottest.month.masked, backdrop.theme = "coastline", color.theme ="RdBu", rev.colors = TRUE, 
          main = "Hottest month", color.key = TRUE, set.min = 1, set.max = 13, at = seq(1, 13, 1)) 
dev.off()

png("hottest.month.masked1.png", width = 1000, height = 600)
spatialPlot(hottest.month.masked, backdrop.theme = "coastline", col.regions =my_palette,
          main = "Hottest month", color.key = TRUE, set.min = 1, set.max = 13, at = seq(1, 13, 1)) 
dev.off()

################################################################################################################
# Extract the data
temp_data <- tmax.aggr.year$Data

# Apply the function which.max() in the "time" dimension and obtain the index of the maximum value
hottest.month.data <- apply(temp_data, c(2, 3), function(x) {
  # Get the indices of the two highest values
  sorted_indices <- order(x, decreasing = TRUE)
  hottest <- sorted_indices[1]  # index of the hottest month
  second_hottest <- sorted_indices[2]  # index of the second hottest month
  return(c(hottest, second_hottest))  # return both indices
})

# Create separate objects for hottest and second hottest months
hottest_month_indices <- hottest.month.data[1,,]  # Indices of hottest months
second_hottest_month_indices <- hottest.month.data[2, , ]  # Indices of second hottest months

# Input the data into an object
hottest.month <- tmax.aggr.year
hottest.month$Data <- hottest_month_indices
attr(hottest.month$Data, "dimensions") <- c("lat", "lon")

# Create a new object for second hottest months
second_hottest.month <- tmax.aggr.year
second_hottest.month$Data <- second_hottest_month_indices
attr(second_hottest.month$Data, "dimensions") <- c("lat", "lon")

# Apply the mask land-sea
mask <- readRDS("mask.lsm.rds")
hottest.month.masked <- gridArithmetics(hottest.month, mask, operator="*")
second_hottest.month.masked <- gridArithmetics(second_hottest.month, mask, operator="*")

difference <- gridArithmetics(hottest.month.masked, second_hottest.month.masked, operator="-")

# Save the results
saveRDS(hottest.month.masked, "hottest.month.masked.rds", compress="xz")
saveRDS(second_hottest.month.masked, "second.hottest.month.masked.rds", compress="xz")


png("difference.hottest.second.png", width = 1000, height = 600)
spatialPlot(difference, backdrop.theme = "coastline", color.theme ="RdBu", rev.colors = FALSE, 
          main = "Difference betweeen the hottest month and the second one", color.key = TRUE, at = seq(-12, 12, 1))
dev.off()

