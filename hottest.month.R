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

png("hottest.month.masked.png", width = 1000, height = 600)
spatialPlot(hottest.month.masked, backdrop.theme = "coastline", color.theme = "jet.colors", rev.colors = TRUE, 
          main = "Hottest month", color.key = TRUE, at = seq(1, 12, 1)) 
dev.off()

