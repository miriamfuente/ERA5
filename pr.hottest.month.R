###########################################################################################
################################ PRECIPITATION QUANTILE 50 ################################
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
hottest.month <- readRDS("hottest.month.masked.rds")

# Function to load and aggregate data by month
process_year <- function(year, hottest.month) {
  # Load the data by year
  dataset <- loadGridData(df, var="tp", years=year)
  # Convert meters to milimeters
  dataset <- gridArithmetics(dataset, 1000, operator="*")
  # Aggregate by month
  pr.monthly <- aggregateGrid(grid = dataset, aggr.m = list(FUN = "sum", na.rm = TRUE))
  var.monthly <- array(NA, dim = dim(hottest.month$Data))
  for (i in 1:dim(var.monthly)[2]){
    for (j in 1:dim(var.monthly)[3]){
          month <- hottest.month$Data[1,i,j]
          pr.month <- pr.monthly$Data[,i,j]
          var.monthly[1,i,j] <- pr.month[month]
    }
  }
  pr.hot.month <- climatology(pr.monthly)
  pr.hot.month$Data <- var.monthly
  attr(pr.hot.month$Data,"dimensions")<- c("time","lat", "lon")
  return(pr.hot.month)
}

# Call the function for each year
list_of_years <- lapply(years, process_year, hottest.month)

pr.final <- do.call(bindGrid, c(list_of_years, list(dimension = "time")))

saveRDS(pr.final, "pr.hot.month.1986.2005.rds", compress="xz")