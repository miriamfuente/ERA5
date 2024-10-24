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

# Function to load and aggregate data by month
process_year <- function(year) {
  # Load the data by year
  dataset <- loadGridData(df, var="tp", years=year)
  # Convert meters to milimeters
  dataset <- gridArithmetics(dataset, 1000, operator="*")
  # Aggregate by month
  pr.monthly <- aggregateGrid(grid = dataset, aggr.m = list(FUN = "sum", na.rm = TRUE))
  return(pr.monthly)
}

# Call the function for each year
list_of_years <- lapply(years, process_year)
# Bind the data in the time dimension
pr.total <- do.call(bindGrid, c(list_of_years, list(dimension = "time")))
saveRDS(pr.total, "pr.monthly.rds", compress="xz")

pr.total <- readRDS("pr.monthly.rds")


############################### P50
# Aggregation of each month
monthly_quantile <- list()
for (i in 1:12){
  month.subset <- subsetGrid(pr.total, season=i)
  pr.month.q <- climatology(month.subset, clim.fun = list(FUN = "quantile", probs = 0.5))
  monthly_quantile[[i]] <- pr.month.q
}
pr.q50 <- do.call(bindGrid, c(monthly_quantile, list(dimension = "time")))
saveRDS(pr.q50, "pr.q50.rds", compress="xz")

hottest.month <- readRDS("hottest.month.masked.rds")
pr.q50 <- readRDS("pr.q50.rds")

final_pr <- array(NA, dim = dim(hottest.month$Data))
a <- dim(final_pr)[2]
b <- dim(final_pr)[3]

for (i in 1:a){
  for (j in 1:b){
    month <- hottest.month$Data[1,i,j]
    pr.month <- pr.q50$Data[,i,j]
    final_pr[1,i,j] <- pr.month[month]
  }
}

pr.q50.month <- hottest.month
pr.q50.month$Data <- final_pr

saveRDS(pr.q50.month, "pr.q50.hottest.month.rds", compress="xz")
