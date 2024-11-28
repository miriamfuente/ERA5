options(java.parameters = "-Xmx32g") 
# Libraries needed for the script
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # needed for the biasCorrection (function biasCorrection)
library(climate4R.indices) # needed for calculate indices

# Variables Data
df <- "/lustre/gmeteo/PTICLIMA/DATA/REANALYSIS/ERA5/data/global/0.25/ERA5_025.ncml"
mask.path <- "/lustre/gmeteo/PTICLIMA/DATA/REANALYSIS/ERA5/lsm/lsm_era5.nc"
hottest.month <- readRDS("hottest.month/idx.hottest.month.masked.rds")

# latitude, longitude and years of the region of interest
latitude <- c(35.9, 44)
longitude <- c(-9.6, 3.6)
years = 1986:2005
# tp for precipitation
# tx2m for maximum temperature
dataset <- loadGridData(df, var="tp", latLim=latitude, lonLim=longitude, years=years)
dataset <- gridArithmetics(dataset, 1000, operator="*") # Convert to mm (Precipitation)
# dataset <- gridArithmetics(dataset, 273, operator="-") # Convert to Celsius (Temperature)
dataset$Data[dataset$Data < 0] <- 0

# Subset the hottest month for the region of interest
hottest.month.pi <- subsetGrid(hottest.month, latLim=latitude, lonLim=longitude)

# Create lists to store the datasets
list.lon <- list()
list.lat <- list()
list.total <- list()
# Define the months
meses <- 1:12

# Loop over the latitude and longitude to mantein the hottest month and set to NA the rest of the months 
# for the region of interest and keep the same structure of the original dataset
for (i in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lat")]) {
    dataset.lat <- subsetDimension(dataset, dimension="lat", indices=i)
  for (j in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lon")]) {
    dataset.lon <- subsetDimension(dataset.lat, dimension="lon", indices=j)
    month <- hottest.month.pi$Data[i, j]
    if (is.na(month)) {
      # If month is NA, assign NA to all values of dataset.lon$Data and continue
      dataset.lon$Data[!is.na(dataset.lon$Data)] <- NA  # Asigna NA a todos los elementos
      list.lon[[j]] <- dataset.lon
    } else {
      # Normal processing if month has a valid value
      dataset.no.hottest <- subsetGrid(dataset.lon, season=c(setdiff(meses, month)))
      dataset.no.hottest$Data[!is.na(dataset.no.hottest$Data)] <- NA
      dataset.hottest <- subsetGrid(dataset.lon, season=month)
      dataset.final <- bindGrid(dataset.no.hottest, dataset.hottest, dimension = "time")

      list.lon[[j]] <- dataset.final
    }
  }
  list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)

}
list.total <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

# Plot the mean of the dataset obtained
png("pruebapirineosymas.tmax.png", width = 800, height = 600)
spatialPlot(climatology(tmax), backdrop.theme = "coastline", color.theme="YlOrRd", rev.colors = FALSE, 
            main= "Daily maximum temperature")
dev.off()

# Save the dataset obtained
saveRDS(list.total, "pr.daily.hottest.month.pi.rds", compress="xz")

