###########################################################################################
######################################## LOAD DATA ########################################
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

# Load land-sea mask
mask.path <- "/lustre/gmeteo/PTICLIMA/DATA/REANALYSIS/ERA5/lsm/lsm_era5.nc"
mask <- loadGridData(mask.path, var="lsm", latLim=c(-60,90))

# Values under 0.5 are considered as sea
mask$Data[mask$Data < 0.5] <- 0
mask$Data[mask$Data >= 0.5] <- 1
# Values of sea are set to NA
mask$Data[mask$Data == 0] <- NA

saveRDS(mask, "mask.lsm.rds", compress="xz")

# Show the result
png("mask.png", width = 800, height = 600)
spatialPlot(mask, backdrop.theme = "coastline", color.theme="YlOrRd", rev.colors = FALSE, 
            main= "Land-sea mask")
dev.off()


# Varibles Data 
df <- "/lustre/gmeteo/PTICLIMA/DATA/REANALYSIS/ERA5/data/global/0.25/ERA5_025.ncml"
di.obs <- dataInventory(df) # Variables: precipitation = tp, temperature_max=t2mx
years = 2000
dataset <- loadGridData(df, var="t2mx", latLim=c(-60,90), years=years)
dataset <- gridArithmetics(dataset, 273.15, operator="-")

# Show the result without mask
png("tmax.no.lsm.png", width = 800, height = 600)
spatialPlot(climatology(dataset), backdrop.theme = "coastline", color.theme="YlOrRd", rev.colors = FALSE, 
            main= "Daily maximum temperature")
dev.off()
