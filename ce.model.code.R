library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # needed for the biasCorrection (function biasCorrection)
library(climate4R.indices) # needed for calculate indices

source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.transform.data.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.quantiles.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.binarization.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.ce.frecuency.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.duration.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.severity.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.trends.frecuency.R")
source("/oceano/gmeteo/users/fuentesm/ERA5/functions/fun.freq.categories.R")


# Path
path <- "/oceano/gmeteo/users/fuentesm/ERA5/modelos.cordex.core/"
# ERA5-LAND data
hottest.month <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/obs.era5.land/hottest.month.pi.1986.2005.era5.land.rds")
pr.obs <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/data/basic/pr.daily.hottest.month.pi.era5.land.rds")
tmax.obs <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/data/basic/tmax.daily.hottest.month.pi.era5.land.rds")

# Model data used
gcm <- "HadGEM"
rcm <- "REMO"
wl <- list("+1.5_2008_2027", "+2_2028_2047", "+3_2052_2071", "+4_2072_2091")
racha = 2
years = list(2008:2027, 2028:2047, 2052:2071, 2072:2091)

# level.wl <- 1

for (i in 1:4){
    # Print warning level
    print(paste0("Warning level: ", i))
    level.wl <- i

    # Load the data
    pr <- readRDS(paste0(path, rcm, "/", gcm, "/pr.dd.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))
    tmax <- readRDS(paste0(path, rcm, "/", gcm, "/tmax.dd.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))

    mask <- tmax
    mask$Data[!is.na(mask$Data)] <- 1
    mask <- climatology(mask)

    print("Transforming data")
    pr.transformed <- transform.data(pr, hottest.month)
    tmax.transformed <- transform.data(tmax, hottest.month)

    pr.q50 <- fun.quantiles(pr.transformed, var="pr", quantile=0.5)
    tmax.q90 <- fun.quantiles(tmax.transformed, var="tmax", quantile=0.9)

    print("Binarization")
    pr.monthly <- aggregateGrid(pr.transformed, aggr.m = list(FUN = "sum", na.rm = FALSE))
    pr.bin <- fun.binarization(dataset=pr.monthly, hottest.month=hottest.month, quantile=pr.q50, var="pr")
    tmax.bin <- fun.binarization(dataset=tmax.transformed, hottest.month=hottest.month, quantile=tmax.q90, var="tmax")

    print("Frecuency")
    frecuency <- fun.ce.frecuency(hottest.month=hottest.month, tmax.bin=tmax.bin, pr.bin=pr.bin)
    saveRDS(frecuency, paste0(path,  rcm, "/", gcm, "/ce.frecuency.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))

    print("Trends")
    trends <- fun.trends.frecuency(ce=frecuency, hottest.month=hottest.month, mask=mask)
    saveRDS(trends, paste0(path,  rcm, "/", gcm, "/ce.trends.frecuency.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))

    print("Duration")
    duration <- fun.duration(frecuency, hottest.month, racha, years[[level.wl]])
    saveRDS(duration, paste0(path,  rcm, "/", gcm, "/ce.duration.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))

    print("Severity")
    severity <- fun.severity(hottest.month, pr.obs, tmax.obs, pr.transformed, tmax.transformed)
    saveRDS(severity, paste0(path,  rcm, "/", gcm, "/ce.severity.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))

    print("Categories")
    categories <- fun.categories(severity, hottest.month)
    saveRDS(categories, paste0(path,  rcm, "/", gcm, "/ce.categories.pi.", gcm, ".", rcm, ".rcp85.", wl[level.wl], ".rds"))
}

