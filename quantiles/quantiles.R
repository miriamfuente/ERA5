options(java.parameters = "-Xmx32g") 
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

pr.daily <- readRDS("pr.daily.hottest.month.pi.1986.2005.rds")

pr.monthly <- aggregateGrid(pr.daily, aggr.m = list(FUN = "sum", na.rm = FALSE))

monthly_quantile <- list()
for (i in 1:12){
  month.subset <- subsetGrid(pr.monthly, season=i)
  pr.month.q <- climatology(month.subset, clim.fun = list(FUN = "quantile", probs=0.5, na.rm = TRUE))
  monthly_quantile[[i]] <- pr.month.q
}
pr.q50 <- do.call(bindGrid, c(monthly_quantile, list(dimension = "time")))
saveRDS(pr.q50, "pr.q50.monthly.pi.1986.2005.rds", compress="xz")

# q50 <- readRDS("pr.q50.no.negative.rds")
# q50 <- subsetGrid(q50, latLim=c(35.9, 44), lonLim=c(-9.6, 3.6))


# mask <- loadGridData(mask.path, var="lsm", latLim=c(35.9, 44), lonLim=c(-9.6, 3.6))
# # Values under 0.5 are considered as sea
# mask$Data[mask$Data < 0.5] <- 0
# mask$Data[mask$Data >= 0.5] <- 1
# # Values of sea are set to NA
# mask$Data[mask$Data == 0] <- NA
# dataset <- q50
# mask$Data <- array(rep(mask$Data, each = dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "time")]),
#                    dim = c(dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "time")], 
#                            dim(mask$Data)[which(attr(mask$Data, "dimensions") == "lat")],
#                            dim(mask$Data)[which(attr(mask$Data, "dimensions") == "lon")]))
# attr(mask$Data, "dimensions") <- c("time", "lat", "lon")

# q500 <- gridArithmetics(q50, mask, operator="*")


# png("p50.multigrid.png", width=800, height=800)
# spatialPlot(climatology(q500), backdrop.theme="coastline", color.theme="YlGnBu")
# dev.off()

# png("p50.multigrid1.png", width=800, height=800)
# spatialPlot(climatology(pr.q50), backdrop.theme="coastline", color.theme="YlGnBu")
# dev.off()


tmax.daily <- readRDS("tmax.daily.hottest.month.pi.1986.2005.rds")

monthly_means <- list()
for (i in 1:12){
  month.subset <- subsetGrid(tmax.daily, season=i)
  tmax.month.q <- climatology(month.subset, clim.fun=list(FUN="quantile", probs=0.9, na.rm=TRUE))
  monthly_means[[i]] <- tmax.month.q
}
tmax.q90 <- do.call(bindGrid, c(monthly_means, list(dimension = "time")))
saveRDS(tmax.q90, "tmax.q90.monthly.pi.1986.2005.rds", compress="xz")