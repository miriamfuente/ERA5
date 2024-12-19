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

pr.q50.2 <- climatology(pr.monthly, clim.fun = list(FUN = "quantile", probs=0.5, na.rm = TRUE))

# Ambas formas pr.q50 y pr.q50.2 son equivalentes. La primera obtienes un array de 12 meses, pero
# datos en el hottest.month. La segunda tiene un unico valor, que correponde con el hottest.month

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


tmax.daily <- readRDS("quantiles/data.basic.pi/tmax.daily.hottest.month.pi.1986.2005.rds")

monthly_means <- list()
for (i in 1:12){
  month.subset <- subsetGrid(tmax.daily, season=i)
  tmax.month.q <- climatology(month.subset, clim.fun=list(FUN="quantile", probs=0.9, na.rm=TRUE))
  monthly_means[[i]] <- tmax.month.q
}
tmax.q90 <- do.call(bindGrid, c(monthly_means, list(dimension = "time")))
saveRDS(tmax.q90, "tmax.q90.monthly.pi.1986.2005.rds", compress="xz")

png("tmax.q90.multigrid.png", width=800, height=800)
spatialPlot(climatology(tmax.q90), backdrop.theme="coastline", color.theme="YlOrRd")
dev.off()


# Theresholds of quantiles for the calculous of the severity
tmax.max <- aggregateGrid(tmax.daily, aggr.m = list(FUN = "max", na.rm = TRUE))
tmax.max.months <- list()
list.lon <- list()
list.lat <- list() 
monthly.means <- list()
list.final <- list()

for (i in 1:dim(tmax.max$Data)[which(attr(tmax.max$Data, "dimensions") == "lat")]) {
  tmax.max.lat <- subsetDimension(tmax.max, dimension="lat", indices=i)
  for(j in 1:dim(tmax.max$Data)[which(attr(tmax.max$Data, "dimensions") == "lon")]) {
    tmax.max.lon <- subsetDimension(tmax.max.lat, dimension="lon", indices=j)
    month <- hottest.month$Data[,i, j]
    if(is.na(month)) {
      for (l in 1:12){
              month.subset <- subsetGrid(tmax.max.lon, season=l)
              tmax.max.month <- climatology(month.subset)
              monthly.means[[l]] <- tmax.max.month
          }
      tmax.max.months <- do.call(bindGrid, c(monthly.means, list(dimension = "time", skip.temporal.check=TRUE)))
    } else {
      tmax.max.lon.hot <- subsetGrid(tmax.max.lon, season=month)
      tmax.max.quantile <- climatology(tmax.max.lon.hot, clim.fun=list(FUN="quantile", probs=0.95, na.rm=TRUE))
      for (l in 1:12){
        month.subset <- subsetGrid(tmax.max.lon, season=l)
        tmax.max.month <- climatology(month.subset)
        monthly.means[[l]] <- tmax.max.month
      }
      tmax.max.months <- do.call(bindGrid, c(monthly.means, list(dimension = "time", skip.temporal.check=TRUE)))
      tmax.max.months$Data[month] <- tmax.max.quantile$Data 
    }
    list.lon[[j]] <- tmax.max.months
  }
    list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)

}
list.final <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)

list.final$Data[is.infinite(list.final$Data)] <- NA

saveRDS(list.final, "quantiles/data.quantiles.tmax/tmax.max.quantiles/tmax.max.q95.monthly.pi.1986.2005.rds", compress = "xz")