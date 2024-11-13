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
latitud <- c(35.9, 44)
longitud <- c(-9.6, 3.6)
years = 1986:2005
dataset <- loadGridData(df, var="tp", latLim=latitud, lonLim=longitud, years=years)
dataset <- gridArithmetics(dataset, 1000, operator="*")
dataset$Data[dataset$Data < 0] <- 0

# mask <- loadGridData(mask.path, var="lsm", latLim=c(42.3, 43.9), lonLim=c(-3.4,0.4))
# # Values under 0.5 are considered as sea
# mask$Data[mask$Data < 0.5] <- 0
# mask$Data[mask$Data >= 0.5] <- 1
# # Values of sea are set to NA
# mask$Data[mask$Data == 0] <- NA

# mask$Data <- array(rep(mask$Data, each = dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "time")]),
#                    dim = c(dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "time")], 
#                            dim(mask$Data)[which(attr(mask$Data, "dimensions") == "lat")],
#                            dim(mask$Data)[which(attr(mask$Data, "dimensions") == "lon")]))
# attr(mask$Data, "dimensions") <- c("time", "lat", "lon")

# land <- gridArithmetics(dataset, mask, operator="*")

hottest.month.pi <- subsetGrid(hottest.month, latLim=latitud, lonLim=longitud)
list.lon <- list()
list.lat <- list()
list.total <- list()
meses <- 1:12

for (i in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lat")]) {
    dataset.lat <- subsetDimension(dataset, dimension="lat", indices=i)
  for (j in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lon")]) {
    dataset.lon <- subsetDimension(dataset.lat, dimension="lon", indices=j)
    month <- hottest.month.pi$Data[i, j]
    if (is.na(month)) {
      # Si month es NA, asignar NA a todos los valores de dataset.lon$Data y continuar
      dataset.lon$Data[!is.na(dataset.lon$Data)] <- NA  # Asigna NA a todos los elementos
      list.lon[[j]] <- dataset.lon
    } else {
      # Procesamiento normal si month tiene un valor válido
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

a <- climatology(list.total)

png("pruebapirineosymas.tmax.png", width = 800, height = 600)
spatialPlot(climatology(tmax), backdrop.theme = "coastline", color.theme="YlOrRd", rev.colors = FALSE, 
            main= "Daily maximum temperature")
dev.off()

saveRDS(list.total, "pr.daily.hottest.month.pi.rds", compress="xz")

