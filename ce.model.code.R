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

path <- "/oceano/gmeteo/users/fuentesm/ERA5/modelos.cordex.core/RegCM/"
mask <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/MPI-REMO/mask.pi.mpi.remo.rds")
# Load the datasets
pr <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/MPI-REMO/rcp85/pr.dd.pi.MPI.REMO.rcp85.+4_2072_2091.rds")
tmax <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/MPI-REMO/rcp85/tmax.dd.pi.MPI.REMO.rcp85.+4_2072_2091.rds")

hottest.month <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/obs.era5.land/hottest.month.pi.1986.2005.era5.land.rds")
