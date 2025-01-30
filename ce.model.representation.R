library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # needed for the biasCorrection (function biasCorrection)
library(climate4R.indices) # needed for calculate indices


# Path
path <- "/oceano/gmeteo/users/fuentesm/ERA5/modelos.cordex.core/"

# Model data used
gcm <- list("NCC", "MPI", "HadGEM")
rcm <- list("REMO", "RegCM")
wl <- list("+1.5_2008_2027", "+2_2028_2047", "+3_2052_2071", "+4_2072_2091")


categories <- readRDS(paste0(path,  rcm, "/", gcm, "/ce.categories.pi.", gcm, ".", rcm, ".rcp85.",wl[1], ".rds"))


# Path
path <- "/oceano/gmeteo/users/fuentesm/ERA5/modelos.cordex.core/"

# Model data used
gcm <- c("NCC", "MPI", "HadGEM")
rcm <- list("REMO", "RegCM")
wl <- c("+1.5_2008_2027", "+2_2028_2047", "+3_2052_2071", "+4_2072_2091")

# Bucle para recorrer todas las combinaciones y crear variables dinámicas
for (r in rcm) {
  for (g in gcm) {
    for (w in seq_along(wl)) {  # Usamos el índice para numerar
      # Construcción de la ruta del archivo
      file_path <- paste0(path, r, "/", g, "/ce.categories.pi.", g, ".", r, ".rcp85.", wl[w], ".rds")
      
      # Construcción del nombre de la variable
      var_name <- paste0("c.", tolower(r), ".", tolower(g), ".", w)

      # Comprobar si el archivo existe antes de intentar leerlo
      if (file.exists(file_path)) {
        assign(var_name, readRDS(file_path))
        print(paste("Archivo leído y guardado en:", var_name))
      } else {
        print(paste("Archivo no encontrado:", file_path))
      }
    }
  }
}

# Aplicar climatology() a cada variable generada
for (r in rcm) {
  for (g in gcm) {
    for (w in seq_along(wl)) {
      var_name <- paste0("c.", tolower(r), ".", tolower(g), ".", w)
      
      # Comprobar si la variable existe en el entorno global
      if (exists(var_name, envir = .GlobalEnv)) {
        data <- get(var_name, envir = .GlobalEnv)  # Obtener los datos
        data$c5 <- climatology(data$c5)  # Aplicar climatology() a la columna c5
        assign(var_name, data, envir = .GlobalEnv)  # Guardar el resultado en la misma variable
        print(paste("climatology() aplicado a:", var_name))
      } else {
        print(paste("Variable no encontrada:", var_name))
      }
    }
  }
}

# Generar PDF
pdf(paste0(path, "ce.categorie5.freq.pdf"), width = 20, height = 12)

spatialPlot(makeMultiGrid(
  climatology(c.remo.ncc.1$c5), climatology(c.regcm.ncc.1$c5), climatology(c.remo.mpi.1$c5), climatology(c.regcm.mpi.1$c5), climatology(c.remo.hadgem.1$c5), climatology(c.regcm.hadgem.1$c5),
  climatology(c.remo.ncc.2$c5), climatology(c.regcm.ncc.2$c5), climatology(c.remo.mpi.2$c5), climatology(c.regcm.mpi.2$c5), climatology(c.remo.hadgem.2$c5), climatology(c.regcm.hadgem.2$c5),
  climatology(c.remo.ncc.3$c5), climatology(c.regcm.ncc.3$c5), climatology(c.remo.mpi.3$c5), climatology(c.regcm.mpi.3$c5), climatology(c.remo.hadgem.3$c5), climatology(c.regcm.hadgem.3$c5),
  climatology(c.remo.ncc.4$c5), climatology(c.regcm.ncc.4$c5), climatology(c.remo.mpi.4$c5), climatology(c.regcm.mpi.4$c5), climatology(c.remo.hadgem.4$c5), climatology(c.regcm.hadgem.4$c5), 
  skip.temporal.check = TRUE),
  backdrop.theme = "coastline", color.theme = "YlOrRd", rev.colors = FALSE, 
  as.table = TRUE, layout = c(6,4), 
  names.attr = c("NCC-REMO +1.5ºC", "NCC-RegCM +1.5ºC", "MPI-REMO +1.5ºC", "MPI-RegCM +1.5ºC", "HadGEM-REMO +1.5ºC", "HadGEM-RegCM +1.5ºC",
                 "NCC-REMO +2ºC", "NCC-RegCM +2ºC", "MPI-REMO +2ºC", "MPI-RegCM +2ºC", "HadGEM-REMO +2ºC", "HadGEM-RegCM +2ºC",
                 "NCC-REMO +3ºC", "NCC-RegCM +3ºC", "MPI-REMO +3ºC", "MPI-RegCM +3ºC", "HadGEM-REMO +3ºC", "HadGEM-RegCM +3ºC",
                 "NCC-REMO +4ºC", "NCC-RegCM +4ºC", "MPI-REMO +4ºC", "MPI-RegCM +4ºC", "HadGEM-REMO +4ºC", "HadGEM-RegCM +4ºC"),
  colorkey = list(space = "bottom", title = list("Nº Events", cex = 1.5)),
  xlab="Models", ylab="Warming levels"
)

dev.off()
