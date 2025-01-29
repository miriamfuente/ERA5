transform.data <- function(dataset, hottest.month.pi){
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
return(list.total)
}