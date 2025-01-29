fun.binarization <- function(dataset, hottest.month, quantile, var){
    if (var=="pr"){
            
        # Define the months
        meses <- 1:12
        # Create lists to store the datasets
        list.lon <- list()
        list.lat <- list()
        list.total <- list()
        start_date <- NULL
        end_date <- NULL

        # Loop over the latitude and longitude to select the hottest month. Then compare the monthly precipitation with the 50th percentile 
        # of the monthly precipitation for the hottest month. If the monthly precipitation is greater than the 50th percentile, assign 0,
        # otherwise assign 1. Finally, combine the results for the region of interest and keep the same structure of the original dataset
        for (i in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lat")]) {
            pr.monthly.lat <- subsetDimension(dataset, dimension="lat", indices=i)
            pr.q50.lat <- subsetDimension(quantile, dimension="lat", indices=i)
            
            for (j in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lon")]) {
                pr.monthly.lon <- subsetDimension(pr.monthly.lat, dimension="lon", indices=j)
                pr.q50.lon <- subsetDimension(pr.q50.lat, dimension="lon", indices=j)
                month <- hottest.month$Data[i, j]
                if (is.na(month)) {
                    pr.final.lon <- redim(pr.monthly.lon, drop = TRUE)
                } else {
                    pr.monthly.lon.no.hot <- subsetGrid(pr.monthly.lon, season=c(setdiff(meses, month)))
                    pr.monthly.lon.hot <- subsetGrid(pr.monthly.lon, season=month)
                    pr.q50.lon.hot <- subsetGrid(pr.q50.lon, season=month)
                    for (k in 1:dim(pr.monthly.lon.hot$Data)[which(attr(pr.monthly.lon.hot$Data, "dimensions") == "time")]) {
                        if (pr.monthly.lon.hot$Data[k] > pr.q50.lon.hot$Data) {
                            if (pr.monthly.lon.hot$Data[k] <= 30){
                                pr.monthly.lon.hot$Data[k] <- 1
                            } else {
                                pr.monthly.lon.hot$Data[k] <- 0
                            }
                        } else {
                            pr.monthly.lon.hot$Data[k] <- 1
                        }
                    }
                    pr.final.lon <- bindGrid(pr.monthly.lon.no.hot, pr.monthly.lon.hot, dimension = "time")
                }
                
                # Needed to assign the start and end dates to the final dataset
                if (is.null(start_date) && is.null(end_date)) {
                    start_date <- pr.monthly.lon$Dates$start
                    end_date <- pr.monthly.lon$Dates$end
                }
                
                # Assign the start and end dates to the final dataset
                pr.final.lon$Dates$start <- start_date
                pr.final.lon$Dates$end <- end_date
                
                # Store the results
                list.lon[[j]] <- pr.final.lon
            }
            
            # Combine the results for longitudes
            list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
        }

        # Combine the results for latitudes
        list.total <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)
    }else if (var=="tmax") {
        # Define the months
        meses <- 1:12
        # Create lists to store the datasets
        list.lon <- list()
        list.lat <- list()
        list.total <- list()

        # Loop over the latitude and longitude to select the hottest month. Then compare the daily maximum temperature with the 90th percentile 
        # of the daily tmax for the hottest month. If the daily tmax is greater than the 50th percentile, assign 0,
        # otherwise assign 1. Finally, combine the results for the region of interest and keep the same structure of the original dataset
        for (i in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lat")]) {
            tmax.daily.lat <- subsetDimension(dataset, dimension="lat", indices=i)
            tmax.q90.lat <- subsetDimension(quantile, dimension="lat", indices=i)
            
            for (j in 1:dim(dataset$Data)[which(attr(dataset$Data, "dimensions") == "lon")]) {
                tmax.daily.lon <- subsetDimension(tmax.daily.lat, dimension="lon", indices=j)
                tmax.q90.lon <- subsetDimension(tmax.q90.lat, dimension="lon", indices=j)
                month <- hottest.month$Data[i, j]
                # print(paste("month= ", month))
                if (is.na(month)) {
                    tmax.final.lon <- redim(tmax.daily.lon, drop = TRUE)
                } else {
                    tmax.daily.lon.no.hot <- subsetGrid(tmax.daily.lon, season=c(setdiff(meses, month)))
                    tmax.daily.lon.hot <- subsetGrid(tmax.daily.lon, season=month)
                    tmax.q90.lon.hot <- subsetGrid(tmax.q90.lon, season=month)

                    for (k in 1:dim(tmax.daily.lon.hot$Data)[which(attr(tmax.daily.lon.hot$Data, "dimensions") == "time")]) {
                        if (tmax.daily.lon.hot$Data[k] < tmax.q90.lon.hot$Data) {
                            tmax.daily.lon.hot$Data[k] <- 0
                        } else {
                            tmax.daily.lon.hot$Data[k] <- 1
                        }
                    }
                    tmax.final.lon <- bindGrid(tmax.daily.lon.no.hot, tmax.daily.lon.hot, dimension = "time")
                    }
                
                # Store the results
                list.lon[[j]] <- tmax.final.lon
            }
            
            # Combine the results for longitudes
            list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
        }

        # Combine the results for latitudes
        list.total <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

    }else{
        stop("The variable must be either 'pr' or 'tmax'")
    }

    return(list.total)
}
