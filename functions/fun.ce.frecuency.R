fun.ce.frecuency <- function(hottest.month, tmax.bin, pr.bin) {
    meses <- 1:12
    # years <- 1986:2005
    list.years <- list()
    list.lon <- list()
    list.lat <- list()
    list.total <- list()

    for (i in 1:dim(tmax.bin$Data)[which(attr(tmax.bin$Data, "dimensions") == "lat")]) {
        print(paste("i= ", i))
        tmax.lat <- subsetDimension(tmax.bin, dimension="lat", indices=i)
        pr.lat <- subsetDimension(pr.bin, dimension="lat", indices=i)
        for(j in 1:dim(tmax.bin$Data)[which(attr(tmax.bin$Data, "dimensions") == "lon")]) {
            # print(paste("j= ", j))
            tmax.lon <- subsetDimension(tmax.lat, dimension="lon", indices=j)
            pr.lon <- subsetDimension(pr.lat, dimension="lon", indices=j)
            month <- hottest.month$Data[i, j]
            if (is.na(month)) {
                tmax.final.lon <- redim(tmax.lon, drop = TRUE)
                pr.final.lon <- redim(pr.lon, drop = TRUE)
            } else {
                pr.lon.hot <- subsetGrid(pr.lon, season=month)
                tmax.lon.hot <- subsetGrid(tmax.lon, season=month)
                tmax.lon.no.hot <- subsetGrid(tmax.lon, season=c(setdiff(meses, month)))    
                for (k in 1:dim(pr.lon.hot$Data)[which(attr(pr.lon.hot$Data, "dimensions") == "time")]) {
                    # print(paste("k= ", k))
                    if (pr.lon.hot$Data[k] == 1) {
                        date <- as.POSIXct(pr.lon.hot$Dates$start[k])
                        year <- as.numeric(format(date, "%Y"))
                        tmax.lon.hot.y <- subsetGrid(tmax.lon.hot, years=year)
                        attr(tmax.lon.hot.y$Data, "dimensions") <- c("time")
                        list.years[[k]] <- tmax.lon.hot.y
                    } else{
                        date <- as.POSIXct(pr.lon.hot$Dates$start[k])
                        year <- as.numeric(format(date, "%Y"))
                        tmax.lon.hot.y <- subsetGrid(tmax.lon.hot, years=year)
                        tmax.lon.hot.y$Data[] <- 0
                        attr(tmax.lon.hot.y$Data, "dimensions") <- c("time")
                        list.years[[k]] <- tmax.lon.hot.y
                    }
                    
                }
                tmax.lon.hot <- do.call(bindGrid, c(list.years, list(dimension = "time", skip.temporal.check = TRUE)))
                tmax.final.lon <- bindGrid(tmax.lon.no.hot, tmax.lon.hot, dimension = "time")

            }
            list.lon[[j]] <- tmax.final.lon
        }
        list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)

    }
    list.final <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)

}
