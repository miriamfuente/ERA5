fun.hottest.month <- function(tmax.total){
    # tmax.total: tmax.daily without transformation 
    monthly_means <- list()
    for (i in 1:12){
    month.subset <- subsetGrid(tmax.total, season=i)
    tmax.month.aggr <- climatology(month.subset)
    monthly_means[[i]] <- tmax.month.aggr
    }
    tmax.aggr.year <- do.call(bindGrid, c(monthly_means, list(dimension = "time")))
    
    # FOR THE DATSET ERA5-LAND
    idx.hottest.month.data <- apply(tmax.aggr.year$Data, c(2, 3), function(x) {
        if (all(is.na(x))) return(NA)  # Devuelve NA si todos los valores son NaN
        which.max(x)
    })

    # INDEX
    idx.hottest.month <- tmax.aggr.year
    idx.hottest.month$Data <- idx.hottest.month.data
    attr(idx.hottest.month$Data,"dimensions")<- c("lat", "lon")

    return(idx.hottest.month)
}