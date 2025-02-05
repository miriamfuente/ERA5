fun.duration <- function(ce, hottest.month, streak, years){
    source("/oceano/gmeteo/users/fuentesm/ERA5/functions/binSpell.R")

    # Initialize variables and lists
    meses <- 1:12
    monthly.means <- list()

    # list.lon.max <- list()
    # list.lon.mean <- list()
    list.lon.median.max <- list()
    # list.lon.median.mean <- list()
    # list.lon.std.max <- list()
    # list.lon.std.mean <- list()

    # list.lat.max <- list()
    # list.lat.mean <- list()
    list.lat.median.max <- list()
    # list.lat.median.mean <- list()
    # list.lat.std.max <- list()
    # list.lat.std.mean <- list()

    # list.total.max <- list()
    # list.total.mean <- list()
    list.total.median.max <- list()
    # list.total.median.mean <- list()
    # list.total.std.max <- list()
    # list.total.std.mean <- list()

    list.max <- list()
    # list.mean <- list()

    for (i in 1:dim(ce$Data)[which(attr(ce$Data, "dimensions") == "lat")]) {
        print(paste("i= ", i))
        ce.lat <- subsetDimension(ce, dimension="lat", indices=i)
        for(j in 1:dim(ce$Data)[which(attr(ce$Data, "dimensions") == "lon")]) {
            ce.lon <- subsetDimension(ce.lat, dimension="lon", indices=j)
            month <- hottest.month$Data[i, j]
            if(is.na(month)) {
                for (l in 1:12){
                        month.subset <- subsetGrid(ce.lon, season=l)
                        ce.month <- climatology(month.subset)
                        monthly.means[[l]] <- ce.month
                    }
                ce.months <- do.call(bindGrid, c(monthly.means, list(dimension = "time", skip.temporal.check=TRUE)))
                # ce.mm.max <- ce.months
                # ce.mm.mean <- ce.months
                ce.mm.median.max <- ce.months
                # ce.mm.median.mean <- ce.months
                # ce.mm.std.max <- ce.months
                # ce.mm.std.mean <- ce.months
            } else {
                ce.lon.hot <- subsetGrid(ce.lon, season=month)
                ce.lon.no.hot <- subsetGrid(ce.lon, season=setdiff(1:12, month))
                # The function binSpell calculates the duration of the maximum streak of CE
                for (k in seq_along(years)){
                    year <- years[k]
                    ce.lon.hot.y <- subsetGrid(ce.lon.hot, years=year)
                    duration <- binSpell(ce.lon.hot.y$Data)
                    len <- duration$len
                    val <- duration$val
                    idx.valid <- len >= racha
                    len.valid <- len[idx.valid]
                    val.valid <- val[idx.valid]
                    idx.val <- which(val.valid == 1)
                    duration.events <- len.valid[idx.val]
                    obj.max <- climatology(ce.lon.hot.y)
                    # obj.mean <- climatology(ce.lon.hot.y)
                    obj.max$Data <- if (length(duration.events) > 0) max(duration.events) else 0
                    attr(obj.max$Data, "dimensions") <- "time"
                    # obj.mean$Data <- if (length(duration.events) > 0) mean(duration.events) else 0
                    # attr(obj.mean$Data, "dimensions") <- "time"
                    list.max[[k]] <- obj.max
                    # list.mean[[k]] <- obj.mean
                }
                ce.lon.hot.max <- do.call(bindGrid, c(list.max, list(dimension = "time", skip.temporal.check=TRUE)))
                # ce.lon.hot.mean <- do.call(bindGrid, c(list.mean, list(dimension = "time", skip.temporal.check=TRUE)))

                idx.max <- which(ce.lon.hot.max$Data > 0)
                # idx.mean <- which(ce.lon.hot.mean$Data > 0)

                # data.max <- mean(ce.lon.hot.max$Data[idx.max])
                # data.mean <- mean(ce.lon.hot.mean$Data[idx.mean])
                data.median.max <- median(ce.lon.hot.max$Data[idx.max])
                # data.median.mean <- median(ce.lon.hot.mean$Data[idx.mean])
                # data.std.max <- sd(ce.lon.hot.max$Data[idx.max])   
                # data.std.mean <- sd(ce.lon.hot.mean$Data[idx.mean])

                # ce.max <- climatology(ce.lon.hot.max)
                # ce.mean <- climatology(ce.lon.hot.mean)
                ce.median.max <- climatology(ce.lon.hot.max)
                # ce.median.mean <- climatology(ce.lon.hot.mean)
                # ce.std.max <- climatology(ce.lon.hot.max)
                # ce.std.mean <- climatology(ce.lon.hot.mean)

                # ce.max$Data <- data.max
                # attr(ce.max$Data, "dimensions") <- "time"
                # ce.mean$Data <- data.mean
                # attr(ce.mean$Data, "dimensions") <- "time"
                ce.median.max$Data <- data.median.max
                attr(ce.median.max$Data, "dimensions") <- "time"
                # ce.median.mean$Data <- data.median.mean
                # attr(ce.median.mean$Data, "dimensions") <- "time"
                # ce.std.max$Data <- data.std.max
                # attr(ce.std.max$Data, "dimensions") <- "time"
                # ce.std.mean$Data <- data.std.mean
                # attr(ce.std.mean$Data, "dimensions") <- "time"

                # Hasta aquí, se ha calculado la duración de los eventos de CE en el mes más cálido
                for (l in 1:12){
                        month.subset <- subsetGrid(ce.lon, season=l)
                        ce.month <- climatology(month.subset)
                        monthly.means[[l]] <- ce.month
                    }
                ce.months <- do.call(bindGrid, c(monthly.means, list(dimension = "time", skip.temporal.check=TRUE)))

                # ce.mm.max <- ce.months
                # ce.mm.mean <- ce.months
                ce.mm.median.max <- ce.months
                # ce.mm.median.mean <- ce.months
                # ce.mm.std.max <- ce.months
                # ce.mm.std.mean <- ce.months
                # ce.mm.max$Data[month] <- ce.max$Data
                # ce.mm.mean$Data[month] <- ce.mean$Data
                ce.mm.median.max$Data[month] <- ce.median.max$Data
                # ce.mm.median.mean$Data[month] <- ce.median.mean$Data
                # ce.mm.std.max$Data[month] <- ce.std.max$Data
                # ce.mm.std.mean$Data[month] <- ce.std.mean$Data

            }
            # list.lon.max[[j]] <- ce.mm.max
            # list.lon.mean[[j]] <- ce.mm.mean
            list.lon.median.max[[j]] <- ce.mm.median.max
            # list.lon.median.mean[[j]] <- ce.mm.median.mean
            # list.lon.std.max[[j]] <- ce.mm.std.max
            # list.lon.std.mean[[j]] <- ce.mm.std.mean
        
        }
        # list.lat.max[[i]] <- redim(do.call(bindGrid, c(list.lon.max, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        # list.lat.mean[[i]] <- redim(do.call(bindGrid, c(list.lon.mean, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.lat.median.max[[i]] <- redim(do.call(bindGrid, c(list.lon.median.max, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        # list.lat.median.mean[[i]] <- redim(do.call(bindGrid, c(list.lon.median.mean, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        # list.lat.std.max[[i]] <- redim(do.call(bindGrid, c(list.lon.std.max, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        # list.lat.std.mean[[i]] <- redim(do.call(bindGrid, c(list.lon.std.mean, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
    }
    # list.final.max <- redim(do.call(bindGrid, c(list.lat.max, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)
    # list.final.mean <- redim(do.call(bindGrid, c(list.lat.mean, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)
    list.final.median.max <- redim(do.call(bindGrid, c(list.lat.median.max, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)
    # list.final.median.mean <- redim(do.call(bindGrid, c(list.lat.median.mean, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)
    # list.final.std.max <- redim(do.call(bindGrid, c(list.lat.std.max, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)
    # list.final.std.mean <- redim(do.call(bindGrid, c(list.lat.std.mean, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)

    # return(list(max.mean = list.final.max, mean.mean = list.final.mean, max.median = list.final.median.max, mean.median = list.final.median.mean, max.std = list.final.std.max, mean.std = list.final.std.mean))
    return(list.final.median.max)
}