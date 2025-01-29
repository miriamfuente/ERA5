# This function is used to define the severity of the compound events
fun.categories <- function(cat, hottest.month){

    months.no.hottest <- list()
    list.c1.lon <- list()
    list.c2.lon <- list()
    list.c3.lon <- list()
    list.c4.lon <- list()
    list.c5.lon <- list()

    list.c1.lat <- list()
    list.c2.lat <- list()
    list.c3.lat <- list()
    list.c4.lat <- list()
    list.c5.lat <- list()

    list.c1 <- list()
    list.c2 <- list()
    list.c3 <- list()
    list.c4 <- list()
    list.c5 <- list()

    meses <- 1:12

    
    for (i in 1:dim(cat$Data)[which(attr(cat$Data, "dimensions") == "lat")]) {
        cat.lat <- subsetDimension(cat, dimension="lat", indices=i)
        for (j in 1:dim(cat$Data)[which(attr(cat$Data, "dimensions") == "lon")]) {
            cat.lon <- subsetDimension(cat.lat, dimension="lon", indices=j)
            month <- hottest.month$Data[i, j]
            if (is.na(month)) {
                for (l in 1:12){
                    month.subset <- subsetGrid(cat.lon, season=l)
                    ce.month <- climatology(month.subset)
                    months.no.hottest[[l]] <- ce.month
                }
                c1.final <- do.call(bindGrid, c(months.no.hottest, list(dimension = "time", skip.temporal.check=TRUE)))
                c2.final <- c1.final
                c3.final <- c1.final
                c4.final <- c1.final
                c5.final <- c1.final
            } else {
                for (l in 1:12){
                            month.subset <- subsetGrid(cat.lon, season=l)
                            ce.month <- climatology(month.subset)
                            months.no.hottest[[l]] <- ce.month
                }
                cat.no.hottest <- do.call(bindGrid, c(months.no.hottest, list(dimension = "time", skip.temporal.check=TRUE)))
                cat.no.hottest <- subsetGrid(cat.no.hottest, season=c(setdiff(meses, month)))
                cat.hottest <- subsetGrid(cat.lon, season=month)
                c1 <- climatology(cat.hottest)
                c2 <- climatology(cat.hottest)
                c3 <- climatology(cat.hottest)
                c4 <- climatology(cat.hottest)
                c5 <- climatology(cat.hottest)
                freq <- table(factor(cat.hottest$Data, levels=0:5))
                c1$Data<- freq["1"]
                attr(c1$Data, "dimensions") <- c("time")
                c2$Data <- freq["2"]
                attr(c2$Data, "dimensions") <- c("time")
                c3$Data <- freq["3"]
                attr(c3$Data, "dimensions") <- c("time")
                c4$Data <- freq["4"]
                attr(c4$Data, "dimensions") <- c("time")
                c5$Data <- freq["5"]
                attr(c5$Data, "dimensions") <- c("time")
                c1.final <- bindGrid(cat.no.hottest, c1, dimension = "time")
                c2.final <- bindGrid(cat.no.hottest, c2, dimension = "time")
                c3.final <- bindGrid(cat.no.hottest, c3, dimension = "time")
                c4.final <- bindGrid(cat.no.hottest, c4, dimension = "time")
                c5.final <- bindGrid(cat.no.hottest, c5, dimension = "time")
            }
            list.c1.lon[[j]] <- c1.final
            list.c2.lon[[j]] <- c2.final
            list.c3.lon[[j]] <- c3.final
            list.c4.lon[[j]] <- c4.final
            list.c5.lon[[j]] <- c5.final
        }
        list.c1.lat[[i]] <- redim(do.call(bindGrid, c(list.c1.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c2.lat[[i]] <- redim(do.call(bindGrid, c(list.c2.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c3.lat[[i]] <- redim(do.call(bindGrid, c(list.c3.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c4.lat[[i]] <- redim(do.call(bindGrid, c(list.c4.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
        list.c5.lat[[i]] <- redim(do.call(bindGrid, c(list.c5.lon, list(dimension = "lon", skip.temporal.check = FALSE))), drop = TRUE)
    }
    list.c1 <- redim(do.call(bindGrid, c(list.c1.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c2 <- redim(do.call(bindGrid, c(list.c2.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c3 <- redim(do.call(bindGrid, c(list.c3.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c4 <- redim(do.call(bindGrid, c(list.c4.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)
    list.c5 <- redim(do.call(bindGrid, c(list.c5.lat, list(dimension = "lat", skip.temporal.check = FALSE))), drop = TRUE)

    return(list(c1 = list.c1, c2 = list.c2, c3 = list.c3, c4 = list.c4, c5 = list.c5))

}