fun.quantiles <- function(dataset, var, quantile){
    if(var=="pr"){
        dataset <- aggregateGrid(dataset, aggr.m = list(FUN = "sum", na.rm = FALSE))
    }else if (var=="tmax"){
        dataset <- dataset
    }else{
        stop("The variable must be pr or tmax")
    }
    monthly_quantile <- list()
    for (i in 1:12){
    month.subset <- subsetGrid(dataset, season=i)
    dataset.mm.q <- climatology(month.subset, clim.fun = list(FUN = "quantile", probs=quantile, na.rm = TRUE))
    monthly_quantile[[i]] <- dataset.mm.q
    }
    quantile.final <- do.call(bindGrid, c(monthly_quantile, list(dimension = "time")))
    return(quantile.final)

}
