adapt.IPCC.regions <- function(dataset, region){
    #Load the reference regions
    load("IPCC/IPCC-WGI-reference-regions-v4_R.rda", verbose = TRUE)
    refregions <- as(IPCC_WGI_reference_regions_v4, "SpatialPolygons")
    med <- refregions[region]
    dataset <- setGridProj(grid = dataset, proj = proj4string(refregions))
    data.med <- overGrid(dataset, med, subset=TRUE)
    
    return(data.med)

}