get.blueprint.id <- function(inputID) {
    typeNames <- paste(item.dt[typeID %in% inputID]$typeName, 'Blueprint');
    
    return(item.dt[typeName %in% typeNames]$typeID);
}
