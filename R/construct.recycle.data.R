construct.recycle.data <- function(oreids = oreids, price.dt = recentavg.dt) {
    recycle.dt <- data.table(dcast(get.blueprint.data(oreids)[, list(constructTypeID, constructTypeName, typeName, quantity)],
                                   constructTypeID + constructTypeName ~ typeName,
                                   value_var = 'quantity',
                                   fill = 0),
                             key = 'constructTypeID');
    
    refining.dt <- merge(item.dt[typeID %in% oreids][, list(typeID, typeName, portionSize)],
                         price.dt[transactionType == 'buy'][, list(typeID, avgprice)],
                         by = 'typeID');
    
    names(refining.dt)[1] <- 'constructTypeID';
    
    recycle.dt <- recycle.dt[, c(1, 2, match(names(mineral.price.weights), names(recycle.dt))), with = F]
    recycle.dt <- merge(recycle.dt, refining.dt, by = 'constructTypeID');
    
    recycle.dt <- within(recycle.dt, {
        typeID            = constructTypeID;
        
        constructTypeID   = NULL;
        constructTypeName = NULL;
    });
    
    return(recycle.dt);
}
