calculate.construction.profit <- function(typeID, ...) {
    construct.dt <- calculate.construction.cost(typeID, ...);
    
    profit.dt <- merge(construct.dt[, list(typeID, buildCost, buildWaste, maxWaste, wasteRatio)],
                       price.dt,
                       by = c('typeID'));
    
    profit.dt <- within(profit.dt, { margin = price / buildCost; });
    
    return(profit.dt[order(-margin)]);
}
