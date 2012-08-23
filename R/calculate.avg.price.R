calculate.avg.price <- function(trade.dt) {
    avgprice.dt <- trade.dt[, list(volume   = sum(quantity),
                                   cash     = sum(quantity * price),
                                   avgprice = (sum(quantity * price) / sum(quantity))),
                            by = list(typeID, typeName, transactionType)];
    
    setkey(avgprice.dt, typeID, transactionType);
    
    return(avgprice.dt);
}
