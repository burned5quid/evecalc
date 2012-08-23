show.last.trades <- function(typeID, count = 10, side = 'buy', trade.dt = trade.dt) {
    showID  <- typeID;
    
    show.dt <- trade.dt[typeID %in% showID][transactionType == side][, list(transactionID, transactTime, transactionType, typeID, typeName, quantity, price)];
    
    print(tail(show.dt, n = count));
    
    return();
}
