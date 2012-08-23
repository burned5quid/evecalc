calculate.implied.mineral.prices <- function(use.weights, oreids = idlist.ore, price.dt = recentavg.dt) {
    recycle.dt <- construct.recycle.data(oreids, price.dt);
    
    if(missing(use.weights)) {
        use.weights = mineral.price.weights;
    }
    
    base.matrix <- construct.base.price.matrix(use.weights);
    
    calculate.mineral.price <- function(tradedata.dt) {
        LHS <- rbind(as.matrix(tradedata.dt[, 1:7, with = F]), base.matrix);
        RHS <- c(with(tradedata.dt, portionSize * avgprice), rep(0, 6));
        
        implied.prices        <- solve(LHS, RHS);
        names(implied.prices) <- names(use.weights);
        
        ### Now we need to zero out the prices for minerals that are not obtained from the ore
        zeroed <- as.numeric(tradedata.dt[1, 1:7, with = F]);
        zeroed <- zeroed / zeroed;
        zeroed[is.nan(zeroed)] <- 0;
        
        prices <- t(implied.prices) * zeroed;
        
        return(data.table(prices));
    }
    
    implied.price.dt <- recycle.dt[, calculate.mineral.price(.SD), by = list(typeID, typeName)];
    
    return(implied.price.dt);
}
