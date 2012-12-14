#' Recycling calculation functions
#'
#' 
#'
#' @usage calculate.implied.mineral.prices(mineral.weights, oreids, price.dt)
#' @keywords recycle
#' @param relative.price price of mineral relative to price of Tritanium
#' @param oreids vector of typeIDs for each ore to be used
#' @param price.dt data.table of prices
#' @name static.data
#' @aliases calculate.implied.mineral.prices construct.base.price.matrix construct.recycle.data
#' @export calculate.implied.mineral.prices construct.base.price.matrix construct.recycle.data
#' @import data.table
#' @import reshape2
#' @examples
#' \dontrun{
#'
#' }
NULL

calculate.implied.mineral.prices <- function(relative.price, oreids = idlist.ore, price.dt = pricedata.dt) {
    recycle.dt <- construct.recycle.data(oreids, price.dt);
    
    if(missing(relative.price)) {
        use.weights = mineral.price.weights;
    }
    
    base.matrix <- construct.base.price.matrix(relative.price);
    
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


construct.base.price.matrix <- function(relative.price = mineral.relative.price) {
    base.matrix      <- diag(rep(1, 7));
    base.matrix[, 1] <- -relative.price;
    
    return(base.matrix[2:7, ]);
}


construct.recycle.data <- function(oreids = oreids, price.dt = pricedata.dt) {
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
