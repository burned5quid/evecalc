calculate.construction.cost <- function(typeID, ME = 0, price.dt = pricedata.dt, bpcost.dt = NULL, verbose = FALSE, dbconnect = data.connection) {
    material.dt <- within(get.blueprint.data(typeID, dbconnect), {
        if(ME >= 0) {
            waste = round((0.1/(1 + ME))  * quantity * wasteFactor, 0);
        } else {
            waste = round((0.1 * abs(ME)) * quantity * wasteFactor, 0);
        }
        
        required = quantity + waste;
        
        waste = required - quantity;
    });
    
    setkey(material.dt, typeID);
    setkey(price.dt,    typeID);
    
    material.dt <- merge(material.dt, price.dt[, list(typeID, price)], all.x = T, by = c('typeID'));
    
    cost.dt <- within(material.dt, {
        requiredCost = required * price;
        wasteCost    = waste    * price;
    });
    
    if(!is.null(bpcost.dt)) {
        cost.dt <- merge(cost.dt, bpcost.dt[, list(constructTypeID = typeID, bpcost)], all.x = T, by = 'constructTypeID');  
    } else {
        cost.dt <- within(cost.dt, { bpcost = 0 });
    }
    
    
    if(concise) {
        cost.dt <- cost.dt[, list(materialCost = sum(requiredCost),
                                  buildCost    = sum(requiredCost) + bpcost[1],
                                  buildWaste   = sum(wasteCost),
                                  maxWaste     = max(wasteCost),
                                  wasteRatio   = sum(wasteCost) / sum(requiredCost)),
                           by = list(constructTypeID, constructTypeName)];
    }
    
    
    return(cost.dt);
}

