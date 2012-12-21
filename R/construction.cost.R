#' Calculate manufacturing cost
#'
#'
#' @usage calculate.advanced.construction.cost(typeID, ME = c(0, 0), price.dt, bpcost.dt = NULL, verbose = FALSE, dbconnect) {
#' @keywords manufacture
#' @param typeID typeID for the item to be manufactured
#' @param ME vector of length (1 or 2) for the ME of the main blueprint (and the components)
#' @param price.dt data.table for the price data
#' @param bpcost.dt data.table for the cost of BPCs
#' @param verbose logical indicating whether verbose output is wanted
#' @param dbconnect db connection to the sqlite database of static data
#' @name construction.cost
#' @aliases calculate.construction.cost calculate.advanced.construction.cost calculate.construction.profit
#' @export calculate.construction.cost calculate.advanced.construction.cost calculate.construction.profit
#' @import data.table
#' @examples
#' \dontrun{
#' ### Calculate the cost of building a Manticore (id 12031)
#' ### with ME -4 and component ME 30
#' manticoreID <- 12031;
#' calculate.advanced.construction.cost(manticoreID, ME = c(-4, 30), price.dt = pricedata.dt)
#' }
#'
NULL

calculate.construction.cost <- function(typeID, ME = 0, price.dt = pricedata.dt, verbose = FALSE, dbconnect = data.connection) {
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
    
    if(!verbose) {
        cost.dt <- cost.dt[, list(materialCost = sum(requiredCost),
                                  buildCost    = sum(requiredCost),
                                  buildWaste   = sum(wasteCost),
                                  maxWaste     = max(wasteCost),
                                  wasteRatio   = sum(wasteCost) / sum(requiredCost)),
                           by = list(constructTypeID, constructTypeName)];
    }
    
    
    return(cost.dt);
}


calculate.advanced.construction.cost <- function(typeID, ME = c(0, 0), price.dt, verbose = FALSE, dbconnect = data.connection) {

    calc.cost <- function(typeID) {
        mat.1.dt <- calculate.construction.cost(typeID,          ME = ME[1], price.dt = price.dt, verbose = TRUE, dbconnect = dbconnect);
        mat.2.dt <- calculate.construction.cost(mat.1.dt$typeID, ME = ME[2], price.dt = price.dt, verbose = TRUE, dbconnect = dbconnect);

        mat.1.dt <- within(mat.1.dt, { mergeID = typeID });
        mat.2.dt <- within(mat.2.dt, { mergeID = constructTypeID });

        basic.dt <- mat.1.dt[!typeID %in% mat.2.dt$constructTypeID];
        comp.dt  <- merge(mat.1.dt[, list(mergeID, q1 = required)],
                          mat.2.dt[, list(mergeID, typeID, typeName, q2 = required, price)], by = 'mergeID');

        comp.dt <- within(comp.dt, { quantity = q1 * q2 });

        cost.dt <- rbind(basic.dt[, list(typeID, typeName, quantity = required, price)],
                         comp.dt [, list(typeID, typeName, quantity, price)]);

        if(!verbose) {
            cost.dt <- cost.dt[, list(required = sum(quantity), price = max(price)), by = list(typeID, typeName)];

            cost.dt <- within(cost.dt, { cost = required * price });

            cost.dt <- cost.dt[, list(cost = sum(cost))];
        }

        return(cost.dt);
    }

    constructID <- typeID;

    advcost.dt <- item.dt[typeID %in% constructID][, list(typeID, typeName, groupName, categoryName)];

    advcost.dt <- advcost.dt[, calc.cost(typeID), by = list(typeID, typeName, groupName, categoryName)];

    return(advcost.dt);
}


calculate.construction.profit <- function(typeID, ...) {
    construct.dt <- calculate.construction.cost(typeID, ...);
    
    profit.dt <- merge(construct.dt[, list(typeID, buildCost, buildWaste, maxWaste, wasteRatio)],
                       price.dt,
                       by = c('typeID'));
    
    profit.dt <- within(profit.dt, { margin = price / buildCost; });
    
    return(profit.dt[order(-margin)]);
}
