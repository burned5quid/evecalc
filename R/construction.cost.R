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
#' @import plyr
#' @examples
#' \dontrun{
#' ### Calculate the cost of building a Manticore (id 12031)
#' ### with ME -4 and component ME 30
#' manticoreID <- 12031;
#' calculate.advanced.construction.cost(manticoreID, ME = c(-4, 30), price.dt = pricedata.dt)
#' }
#'
NULL

calculate.construction.cost <- function(typeID, ME = 0, price.dt = pricedata.dt, verbose = FALSE, bpcost.dt = NULL, dbconnect = data.connection) {

    material.dt <- calculate.waste(get.blueprint.data(typeID, dbconnect), ME);

    setkey(material.dt, typeID);
    setkey(price.dt,    typeID);

    material.dt <- merge(material.dt, price.dt[, list(matTypeID = typeID, price)], all.x = TRUE, by = c('matTypeID'));

    cost.dt <- material.dt;
    cost.dt[, requiredCost := required * price];
    cost.dt[, wasteCost    := waste    * price];


    ### If blueprint costs have been supplied, we add that in as an additional
    ### line item and include it in the cost calculation.
    if(!is.null(bpcost.dt)) {
        itemID <- typeID;

        bp.dt  <- bpcost.dt[typeID %in% itemID][, list(typeID,
                                                       typeName     = as.character(typeName),
                                                       matTypeID    = 0,
                                                       matTypeName  = 'BPC',
                                                       quantity     = 1,
                                                       wasteFactor  = 0,
                                                       waste        = 0,
                                                       required     = 1,
                                                       price        = bpcost,
                                                       wasteCost    = 0,
                                                       requiredCost = bpcost)];

        cost.dt <- data.table(rbind.fill(cost.dt, bp.dt));
    }


    if(!verbose) {
        cost.dt <- cost.dt[, list(materialCost = sum(requiredCost[matTypeName != 'BPC']),
                                  buildCost    = sum(requiredCost),
                                  buildWaste   = sum(wasteCost),
                                  maxWaste     = max(wasteCost),
                                  wasteRatio   = sum(wasteCost) / sum(requiredCost)),
                           by = list(typeID, typeName)];
    }


    return(cost.dt);
}


calculate.advanced.construction.cost <- function(typeID, ME = c(0, 0), price.dt, verbose = FALSE, dbconnect = data.connection) {

    calc.cost <- function(typeID) {
        mat.1.dt <- calculate.construction.cost(typeID,             ME = ME[1], price.dt = price.dt, verbose = TRUE, dbconnect = dbconnect);
        mat.2.dt <- calculate.construction.cost(mat.1.dt$matTypeID, ME = ME[2], price.dt = price.dt, verbose = TRUE, dbconnect = dbconnect);

        mat.1.dt[, mergeID := matTypeID];
        mat.2.dt[, mergeID := typeID];


        basic.dt <- mat.1.dt[!matTypeID %in% mat.2.dt$typeID];
        comp.dt  <- merge(mat.1.dt[, list(mergeID, q1 = required)],
                          mat.2.dt[, list(mergeID, typeID = matTypeID, typeName = matTypeName, q2 = required, price)], by = 'mergeID');

        comp.dt[, quantity := q1 * q2];

        cost.dt <- rbind(basic.dt[, list(typeID = matTypeID, typeName = matTypeName, quantity = required, price)],
                         comp.dt [, list(typeID, typeName, quantity, price)]);

        if(!verbose) {
            cost.dt <- cost.dt[, list(required = sum(quantity), price = max(price)), by = list(typeID, typeName)];

            cost.dt[, cost := required * price];

            cost.dt <- cost.dt[, list(cost = sum(cost))];
        }

        return(cost.dt);
    }

    constructID <- typeID;

    advcost.dt <- item.dt[typeID %in% constructID][, list(typeID, typeName, groupName, categoryName)];

    advcost.dt <- advcost.dt[, calc.cost(typeID), by = list(typeID, typeName, groupName, categoryName)];

    return(advcost.dt);
}


calculate.construction.profit <- function(typeID, price.dt, ...) {
    construct.dt <- calculate.construction.cost(typeID, price.dt = price.dt, ...);

    profit.dt <- merge(construct.dt[, list(typeID, typeName, buildCost, buildWaste, maxWaste, wasteRatio)],
                       price.dt,
                       by = c('typeID'));

    profit.dt[, margin := price / buildCost];

    return(profit.dt[order(-margin)]);
}


calculate.waste <- function(data.dt, ME) {
    stopifnot(is.data.table(data.dt));

    waste.dt <- data.dt;

    if(ME >= 0) {
        waste.dt[, waste := round((0.1 / (1 + ME)) * quantity * wasteFactor, 0)];
    } else {
        waste.dt[, waste := round((0.1 * abs(ME))  * quantity * wasteFactor, 0)];
    }

    waste.dt[, required := quantity + waste];
    waste.dt[, waste    := required - quantity];

    return(waste.dt);
}
