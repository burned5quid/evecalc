calculate.advanced.construction.cost <- function(typeID, ME = c(0, 0), price.dt, bpcost.dt = NULL, verbose = FALSE, dbconnect = data.connection) {

    calc.cost <- function(typeID) {
        mat.1.dt <- calculate.construction.cost(typeID,          ME = ME[1], price.dt = price.dt, bpcost.dt = bpcost.dt, verbose = T, dbconnect = dbconnect);
        mat.2.dt <- calculate.construction.cost(mat.1.dt$typeID, ME = ME[2], price.dt = price.dt, bpcost.dt = bpcost.dt, verbose = T, dbconnect = dbconnect);

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

            if('bpcost' %in% names(cost.dt)) { cost.dt <- within(cost.dt, { cost = cost + bpcost }) }

            cost.dt <- cost.dt[, list(cost = sum(cost))];
        }

        return(cost.dt);
    }

    constructID <- typeID;

    advcost.dt <- item.dt[typeID %in% constructID][, list(typeID, typeName, groupName, categoryName)];
    advcost.dt <- advcost.dt[, calc.cost(typeID), by = list(typeID, typeName, groupName, categoryName)];

    return(advcost.dt);
}
