#' Retrieve static data
#'
#' This function reads in the entries from the CSV trade file downloads
#' and returns a data.table of all the unique entries
#'
#' @usage get.blueprint.data(typeID, dbconnect = static.dbconnection)
#' @usage get.blueprint.id(typeID)
#' @usage get.component.data(typeID, dbconnect = static.dbconnection)
#' @usage get.item.data(dbconnect = static.dbconnection)
#' @usage get.material.data(typeID, dbconnect = static.dbconnection)
#' @usage get.name.data(dbconnect = static.dbconnection)
#' @keywords staticdata
#' @param typeID typeID for the item to be queried
#' @param dbconnect connection to the sqlite static data dump
#' @name static.data
#' @aliases get.blueprint.data get.blueprint.id get.component.data get.item.data get.material.data get.name.data
#' @export get.blueprint.data get.blueprint.id get.component.data get.item.data get.material.data get.name.data
#' @import data.table
#' @import RSQLite
#' @examples
#' \dontrun{
#'
#' }
NULL

get.blueprint.data <- function(typeID, dbconnect = static.dbconnection) {
    get.data <- function(iterID) {
        ### First get the mineral count
        sql.query <- paste("SELECT c.typeID AS typeID, c.typeName typeName, ",
                           "       m.typeID AS matTypeID, m.typeName AS matTypeName, quantity, 1 AS wasteFactor ",
                           "FROM invTypes AS c, invTypes AS m, invTypeMaterials itm ",
                           "WHERE c.typeID = itm.typeID ",
                           "  AND m.typeID = itm.materialTypeID ",
                           "  AND c.typeID = ", iterID, sep = '');

        mineral.dt <- data.table(dbGetQuery(dbconnect, sql.query));

        ### Now we need the extra data
        blueprintID <- get.blueprint.id(iterID);

        if(length(blueprintID) > 0) {
            sql.query <- paste("SELECT t.typeID AS matTypeID, t.typeName AS matTypeName, r.quantity, 0 AS wasteFactor ",
                               "FROM ramTypeRequirements AS r, invTypes AS t ",
                               "WHERE r.requiredTypeID = t.typeID ",
                               "  AND damagePerJob > 0 ",
                               "  AND r.typeID     = ", blueprintID,
                               "  AND r.activityID = 1", sep = '');


            extra.dt <- data.table(dbGetQuery(dbconnect, sql.query));
            extra.dt <- cbind(mineral.dt[rep(1, dim(extra.dt)[1]), list(typeID, typeName)], extra.dt);

            total.dt <- rbind(mineral.dt[, list(typeID, typeName, matTypeID, matTypeName, quantity, wasteFactor)], extra.dt);
        } else {
            total.dt <- mineral.dt;
        }

        total.dt <- within(total.dt, { wasteFactor = as.numeric(wasteFactor) });

        if(dim(total.dt)[1] > 0) {
            total.dt <- within(total.dt, {
                typeName    = as.character(typeName);
                matTypeName = as.character(matTypeName);
            });
        }

        return(total.dt);
    };

    return(data.table(typeID = typeID)[, get.data(typeID), by = typeID][, -1, with = F]);
}


get.blueprint.id <- function(typeID) {
    inputID <- typeID;

    typeNames <- paste(item.dt[typeID %in% inputID]$typeName, 'Blueprint');

    return(item.dt[typeName %in% typeNames]$typeID);
}


get.component.data <- function(typeID, dbconnect = static.dbconnection) {
    get.data <- function(iterID) {
        sql.query <- paste("SELECT construct.typeID as constructTypeID, construct.typeName constructTypeName, ",
                           "       material.typeID, material.typeName, quantity ",
                           "FROM invtypes AS construct, invtypes AS material, invtypematerials ",
                           "WHERE construct.typeID = invtypematerials.typeID ",
                           "  AND material.typeID  = invtypematerials.materialTypeID ",
                           "  AND material.typeID  = '", iterID, "'", sep = '');

        return(dbGetQuery(dbconnect, sql.query));
    };

    return(data.table(typeID = typeID)[, get.data(typeID), by = typeID]);
}


get.item.data <- function(dbconnect = static.dbconnection) {
    sql.query <- paste("SELECT invTypes.typeID, invTypes.typeName typeName,",
                       "       mass, volume, capacity, portionSize, basePrice, invTypes.published, invTypes.marketGroupID, chanceOfDuplicating,",
                       "       invGroups.groupID, invGroups.groupName,",
                       "       invCategories.categoryID, invCategories.categoryName",
                       "FROM invTypes",
                       "    INNER JOIN invGroups      ON invTypes.groupID     = invGroups.groupID",
                       "    INNER JOIN invCategories  ON invGroups.categoryID = invCategories.categoryID");

    return(data.table(dbGetQuery(dbconnect, sql.query), key = 'typeID'));
}


get.material.data <- function(typeID, dbconnect = static.dbconnection) {
    get.data <- function(iterID) {
        sql.query <- paste("SELECT c.typeName, m.typeID, m.typeName, r.quantity, r.damagePerJob ",
                           "FROM invTypes AS c, invTypes AS m, ramTypeRequirements r ",
                           "WHERE c.typeID = r.typeID ",
                           "  AND m.typeID = r.requiredTypeID ",
                           "  AND m.typeID = '", iterID, "'", sep = '');

        return(dbGetQuery(dbconnect, sql.query));
    };

    return(data.table(typeID = typeID)[, get.data(typeID), by = typeID]);
}


get.name.data <- function(dbconnect = static.dbconnection) {
    sql.query <- paste("SELECT * FROM invNames");

    return(data.table(dbGetQuery(dbconnect, sql.query), key = 'itemID'));
}
