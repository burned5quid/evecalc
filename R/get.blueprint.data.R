get.blueprint.data <- function(typeID, dbconnect = data.connection) {
    get.data <- function(iterID) {
        ### First get the mineral count
        sql.query <- paste("SELECT c.typeID AS constructTypeID, c.typeName constructTypeName, ",
                           "       m.typeID, m.typeName, quantity, 1 AS wasteFactor ",
                           "FROM invTypes AS c, invTypes AS m, invTypeMaterials itm ",
                           "WHERE c.typeID = itm.typeID ",
                           "  AND m.typeID = itm.materialTypeID ",
                           "  AND c.typeID = ", iterID, sep = '');
        
        mineral.dt <- data.table(dbGetQuery(dbconnect, sql.query));
        
        ### Now we need the extra data
        blueprintID <- get.blueprint.id(iterID);
        
        if(length(blueprintID) > 0) {
            sql.query <- paste("SELECT t.typeID , t.typeName, r.quantity, 0 AS wasteFactor ",
                               "FROM ramTypeRequirements AS r, invTypes AS t ",
                               "WHERE r.requiredTypeID = t.typeID ",
                               "  AND damagePerJob > 0 ",
                               "  AND r.typeID     = ", blueprintID,
                               "  AND r.activityID = 1", sep = '');
            
            
            extra.dt <- data.table(dbGetQuery(dbconnect, sql.query));
            extra.dt <- cbind(mineral.dt[rep(1, dim(extra.dt)[1]), list(constructTypeID, constructTypeName)], extra.dt);
            
            total.dt <- rbind(mineral.dt[, list(constructTypeID, constructTypeName, typeID, typeName, quantity, wasteFactor)], extra.dt);
        } else {
            total.dt <- mineral.dt;
        }
        
        if(dim(total.dt)[1] > 0) {
            total.dt <- within(total.dt, {
                constructTypeName = as.character(constructTypeName);
                typeName          = as.character(typeName);
            });
        }
        
        return(total.dt);
    };
    
    return(data.table(typeID = typeID)[, get.data(typeID), by = typeID][, -1, with = F]);
}
