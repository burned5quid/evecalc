get.material.data <- function(typeID, dbconnect = data.connection) {
    get.data <- function(iterID) {
        sql.query <- paste("SELECT c.typeID, c.typeName, m.typeID, m.typeName, r.quantity, r.damagePerJob ",
                           "FROM invTypes AS c, invTypes AS m, ramTypeRequirements r ",
                           "WHERE c.typeID = r.typeID ",
                           "  AND m.typeID = r.requiredTypeID ",
                           "  AND m.typeID = '", iterID, "'", sep = '');
        
        return(dbGetQuery(dbconnect, sql.query));
    };
    
    return(data.table(typeID = typeID)[, get.data(typeID), by = typeID][, -1, with = F]);
}
