get.component.data <- function(typeID, dbconnect = data.connection) {
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
