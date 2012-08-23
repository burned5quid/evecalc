get.item.data <- function(dbconnect = data.connection) {
    sql.query <- paste("SELECT invTypes.typeID, invTypes.typeName typeName,",
                       "       mass, volume, capacity, portionSize, basePrice, invTypes.published, chanceOfDuplicating,",
                       "       invGroups.groupID, invGroups.groupName,",
                       "       invCategories.categoryID, invCategories.categoryName",
                       "FROM invTypes",
                       "    INNER JOIN invGroups      ON invTypes.groupID     = invGroups.groupID",
                       "    INNER JOIN invCategories  ON invGroups.categoryID = invCategories.categoryID");
    
    return(data.table(dbGetQuery(dbconnect, sql.query), key = 'typeID'));
}
