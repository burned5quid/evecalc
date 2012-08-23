get.name.data <- function(dbconnect = data.connection) {
    sql.query <- paste("SELECT * FROM invNames");
    
    return(data.table(dbGetQuery(dbconnect, sql.query), key = 'itemID'));
}
