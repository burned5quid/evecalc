#' Calculate trades
#'
#' This function reads in the entries from the CSV trade file downloads
#' and returns a data.table of all the unique entries
#'
#' @usage get.trade.data(trade.files)
#' @usage calculate.avg.price(trade.dt)
#' @usage show.last.trades(typeID, count = 10, side = 'buy', trade.dt = trade.dt)
#' @keywords trades
#' @param typeID typeID for the item to be manufactured
#' @name trades
#' @aliases get.trade.data calculate.avg.price show.last.trades
#' @export get.trade.data calculate.avg.price show.last.trades
#' @import data.table
#' @examples
#' \dontrun{
#'
#' }
NULL

get.trade.data <- function(trade.files) {
    stopifnot(is.character(trade.files));

    read.trade.file <- function(file) {
        file <- as.character(file);

        return(read.csv(file,
                        stringsAsFactors = F,
                        colClasses = list(transactionID = 'character',
                                          transactTime  = 'POSIXct')));
    }

    alltrade.dt <- data.table(file = trade.files)[, read.trade.file(file), by = file];

    alltrade.dt <- within(alltrade.dt, {
        linetype = NULL;
        file     = NULL;
    });

    trade.dt <- alltrade.dt[!duplicated(transactionID)];

    return(trade.dt);
}


calculate.avg.price <- function(trade.dt) {
    avgprice.dt <- trade.dt[, list(volume   = sum(quantity),
                                   cash     = sum(quantity * price),
                                   avgprice = (sum(quantity * price) / sum(quantity))),
                            by = list(typeID, typeName, transactionType)];
    
    setkey(avgprice.dt, typeID, transactionType);
    
    return(avgprice.dt);
}


show.last.trades <- function(typeID, count = 10, side = 'buy', trade.dt = trade.dt) {
    showID  <- typeID;
    
    show.dt <- trade.dt[typeID %in% showID][transactionType == side][, list(transactionID, transactTime, transactionType, typeID, typeName, quantity, price)];
    
    print(tail(show.dt, n = count));
    
    return();
}
