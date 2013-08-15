#' Retrieving price data from eve-central.com
#'
#'
#'
#' @usage get.evecentral.data(typeID, system)
#' @keywords recycle
#' @param typeID typeID for the item to be manufactured
#' @param system system for which data is retrieved
#' @param name.dt data.table of names
#' @name static.data
#' @aliases get.evecentral.data
#' @export get.evecentral.data
#' @import data.table
#' @import XML
#' @examples
#' \dontrun{
#'
#' }
NULL

get.evecentral.data <- function(typeID = idlist.mineral, system = 'Dodixie', name.dt = name.dt, block.size = 20) {

    root.url <- 'http://api.eve-central.com/api/marketstat';

    get.data <- function(data.dt, by.lst) {

        iter.system <- by.lst$system;
        systemid <- name.dt[itemName %in% iter.system]$itemID;

        iter.typeID <- data.dt$typeID;

        systems.qs <- paste(systemid, collapse = '&usesystem=');
        type.qs    <- paste(iter.typeID, collapse = '&typeid=');

        fetch.url <- paste(root.url, '?typeid=', type.qs, '&usesystem=', systems.qs, sep = '');

        cat(paste('Fetching url', fetch.url, '\n'));
        data.xml <- xmlRoot(xmlTreeParse(fetch.url));

        typeids <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type'),          xmlGetAttr, 'id')));

        bid <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type/buy/max'),  xmlValue)));
        ask <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type/sell/min'), xmlValue)));

        iter.dt <- data.table(typeID = typeids, bid = bid, ask = ask, price = ask);

        return(iter.dt);
    }

    data.dt <- CJ(system = system, typeID = typeID);
    N       <- dim(data.dt)[1];

    data.dt[, blockid := rep(1:N, each = block.size, length.out = N)];

    price.dt <- data.dt[, get.data(.SD, .BY), by = list(system, blockid)];

    price.dt[, blockid := NULL];
    price.dt[, date    := as.Date(Sys.time())];


    setkey(price.dt, typeID);

    return(price.dt);
}
