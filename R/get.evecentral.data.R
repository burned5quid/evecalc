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

get.evecentral.data <- function(typeID = idlist.mineral, system = 'Dodixie', name.dt = name.dt) {

    systemid <- name.dt[itemName %in% system]$itemID;

    root.url <- 'http://api.eve-central.com/api/marketstat';
    
    systems.qs <- paste(systemid, collapse = '&usesystem='); print(systems.qs);
    type.qs    <- paste(typeID,   collapse = '&typeid=');
    
    fetch.url <- paste(root.url, '?typeid=', type.qs, '&usesystem=', systems.qs, sep = '');
    
    cat(paste('Fetching url', fetch.url, '\n'));
    data.xml <- xmlRoot(xmlTreeParse(fetch.url));
    
    typeids <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type'),          xmlGetAttr, 'id')));

    bid <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type/buy/max'),  xmlValue)));
    ask <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type/sell/min'), xmlValue)));
    
    return(data.table(typeID = typeids, system = system, bid = bid, ask = ask, price = ask, key = 'typeID'));
}
