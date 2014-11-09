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

    get.data <- function(data.dt, iter.system) {
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

    price.dt <- data.dt[, get.data(.SD, system), by = list(system, blockid)];

    price.dt[, blockid := NULL];
    price.dt[, date    := as.Date(Sys.time())];


    setkey(price.dt, typeID);

    return(price.dt);
}


update.prices <- function(typeID = idlist.marketdata, system = c('Jita', 'Amarr', 'Dodixie', 'Rens'), price.system = 'Jita',
                          idlist.buyitems = idlist.buyitems, idlist.sellitems = idlist.sellitems, name.dt = name.dt, block.size = 20) {

    prices.hubs.dt <- get.evecentral.data(idlist.marketdata, name.dt = name.dt, system = c('Jita', 'Amarr', 'Rens', 'Dodixie'));
    use.prices.dt  <- prices.hubs.dt;
    setkey(use.prices.dt, typeID);

    date.str <- format(unique(prices.hubs.dt$date), '%Y%m%d');

    prices.name <- paste("prices.hubs.", date.str, sep = '');

    prices.var <- paste(prices.name, ".dt", sep = '');

    assign(prices.var, prices.hubs.dt)

    save(list = prices.var, file = paste("data/noautoload/prices/", prices.name, ".rda", sep = ''), compress = 'xz');


    y790.buy.dt  <- item.dt[use.prices.dt][system == price.system][typeID %in% idlist.buyitems] [, list(typeID, date, typeName, groupName, categoryName, price = round(bid * 0.9, 2))];
    y790.sell.dt <- item.dt[use.prices.dt][system == price.system][typeID %in% idlist.sellitems][, list(typeID, date, typeName, groupName, categoryName, price = round(bid * 1.05, 2))];

    setkey(caprica.buy.dt, typeName);

    prices.lst <- list(y790.buy.dt    = y790.buy.dt,
                       y790.sell.dt   = y790.sell.dt,
                       prices.hubs.dt = prices.hubs.dt);
}
