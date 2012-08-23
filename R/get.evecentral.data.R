get.evecentral.data <- function(typeID = idlist.mineral, system = 'Dodixie') {
    systemid <- name.dt[itemName %in% system]$itemID;
    
    root.url <- 'http://api.eve-central.com/api/marketstat';
    
    systems.qs <- paste(systemid, collapse = '&usesystem=');
    type.qs    <- paste(typeID,   collapse = '&typeid=');
    
    fetch.url <- paste(root.url, '?typeid=', type.qs, '&usesystem=', regions.qs, sep = '');
    
    cat(paste('Fetching url', fetch.url, '\n'));
    data.xml <- xmlRoot(xmlTreeParse(fetch.url));
    
    typeids <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type'),          xmlGetAttr, 'id')));
    minsell <- as.numeric(unlist(lapply(getNodeSet(data.xml, '//type/sell/min'), xmlValue)));
    
    return(data.table(typeID = typeids, price = minsell, key = 'typeID'));
}
