show.asset.stations <- function(showids, count = 3, exclude = 'Caretyn', assetlist.dt = assets.dt) {
    display.dt <- assetlist.dt[typeID %in% showids][!grepl(exclude, stationName)][order(typeID, -quantity)];
    
    display.dt <- display.dt[, .SD[1:(min(count, dim(.SD)[1]))], by = list(typeID, characterID)];
    display.dt <- display.dt[, list(characterID, characterName, regionName, constellationName, stationName, typeName, quantity)];
    
    return(display.dt);
}
