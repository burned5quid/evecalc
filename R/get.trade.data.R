### This function reads in the entries from the CSV trade file downloads
### and returns a data.table of all the unique entries

get.trade.data <- function(trade.files) {
#    trade.files <- dir('data/noautoload', pattern = 'trades_', full.names = T);

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
