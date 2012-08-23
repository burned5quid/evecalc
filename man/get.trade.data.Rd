\name{get.trade.data}
\alias{get.trade.data}
\title{Retrieves market data for the given typeIDs from the website
    eve-central.com.}
\usage{get.trade.data(trade.files)}
\arguments{
    \item{trade.files}{Character vector for the CSV filenames with trade data.}
}
\value{data.table of trades contained in all the trade files.}
\description{This function loads up all the CSV files supplied and returns a
    data.table with all the unique entries for the trades.}
%\details{}
\examples{
### Loads up all the trades
library(evecalc);

\dontrun{
trade.files <- c('tradefile.csv');

get.trade.data(trade.files);
}}
