\name{get.evecentral.data}
\alias{get.evecentral.data}
\title{Retrieves market data for the given typeIDs from the website
    eve-central.com.}
\usage{get.evecentral.data(typeID, system)}
\arguments{
    \item{typeID}{The typeID of the item(s) required.}
    \item{system}{System name for the data required.}
}
\value{data.table of price data for each of the typeID(s) supplied.}
\description{This function retrieves market data from eve-central.com and
    stores it all in a data.table which is then returned as the value.}
%\details{}
\examples{
### Retrieves market data for Tritanium (id 34) from the Dodixie system.
library(evecalc);

\dontrun{
get.evecentral.data(34, 'Dodixie');
}}
