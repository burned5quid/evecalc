\name{show.last.trades}
\alias{show.last.trades}
\title{Returns a data.table that contains the last number of trades in the given
    trades data.table.}
\usage{show.last.trades(typeID, count = 10, side = 'buy', trade.dt = trade.dt)}
\arguments{
    \item{typeID}{The typeID of the item(s) under inspection.}
    \item{count}{Integer number of rows required.}
    \item{side}{Side of transactions wanted (value should be 'buy' or 'sell').}
    \item{trade.dt}{data.table of trades}
}
\value{data.table of trades matching the criteria.}
\description{This function filters out all the trades apart from the last
    'count' trades on either the 'buy' or 'sell' side.}
%\details{}
\examples{
### Show the last 5 sell trades for the Brutix Battlecruiser (id 16229)
library(evecalc);

\dontrun{
show.last.trades(16229, 5, 'sell', trade.dt);
}}
