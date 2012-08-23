\name{calculate.construction.profit}
\alias{calculate.construction.profit}
\title{Calculates the construction profit for an item, calculating the
    construction cost for the item and then comparing it to its market price. It
    then calculates the margin on the item.}
\usage{calculate.construction.profit(typeID, ...)}
\arguments{
    \item{typeID}{The typeID of the item(s) under construction.}
    \item{...}{Extra arguments passed through into the construction
               calculations.}
}
\value{data.table giving profit data for the construction.}
\description{This function first creates a bill of materials for the item with
    given typeID, then does the same for any of those items. It then aggregates
    all this information and connects it to the price for the inputs. The total
    construction cost is then calculated.}
%\details{}
\examples{
### Calculates the construction profit for a Brutix (typeID 16229) with a
### blueprint with ME 30. The price data is stored in the data.table
### pricedata.dt. 
library(evecalc);

\dontrun{
calculate.construction.profit(16229,
                              ME = 30,
                              price.dt = pricedata.dt);
}}
