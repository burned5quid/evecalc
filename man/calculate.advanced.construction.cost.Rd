\name{calculate.advanced.construction.cost}
\alias{calculate.advanced.construction.cost}
\title{Calculates the construction cost for an item, recursively calculating the
    construction cost of any components.}
\usage{calculate.advanced.construction.cost(typeID, ME = c(0, 0), price.dt,
                                            bpcost.dt = NULL, verbose = FALSE,
                                            dbconnect = data.connection)}
\arguments{
    \item{typeID}{The typeID of the item(s) under construction.}
    \item{ME}{A numeric vector of length 2, giving the ME of the primary
             blueprint and the ME of the component blueprints.}
    \item{price.dt}{data.table of prices for the components used in the
                    construction.}
    \item{bpcost.dt}{data.table of prices for the blueprints used.}
    \item{verbose}{switch for verbose output.}
    \item{dbconnect}{data connection to the Eve static data}
}
\value{data.table giving price data for the construction.}
\description{This function first creates a bill of materials for the item with
    given typeID, then does the same for any of those items. It then aggregates
    all this information and connects it to the price for the inputs. The total
    construction cost is then calculated.}
%\details{}
\examples{
### Calculates the construction cost for an Orca (typeID 28606) with a blueprint
### with ME 6 and component BP MEs of 50. The price data is stored in the
### data.table pricedata.dt
library(evecalc);

\dontrun{
calculate.advanced.construction.cost(28606,
                                     ME = c(5, 50),
                                     price.dt = pricedata.dt,
                                     bpcost.dt = NULL,
                                     verbose = FALSE,
                                     dbconnect = data.connection);
}}
