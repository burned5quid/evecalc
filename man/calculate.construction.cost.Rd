\name{calculate.construction.cost}
\alias{calculate.construction.cost}
\title{Calculates the construction cost for an item, using prices for all input
    components, without recursively calculating construction costs for any
    components.}
\usage{calculate.construction.cost(typeID, ME = 0, price.dt,
                                   bpcost.dt = NULL, verbose = FALSE,
                                   dbconnect = data.connection)}
\arguments{
    \item{typeID}{The typeID of the item(s) under construction.}
    \item{ME}{A numeric value, giving the ME of the blueprint.}
    \item{price.dt}{data.table of prices for the components used in the
                    construction.}
    \item{bpcost.dt}{data.table of prices for the blueprint used.}
    \item{verbose}{switch for verbose output.}
    \item{dbconnect}{data connection to the Eve static data}
}
\value{data.table giving price data for the construction.}
\description{Calculates the basic cost of an item. The costing uses the price of
the input, it does not recursively calculate the cost of building any
components.}
%\details{}
\examples{
### Calculate the construction cost of Brutix battlecruiser. The price data is
### stored in pricedata.dt. The typeID for the Brutix is 16229.
library(evecalc);

\dontrun{
calculate.construction.cost(16229,
                            ME = 30,
                            price.dt = pricedata.dt,
                            bpcost.dt = NULL,
                            verbose = FALSE,
                            dbconnect = data.connection);
}}
