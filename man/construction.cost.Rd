\name{construction.cost}
\alias{calculate.advanced.construction.cost}
\alias{calculate.construction.cost}
\alias{calculate.construction.profit}
\alias{construction.cost}
\title{Calculate manufacturing cost}
\arguments{
  \item{typeID}{typeID for the item to be manufactured}

  \item{ME}{vector of length (1 or 2) for the ME of the
  main blueprint (and the components)}

  \item{price.dt}{data.table for the price data}

  \item{bpcost.dt}{data.table for the cost of BPCs}

  \item{verbose}{logical indicating whether verbose output
  is wanted}

  \item{dbconnect}{db connection to the sqlite database of
  static data}
}
\description{
  Calculate manufacturing cost
}
\examples{
\dontrun{
### Calculate the cost of building a Manticore (id 12031)
### with ME -4 and component ME 30
manticoreID <- 12031;
calculate.advanced.construction.cost(manticoreID, ME = c(-4, 30), price.dt = pricedata.dt)
}
}
\keyword{manufacture}

