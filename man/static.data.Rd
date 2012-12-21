\name{static.data}
\alias{calculate.implied.mineral.prices}
\alias{construct.base.price.matrix}
\alias{construct.recycle.data}
\alias{get.blueprint.data}
\alias{get.blueprint.id}
\alias{get.component.data}
\alias{get.evecentral.data}
\alias{get.item.data}
\alias{get.material.data}
\alias{get.name.data}
\alias{static.data}
\title{Retrieving price data from eve-central.com}
\arguments{
  \item{typeID}{typeID for the item to be manufactured}

  \item{system}{system for which data is retrieved}

  \item{name.dt}{data.table of names}

  \item{relative.price}{price of mineral relative to price
  of Tritanium}

  \item{oreids}{vector of typeIDs for each ore to be used}

  \item{price.dt}{data.table of prices}

  \item{typeID}{typeID for the item to be queried}

  \item{dbconnect}{connection to the sqlite static data
  dump}
}
\description{
  Retrieving price data from eve-central.com

  Recycling calculation functions

  This function reads in the entries from the CSV trade
  file downloads and returns a data.table of all the unique
  entries
}
\examples{
\dontrun{

}
\dontrun{

}
\dontrun{

}
}
\keyword{recycle}
\keyword{staticdata}

