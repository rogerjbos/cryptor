% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coingecko_price.R
\encoding{UTF-8}
\name{get_coingecko_coins}
\alias{get_coingecko_coins}
\title{get_coingecko_coins}
\usage{
get_coingecko_coins(Id = "bitcoin", lang = "en")
}
\arguments{
\item{cur}{string currency (defaults to 'usd') as listed in get_coingecko_currencies()}

\item{ord}{string order of the data frame returned, one of: "market_cap_desc", "gecko_desc", "gecko_asc", "market_cap_asc", "market_cap_desc", "volume_asc", "volume_desc", "id_asc", "id_desc"}
}
\value{
data.table of market date
}
\description{
Get coin specific data from the CoinGecko web api
https://www.coingecko.com/en/api/documentation
}
\examples{
tmp <- get_coingecko_coins('ovr')

}
\author{
Roger J. Bos, \email{roger.bos@gmail.com}
}
\concept{Get coin specific data from the CoinGecko web api}
