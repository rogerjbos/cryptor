% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coinmarketcap_price.R
\encoding{UTF-8}
\name{get_cmc_name}
\alias{get_cmc_name}
\title{get_cmc_name}
\usage{
get_cmc_name(txt)
}
\arguments{
\item{txt}{string to seach for in the CoinMarketCap mapping table}
}
\value{
data.table with all observations that match the input string
}
\description{
Function to return the crypto's matching the search criteria
}
\examples{
get_cmc_name("luna")

}
\author{
Roger J. Bos, CFA, \email{me@rogerjbos.com}
}
\concept{Function to return the crypto's matching the search criteria}
