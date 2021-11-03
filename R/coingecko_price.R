# To update map!
if (!exists(".map")) {
  if (requireNamespace("data.table", quietly=TRUE)) {
    .map <<- data.table::fread("data-raw/gecko.csv")
  } else {
    .map <<- read.csv("data-raw/gecko.csv")
  }
}
# usethis::use_data(.map, internal = TRUE, overwrite = TRUE)

#' Mapping from CoinGecko id to crypto symbol
#'
#' @name get_coingecko_map
#' @title get_coingecko_map
#' @encoding UTF-8
#' @concept Mapping from CoinGecko id to crypto symbol
#'
#' @return data.table with mapping
#'
#' @examples
#' get_coingecko_map()
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_coingecko_map <- function() .map

#' Function to return the crypto's matching the search criteria
#'
#' @name get_coingecko_name
#' @title get_coingecko_name
#' @encoding UTF-8
#' @concept Function to return the crypto's matching the search criteria
#' @param txt string to seach for in the CoinGecko mapping table
#'
#' @return data.table with all observations that match the input string
#'
#' @examples
#' get_coingecko_name("luna")
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
# get_coingecko_name <- function(txt) .map[grep(txt, .map$Name, ignore.case = TRUE)]
get_coingecko_name <- function(txt) .map[grep(txt, Name, ignore.case = TRUE)]

#' Function to get CoinGecko id based on a crypto symbol
#'
#' @name get_coingecko_id_from_symbol
#' @title get_coingecko_id_from_symbol
#' @encoding UTF-8
#' @concept Lookup corresponding id to use to download prices for the correct crypto
#'
#' @param txt string text to lookup in mapping object
#'
#' @return numberic id
#'
#' @examples
#' get_coingecko_id_from_symbol("luna")
#' get_coingecko_id_from_symbol("LUNA")
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_coingecko_id_from_symbol <- function(tag) .map[Symbol == tolower(tag)]

get_coingecko_verify_id <- function(tag) nrow(.map[Id == tolower(tag)])==1

#' Get LATEST price from the CoinMarketCap web api for a given Id (not Symbol)
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_price
#' @title get_coingecko_price
#' @encoding UTF-8
#' @concept Get prices from the CoinGecko web api
#' @param id string Id
#' @param cur string currency (defaults to 'usd')
#'
#' @return float price
#'
#' @examples
#' get_coingecko_price("cardano")
#' get_coingecko_price("bitcoin")
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_price <- function(id = NULL, cur = 'usd') {

  if (is.null(id)) stop("Error: a [character] `id` must be specified.")

  url <- paste0("https://api.coingecko.com/api/v3/simple/price?ids=", id, "&vs_currencies=", cur)
  j <- jsonlite::read_json(url)
  # return the latest price
  unlist(j[1])

}

#' Get OHLC candle data from the CoinGecko web api for a given Id (not Symbol)
#' Candle's body:
#' 1 - 2 days: 30 minutes
#' 3 - 30 days: 4 hours
#' 31 and before: 4 days
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_ohlc
#' @title get_coingecko_ohlc
#' @encoding UTF-8
#' @concept Get prices from the CoinGecko web api
#' @param id string Id
#' @param cur string currency (defaults to 'usd')
#' @param days integer (1/7/14/30/90/180/365) or string "max"
#'
#' @return data.table of date, open, high, low, close, Id, and epoch (unix time)
#'
#' @examples
#' tmp <- get_coingecko_ohlc("cardano", days = 30)
#' tmp <- get_coingecko_ohlc("bitcoin", days = "max")
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_ohlc <- function(id = NULL, cur = 'usd', days = "max") {

  # id <- 'bitcoin'; cur = 'usd'; days = 30
  if (is.null(id)) stop("Error: a [character] `id` must be specified.")

  url <- paste0("https://api.coingecko.com/api/v3/coins/", id, "/ohlc?vs_currency=", cur, "&days=" , days)
  j <- jsonlite::read_json(url, simplifyVector = TRUE) %>%
    as.data.table() %>%
    setnames(c('epoch','open','high','low','close'))
  j[, date := lubridate::as_datetime(epoch / 1000)]
  j[, Id := id]
  setcolorder(j, c('date','open','high','low','close','Id','epoch'))
  j
}


#' Get historical data from the CoinGecko web api for a given Id (not Symbol)
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_history
#' @title get_coingecko_history
#' @encoding UTF-8
#' @concept Get historical prices from the CoinGecko web api
#' @param id string Id
#' @param cur string currency (defaults to 'usd')
#' @param days integer (1/7/14/30/90/180/365) or string "max"
#'
#' @return data.table of date, price, market_cap, volume, Id, and epoch (unix time)
#'
#' @examples
#' tmp <- get_coingecko_history("cardano", days = 30)
#' tmp <- get_coingecko_history("bitcoin", days = "max")
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_history <- function(id = NULL, cur = 'usd', days = "max") {

  # id <- 'bitcoin'; cur = 'usd'; days = 30
  if (is.null(id)) stop("Error: a [character] `id` must be specified.")

  url <- paste0("https://api.coingecko.com/api/v3/coins/", id, "/market_chart?vs_currency=", cur, "&days=" , days, "&interval=daily")
  j <- jsonlite::read_json(url, simplifyVector = TRUE) %>%
    as.data.table() %>%
    setnames(c('epoch','price','epoch2','market_cap','epoch3','volume'))
  j[, date := lubridate::as_datetime(epoch / 1000)]
  j[, Id := id]
  j[, epoch2 := NULL]
  j[, epoch3 := NULL]
  setcolorder(j, c('date','price','market_cap','volume','Id','epoch'))
  j
}

