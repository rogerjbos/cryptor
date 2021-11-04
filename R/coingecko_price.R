# To update map!
if (!exists(".map")) .map <- read.csv("data-raw/gecko.csv")
# usethis::use_data(.map, internal = TRUE, overwrite = TRUE)

#' Get status from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_ping
#' @title get_coingecko_ping
#' @encoding UTF-8
#' @concept Get status from the CoinGecko web api
#' @param None
#'
#' @return string "(V3) To the Moon!" means everything is okay on the CoinGecko side.
#'
#' @examples
#' tmp <- get_coingecko_ping()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_ping <- function() {
  j <- jsonlite::read_json(path="https://api.coingecko.com/api/v3/ping")
  j$gecko_says
}

#' Mapping from CoinGecko id to crypto symbol
#'
#' @name get_coingecko_map
#' @title get_coingecko_map
#' @encoding UTF-8
#' @concept Mapping from CoinGecko id to crypto symbol
#'
#' @return data.frame with mapping
#'
#' @examples
#' get_coingecko_map()
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_coingecko_map <- function() .map

#' Get list of supported vs currencies from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_currencies
#' @title get_coingecko_currencies
#' @encoding UTF-8
#' @concept Get status from the CoinGecko web api
#' @param None
#'
#' @return vector of supported currencies
#'
#' @examples
#' tmp <- get_coingecko_currencies()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_currencies <- function() {
  jsonlite::read_json(path="https://api.coingecko.com/api/v3/simple/supported_vs_currencies", simplifyVector = TRUE)
}

#' Function to return the crypto's matching the search criteria
#'
#' @name get_coingecko_name
#' @title get_coingecko_name
#' @encoding UTF-8
#' @concept Function to return the crypto's matching the search criteria
#' @param txt string to search for in the CoinGecko mapping table
#'
#' @return data.frame with all observations that match the input string
#'
#' @examples
#' get_coingecko_name("luna")
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_coingecko_name <- function(txt) {

  .map[grep(txt, .map$Name, ignore.case = TRUE), ]

}

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
get_coingecko_id_from_symbol <- function(tag) {

  .map[.map$Symbol == tolower(tag), ]

}

get_coingecko_verify_id <- function(tag) {

  nrow(.map[.map$Id == tolower(tag)])==1

}

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
#' @return data.frame of date, open, high, low, close, Id, and epoch (unix time)
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
  j <- as.data.frame(jsonlite::read_json(url, simplifyVector = TRUE))
  names(j) <- c('epoch','open','high','low','close')
  j$date <- as.POSIXct(j$epoch / 1000, origin = "1970-01-01")
  j$Id <- id
  j[, c('date','open','high','low','close','Id','epoch')]

}

# tmp <- get_coingecko_ohlc("cardano", days = 30)
# head(tmp)
# library(xts)
# xtmp <- xts(tmp[, c("open","high","low","close")], order.by = tmp$date, frequency = 4)
# to.daily(xtmp)


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
#' @return data.frame of date, price, market_cap, volume, Id, and epoch (unix time)
#'
#' @examples
#' tmp <- get_coingecko_history("cardano", days = 30)
#' tmp <- get_coingecko_history("bitcoin", days = "max")
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_history <- function(id = NULL, cur = 'usd', days = "max") {

  # id <- 'bitcoin'; cur = 'usd'; days = "max"
  if (is.null(id)) stop("Error: a [character] `id` must be specified.")

  url <- paste0("https://api.coingecko.com/api/v3/coins/", id, "/market_chart?vs_currency=", cur, "&days=" , days, "&interval=daily")
  j <- as.data.frame(jsonlite::read_json(url, simplifyVector = TRUE))
  names(j) <- c('epoch','price','epoch2','market_cap','epoch3','volume')
  j$date <- as.POSIXct(j$epoch / 1000, origin = "1970-01-01")
  j$Id <-id
  j[, c('date','price','market_cap','volume','Id','epoch')]

}

#' Get markets descriptions from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_markets
#' @title get_coingecko_markets
#' @encoding UTF-8
#' @concept Get markets descriptions from the CoinGecko web api
#' @param cur string currency (defaults to 'usd') as listed in get_coingecko_currencies()
#' @param ord string order of the data frame returned, one of: "market_cap_desc", "gecko_desc", "gecko_asc", "market_cap_asc", "market_cap_desc", "volume_asc", "volume_desc", "id_asc", "id_desc"
#'
#' @return data.table of market date
#'
#' @examples
#' tmp <- get_coingecko_markets(ord = 'market_cap_desc')
#' tmp <- get_coingecko_markets(ord = 'gecko_desc')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_markets <- function(cur = 'usd', ord = 'market_cap_desc') {

  valid_cur <- get_coingecko_currencies()
  if (!cur %in% valid_cur) stop(paste("Error: cur must be one of:", paste0(valid_cur, collapse = ", ")))

  valid_ord <- c("market_cap_desc", "gecko_desc", "gecko_asc", "market_cap_asc", "market_cap_desc", "volume_asc", "volume_desc", "id_asc", "id_desc")
  if (!ord %in% valid_ord) stop(paste("Error: ord must be one of:", paste0(valid_ord, collapse = ", ")))

  url <- paste0("https://api.coingecko.com/api/v3/coins/markets?vs_currency=", cur, "&order=", ord, "&per_page=100&page=1&sparkline=false&price_change_percentage=1h%2C24h%2C7d14d%2C30d%2C200d%2C1y")
  j <- as.data.frame(jsonlite::read_json(url, simplifyVector = TRUE))
  j
}

#' Get list of asset_platforms from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_asset_platforms
#' @title get_coingecko_asset_platforms
#' @encoding UTF-8
#' @concept Get asset_platforms from the CoinGecko web api
#' @param None
#'
#' @return vector of asset_platforms
#'
#' @examples
#' tmp <- get_coingecko_asset_platforms()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_asset_platforms <- function() {
  jsonlite::read_json(path="https://api.coingecko.com/api/v3/asset_platforms", simplifyVector = TRUE)
}


