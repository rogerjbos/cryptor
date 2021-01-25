#' Mapping from CoinMarketCap id to crypto symbol
#'
#' @name get_cmc_map
#' @title get_cmc_map
#' @encoding UTF-8
#' @concept Mapping from CoinMarketCap id to crypto symbol
#'
#' @return data.table with mapping
#'
#' @examples
#' get_cmc_map()
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_cmc_map <- function() .map

#' Function to return the crypto's matching the search criteria
#'
#' @name get_cmc_name
#' @title get_cmc_name
#' @encoding UTF-8
#' @concept Function to return the crypto's matching the search criteria
#' @param txt string to seach for in the CoinMarketCap mapping table
#'
#' @return data.table with all observations that match the input string
#'
#' @examples
#' get_cmc_name("luna")
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_cmc_name <- function(txt) .map[grep(txt, .map$name, ignore.case = TRUE)]

#' Function to get CoinMarketCap id based on a crypto symbol
#'
#' @name get_cmc_id
#' @title get_cmc_id
#' @encoding UTF-8
#' @concept Lookup corresponding id to use to download prices for the correct crypto
#'
#' @param txt string text to lookup in mapping object
#'
#' @return numberic id
#'
#' @examples
#' get_cmc_id("luna")
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' @export
get_cmc_id <- function(txt) .map[symbol == toupper(txt), id]

#' Get prices from the CoinMarketCap web api
#'
#' @name get_cmc_price
#' @title get_cmc_price
#' @encoding UTF-8
#' @concept Get prices from the CoinMarketCap web api
#' @param id numeric id or string symbol
#' @param time_start starting date
#' @param time_end end date, defaulting to today
#'
#' @return data.table with symbol, OHLC, volume, and name for the input id
#'
#' @examples
#' get_cmc_price(4172)
#' get_cmc_price("dot")
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_cmc_price <- function(id = NULL, time_start = '2018-01-01', time_end = Sys.Date()) {

  if (is.null(id)) stop("Error: Either a [numeric] `id` or [character] `symbol` must be specified.")
  if (!is.numeric(id)) id <- get_cmc_id(toupper(id))
  if (length(id) > 1) stop("Error: More than one id found for symbol.  Please use numberic id instead.")

  url <- paste0("https://web-api.coinmarketcap.com/v1/cryptocurrency/ohlcv/historical?id=", id, "&convert=USD&time_start='", time_start, "'&time_end='", time_end, "'")
  j <- jsonlite::read_json(url)
  rawdata <- rbindlist(j$data$quotes)
  data <- rbindlist(rawdata$quote)
  data[, symbol := j$data$symbol]
  data[, name := j$data$name]
  data[, .(symbol, timestamp, open, high, low, close, volume, market_cap, name)]

}


# .map <- fread(file = "/Users/rogerbos/data/map.csv")
# save(.map, file = "/Users/rogerbos/R_HOME/cryptor/data/map.RData")

# x <- sample(1000)
# usethis::use_data(.map, internal = TRUE)
