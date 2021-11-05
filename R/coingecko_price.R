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

#' Get coin specific data from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_coins
#' @title get_coingecko_coins
#' @encoding UTF-8
#' @concept Get coin specific data from the CoinGecko web api
#' @param cur string currency (defaults to 'usd') as listed in get_coingecko_currencies()
#' @param ord string order of the data frame returned, one of: "market_cap_desc", "gecko_desc", "gecko_asc", "market_cap_asc", "market_cap_desc", "volume_asc", "volume_desc", "id_asc", "id_desc"
#'
#' @return data.table of market date
#'
#' @examples
#' tmp <- get_coingecko_coins('bitcoin')
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_coins <- function(id = 'bitcoin') {

  localization = TRUE; tickers=TRUE; market_data=TRUE; community_data=TRUE; developer_data=TRUE
  url <- paste0("https://api.coingecko.com/api/v3/coins/", id,
    "?localization=", tolower(localization),
    "&tickers=", tolower(tickers),
    "&market_data=", tolower(market_data),
    "&community_data=", tolower(community_data),
    "&developer_data=", tolower(developer_data),
    "&sparkline=false")

  j <- jsonlite::read_json(url, simplifyVector = TRUE)

  out <- data.frame(id = j$id,
    symbol = j$symbol,
    name = j$name,
    block_time_in_minutes = j$block_time_in_minutes,
    hashing_algorithm = j$hashing_algorithm,
    categories = j$categories,
    description = j$description,
    country_origin = j$country_origin,
    genesis_date = j$genesis_date,
    sentiment_votes_up_percentage = j$sentiment_votes_up_percentage,
    sentiment_votes_down_percentage = j$sentiment_votes_down_percentage,
    market_cap_rank = j$market_cap_rank,
    coingecko_rank = j$coingecko_rank,
    coingecko_score = j$coingecko_score,
    developer_score = j$developer_score,
    community_score = j$community_score,
    liquidity_score = j$liquidity_score,
    public_interest_score = j$public_interest_score,
    t(j$links),
    t(j$community_data),
    t(j$developer_data),
    t(j$public_interest_stats),
    last_updated = j$last_updated)

  mkt_data <- t(j$market_data)
  tickers_df <- j$tickers

  list(out, mkt_data, tickers_df)

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

#' Get list of coin categories from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_coin_categories
#' @title get_coingecko_coin_categories
#' @encoding UTF-8
#' @concept Get coin categories from the CoinGecko web api
#' @param None
#'
#' @return data.frame of coin categories
#'
#' @examples
#' tmp <- get_coingecko_coin_categories()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_coin_categories <- function() {
  jsonlite::read_json(path="https://api.coingecko.com/api/v3/coins/categories", simplifyVector = TRUE)
}

#' Get list of coin categories from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_defi
#' @title get_coingecko_defi
#' @encoding UTF-8
#' @concept Get DeFi stats from the CoinGecko web api
#' @param None
#'
#' @return data.frame of coin categories
#'
#' @examples
#' tmp <- get_coingecko_defi()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_defi <- function() {
  j <- jsonlite::read_json(path="https://api.coingecko.com/api/v3/global/decentralized_finance_defi", simplifyVector = TRUE)
  data.frame(j$data)
}

#' Get cryptocurrency global data from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_global
#' @title get_coingecko_global
#' @encoding UTF-8
#' @concept Get cryptocurrency global data from the CoinGecko web api
#' @param None
#'
#' @return data.frame of cryptocurrency global data
#'
#' @examples
#' tmp <- get_coingecko_global()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_global <- function() {
  j <- jsonlite::read_json(path="https://api.coingecko.com/api/v3/global", simplifyVector = TRUE)
  data.frame(j$data)
}




#' Get public treasury holdings from the CoinGecko web api
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_public_treasury
#' @title get_coingecko_public_treasury
#' @encoding UTF-8
#' @concept Get public treasury holdings from the CoinGecko web api
#' @param None
#'
#' @return data.frame of treasury holdings by public companies
#'
#' @examples
#' tmp <- get_coingecko_public_treasury()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_public_treasury <- function() {
  bitcoin <- jsonlite::read_json(path="https://api.coingecko.com/api/v3/companies/public_treasury/bitcoin", simplifyVector = TRUE)
  ethereum <- jsonlite::read_json(path="https://api.coingecko.com/api/v3/companies/public_treasury/ethereum", simplifyVector = TRUE)
  bitcoin$companies$symbol <- gsub(" ", "", bitcoin$companies$symbol, fixed = TRUE)
  ethereum$companies$symbol <- gsub(" ", "", ethereum$companies$symbol, fixed = TRUE)

  both <- merge(bitcoin$companies, ethereum$companies,
                by = "symbol", all = TRUE)
  both$name.x[is.na(both$name.x)] <- both$name.y[is.na(both$name.x)]
  both$country.x[is.na(both$country.x)] <- both$country.y[is.na(both$country.x)]
  both <- both[, -c(8:9)]
  names(both) <- c("symbol","name","country","BTC_holdings","BTC_entry_value_usd",
                   "BTC_current_value_usd","BTC_pct_of_total_supply","ETH_holdings",
                   "ETH_entry_value_usd","ETH_current_value_usd","ETH_pct_of_total_supply")
  both

}
