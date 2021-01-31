library(digest)
library(caTools)

# Please see https://support.kraken.com/hc/en-us/articles/360029054811-What-is-the-authentication-algorithm-for-private-endpoints-
# URI path = URL of API call without 'https://api.kraken.com'.
# nonce = A unique identifier which must increase in value with each API call (often a UNIX timestamp, which is the current time in seconds (or milliseconds for higher resolution) since January 1st 1970).
# POST data = Form encoded name/value pairs of the nonce and the API method parameters.
#
# Examples of the variables for a call to the TradeBalance method are as follows
# (note that all of the values are string values regardless of what the values represent):
#
#   URI path = "/0/private/TradeBalance"
# nonce = "1540973848000"
# POST data = "nonce=1540973848000&asset=xxbt"
# Algorithm
#
# Calculate the SHA256 of the nonce and the POST data.
# Decode the API secret (the private part of the API key) from base64.
# Calculate the HMAC of the URI path and the SHA256, using SHA512 as the HMAC hash and the decoded API secret as the HMAC key.
# Encode the HMAC into base64.
# An example of the algorithm using the variables shown above is as follows:
#
#   Base64Encode(HMAC-SHA512 of ("/0/private/TradeBalance" + SHA256("1540973848000nonce=1540973848000&asset=xxbt")) using Base64Decode("FRs+gtq09rR7OFtKj9BGhyOGS3u5vtY/EdiIBO9kD8NFtRX7w7LeJDSrX6cq1D8zmQmGkWFjksuhBvKOAWJohQ==") as the HMAC key
# Helper function called by get_kraken
get_kraken_url <- function(url, key = NULL, secret = NULL) {

  if (is.null(key)) key <- kraken_api_key
  if (is.null(secret)) secret <- kraken_private_key

  if (grepl("*?/public/", url)) {

    out <- jsonlite::fromJSON(url)
    return(out$result)

  } else if (grepl("*?/private/", url)) {
    URI_path <- gsub("^.*?kraken.com","", url)
    nonce <- as.character(as.numeric(Sys.time()) * 1e6)
    post_data <- paste0('nonce=',nonce)
    sign <- hmac(key = base64decode(secret,what='raw'),
     object = c(charToRaw(URI_path),
       digest(object = paste0(nonce, post_data), algo = 'sha256', serialize = FALSE, raw = TRUE)),
     algo = 'sha512', raw = TRUE)
    httpheader <- c('API-Key' = key, 'API-Sign' = base64encode(sign))
    curl_msg <- RCurl::getCurlHandle(useragent = paste("cryptor", packageVersion("cryptor")))
    query_result_json <- rawToChar(RCurl::getURLContent(curl = curl_msg, url = url, binary = TRUE,
      postfields = post_data, httpheader = httpheader))
    out <- jsonlite::fromJSON(query_result_json)
    return(out$result)
  } else {
    stop("Error: Incorrect Kraken URL!")
  }

}

get_kraken <- function(method, pair = '', since = 0, interval = 1, info = "info", count = 20, start = '2021-01-01', end = Sys.Date(), ...) {


  # method = "OHLC"; pair = 'LTCUSD'; count = 20; since = 0; interval = 1
  # Determine URL to send to Kraken api
  switch(method,
         'Ticker'      = { url <- paste0('https://api.kraken.com/0/public/Ticker?pair=', pair) },
         'Trades'      = { url <- paste0('https://api.kraken.com/0/public/Trades?pair=', pair, '&', 'since=', since) },
         'Depth'       = { url <- paste0('https://api.kraken.com/0/public/Depth?pair=', pair, '&count=', count) },
         'OHLC'        = { url <- paste0('https://api.kraken.com/0/public/OHLC?pair=', pair, '&since=', since, '&interval=', interval) },
         'Spread'      = { url <- paste0('https://api.kraken.com/0/public/Spread?pair=', pair, '&', 'since=', since)},
         'AssetPairs'  = { url <- paste0('https://api.kraken.com/0/public/AssetPairs?info=', info, '&pair=', pair) },
         'Ledgers'     = { url <- 'https://api.kraken.com/0/private/Ledgers' },
         # 'Ledgers'     = { url <- paste0('https://api.kraken.com/0/private/Ledgers&start=', as.numeric(as.POSIXct(start)), '&end=', as.numeric(as.POSIXct(end))) },
         # 'Ledgers'     = { url <- paste0('https://api.kraken.com/0/private/Ledgers&start=', as.POSIXct(start)) },
         'Assets'      = { url <- 'https://api.kraken.com/0/public/Assets' },
         'Time'        = { url <- 'https://api.kraken.com/0/public/Time' },
         'Balance'     = { url <- 'https://api.kraken.com/0/private/Balance' },
         'OpenOrders'  = { url <- 'https://api.kraken.com/0/private/OpenOrders' },
         'CancelOrder' = { url <- 'https://api.kraken.com/0/private/CancelOrder' })

  # Call helper function
  res <- get_kraken_url(url)

  # Convert json list to data.table
  if (method == "Ticker") {

    pairs <- names(res)
    tickers <- list()
    for (pair in pairs) {
      tickers[[pair]] <- data.table(query_time = Sys.time(), pair = pair,
                                    open = as.numeric(res[[pair]][['o']][1]),
                                    high = as.numeric(res[[pair]][['h']][1]),
                                    low = as.numeric(res[[pair]][['l']][1]),
                                    last = as.numeric(res[[pair]][['c']][1]),
                                    vwap = as.numeric(res[[pair]][['p']][2]),
                                    volume = as.numeric(res[[pair]][['v']][2]),
                                    ask = as.numeric(res[[pair]][['a']][1]),
                                    bid = as.numeric(res[[pair]][['b']][1]))
    }
    out <- rbindlist(tickers)

  } else if (method == "OHLC") {

    pair <- names(res)[1]
    out <- data.table(pair = pair, res[[pair]])
    setnames(out, c("pair","time","open","high","low","close","vwap","volume","count"))
    out[, time := as.POSIXct(as.numeric(time), origin="1970-01-01", TZ='UTC')]
    out

  } else if (method == "Balance") {

    pairs <- names(res)
    balances <- list()
    for (pair in pairs) {
      balances[[pair]] <- data.table(query_time = Sys.time(), pair = pair, balance = as.numeric(res[[pair]]))
    }
    out <- rbindlist(balances)
    out <- out[balance != 0]

  } else if (method == "Trades") {

    pairs <- names(res)
    trades <- list()
    for (pair in pairs) {
      if (pair != 'last') trades[[pair]] <- data.table(query_time = Sys.time(), pair = pair, res[[pair]])
    }
    out <- rbindlist(trades)

  } else if (method == "Depth") {

    pair <- names(res)[1]
    ask <- data.table(res[[pair]]$ask)
    setnames(ask, paste0("ask.", c("price","volume","time")))
    ask$ask.time <- as.POSIXct(as.numeric(ask$ask.time), origin="1970-01-01", TZ='UTC')

    bid <- data.table(res[[pair]]$bid)
    setnames(bid, paste0("bid.", c("price","volume","time")))
    bid$bid.time <- as.POSIXct(as.numeric(bid$bid.time), origin="1970-01-01", TZ='UTC')

    out <- data.table(bid, ask)
    out <- out[, c('bid.time','bid.volume','bid.price','ask.price','ask.volume','ask.time')]

  } else if (method == "Ledgers") {

    ids <- names(res[['ledger']])
    ledger <- list()
    for (id in ids) {
      # id <- ids[1]
      ledger[[id]] <- data.table(id = id,
        time = res[['ledger']][[id]][['time']],
        type = res[['ledger']][[id]][['type']],
        subtype = res[['ledger']][[id]][['subtype']],
        aclass = res[['ledger']][[id]][['aclass']],
        asset = res[['ledger']][[id]][['asset']],
        amount = res[['ledger']][[id]][['amount']],
        fee = res[['ledger']][[id]][['fee']],
        balance = res[['ledger']][[id]][['balance']])
    }
    out <- rbindlist(ledger)
    out$time <- as.POSIXct(as.numeric(out$time), origin="1970-01-01", TZ='UTC')

  } else if (method == "OpenOrders") {

    oids <- names(res[['open']])
    orders <- list()
    for (oid in oids) {
      orders[[oid]] <- data.table(query_time = Sys.time(), order_id = oid,
        pair = res[['open']][[oid]][['descr']][['pair']],
        type = res[['open']][[oid]][['descr']][['type']],
        ordertype = res[['open']][[oid]][['descr']][['ordertype']],
        price = res[['open']][[oid]][['descr']][['price']],
        price2 = res[['open']][[oid]][['descr']][['price2']],
        leverage = res[['open']][[oid]][['descr']][['leverage']],
        order = res[['open']][[oid]][['descr']][['order']])
    }
    out <- rbindlist(orders)

  } else {
    out <- res
  }
  return(out)
}

if (FALSE) {

  get_kraken("Ticker", pair = "DOTUSD")
  get_kraken("Trades", pair = "DOTUSD")
  get_kraken("Depth", pair = "DOTUSD")
  get_kraken("OHLC", pair = "DOTUSD") # ?
  get_kraken("Spread", pair = "DOTUSD")
  get_kraken("AssetPairs", pair = "DOTUSD")
  get_kraken("Assets")
  get_kraken("Time")
  get_kraken("Balance")
  get_kraken("OpenOrders")

}





#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/public/Ticker?pair=XXBTZEUR')
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/private/Balance',
#'                  key = '', secret = '')
#' # order book
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/public/Depth?pair=XXBTZEUR')
#' # open orders
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/private/OpenOrders',
#'                  key = '', secret = '')
#' # place order
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/private/AddOrder',
#'                  key = '', secret = '',
#'                  req = list(pair = 'XXBTZEUR',
#'                             type = 'sell',
#'                             ordertype = 'limit',
#'                             price = 1200, # 1200 eur
#'                             volume = 0.1)) # 0.1 btc
#' # cancel order
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/private/CancelOrder',
#'                  key = '', secret = '',
#'                  req = list(txid = 'id_from_open_orders'))
#' # trades
#' market.api.query(market = 'kraken',
#'                  url = 'https://api.kraken.com/0/public/Trades?pair=XXBTZEUR')
#' }

