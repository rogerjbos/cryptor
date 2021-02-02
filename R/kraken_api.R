#' Helper function called by get_kraken
#'
#' @name get_kraken_url
#' @title get_kraken_url
#' @encoding UTF-8
#' @concept Helper function called by get_kraken
#'
#' @return data.table with mapping
#'
#' Please see https://support.kraken.com/hc/en-us/articles/360029054811-What-is-the-authentication-algorithm-for-private-endpoints-
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
get_kraken_url <- function(url, params = NULL, key = NULL, secret = NULL) {

  if (is.null(key)) key <- kraken_api_key
  if (is.null(secret)) secret <- kraken_private_key

  if (grepl("*?/public/", url)) {

    out <- jsonlite::fromJSON(url)
    return(out$result)

  } else if (grepl("*?/private/", url)) {

    URI_path <- gsub("^.*?kraken.com","", url)
    nonce <- as.character(as.numeric(Sys.time()) * 1e6)
    post_data <- ifelse(is.null(params), paste0('nonce=', nonce), paste0('nonce=', nonce, params))
    # Calculate the SHA256 of the nonce and the POST data.
    dig <- digest(object = paste0(nonce, post_data), algo = 'sha256', serialize = FALSE, raw = TRUE)
    message <- c(charToRaw(URI_path), dig)
    # Decode the API secret (the private part of the API key) from base64
    key_decode <- base64decode(secret, what='raw')
    # Calculate the HMAC of the URI path and the SHA256, using SHA512 as the HMAC hash and the decoded API secret as the HMAC key.
    sign <- hmac(key = key_decode, object = message, algo = 'sha512', raw = TRUE)
    # Encode the HMAC into base64.
    httpheader <- c('API-Key' = key, 'API-Sign' = base64encode(sign))
    curl_msg <- RCurl::getCurlHandle(useragent = paste("cryptor", packageVersion("cryptor")))
    # curl
    query_result_json <- RCurl::getURLContent(curl = curl_msg, url = url, postfields = post_data, httpheader = httpheader)
    out <- jsonlite::fromJSON(query_result_json)
    print(out$error)
    return(out$result)

  } else {
    stop("Error: Incorrect Kraken URL!")
  }
}

#' Helper function called by get_kraken
#'
#' @name get_kraken
#' @title get_kraken
#' @encoding UTF-8
#' @concept Helper function called by get_kraken
#' @param method string one of ("Ticker","Trades","Depth","OHLC","Spread","AssetPairs","Assets","Time","Balance","Ledgers","OpenOrders","AddOrder","CalcelOrder","CalcelAll")
#' @param pair string currency pair
#' @param since numeric UNIX time for start of query
#' @param interval numeric, default of 1
#' @param count numeric how many observations to return, used for `Depth`
#'
#' @return data.table or json
#'
#' @author Roger J. Bos, CFA, \email{me@rogerjbos.com}
#' #' @examples
#' get_kraken("Ticker", pair = "DOTUSD")
#' get_kraken("Trades", pair = "DOTUSD")
#' get_kraken("Depth", pair = "DOTUSD")
#' get_kraken("OHLC", pair = "DOTUSD")
#' get_kraken("Spread", pair = "DOTUSD")
#' get_kraken("AssetPairs", pair = "DOTUSD")
#' get_kraken("Assets")
#' get_kraken("Time")
#' get_kraken("Balance")
#' get_kraken("Ledgers")
#' get_kraken("OpenOrders")
#'
#' @export
get_kraken <- function(method, pair = '', since = '2020-01-01', interval = 1, info = "info", txid = '',
                       asset = 'all', type = 'all', ofs = 1, enddate = Sys.Date(), timeout = 60,
                       docalcs = 'true', consolidation = 'market', trades = '', ...) {

  # Determine URL to send to Kraken api
  switch(method,
         'Ticker'      = { url <- paste0('https://api.kraken.com/0/public/Ticker?pair=', pair) },
         'Trades'      = { url <- paste0('https://api.kraken.com/0/public/Trades?pair=', pair, '&', 'since=', as.numeric(as.POSIXct(since))) },
         'Depth'       = { url <- paste0('https://api.kraken.com/0/public/Depth?pair=', pair, '&count=', count) },
         'OHLC'        = { url <- paste0('https://api.kraken.com/0/public/OHLC?pair=', pair, '&since=', as.numeric(as.POSIXct(since)), '&interval=', interval) },
         'Spread'      = { url <- paste0('https://api.kraken.com/0/public/Spread?pair=', pair, '&since=', as.numeric(as.POSIXct(since)))},
         'AssetPairs'  = { url <- paste0('https://api.kraken.com/0/public/AssetPairs?info=', info, '&pair=', pair) },
         'Assets'      = { url <- 'https://api.kraken.com/0/public/Assets' },
         'Time'        = { url <- 'https://api.kraken.com/0/public/Time' },
         'Balance'     = { url <- 'https://api.kraken.com/0/private/Balance' },
         'OpenOrders'  = { url <- 'https://api.kraken.com/0/private/OpenOrders' },
         'CancelAll'   = { url <- 'https://api.kraken.com/0/private/CancelAll' },
         'CancelOrder' = { url <- 'https://api.kraken.com/0/private/CancelOrder'
                           params <- paste0('&txid=', txid) },
         'TradeVolume' = { url <- 'https://api.kraken.com/0/private/TradeVolume'
                           params <- paste0('&pair=', pair) },
         'CancelAllOrdersAfter' = { url <- 'https://api.kraken.com/0/private/CancelAllOrdersAfter'
                                    params <- paste0('&timeout=', timeout) },
         'OpenPositions' = { url <- 'https://api.kraken.com/0/private/OpenPositions'
                             params <- paste0('&txid=', txid, '&docalcs=', docalcs) },
         'TradesHistory' = { url <- 'https://api.kraken.com/0/private/TradesHistory'
                             params <- paste0('&asset=', asset, '&type=', type, '&start=', as.numeric(as.POSIXct(since)), '&end=', as.numeric(as.POSIXct(enddate)), '&ofs=', ofs) },
         'ClosedOrders' = { url <- 'https://api.kraken.com/0/private/ClosedOrders'
                            params <- paste0('&trades=', trades, '&start=', as.numeric(as.POSIXct(since)), '&end=', as.numeric(as.POSIXct(enddate)), '&ofs=', ofs) },
         'Ledgers'     = { url <- 'https://api.kraken.com/0/private/Ledgers'
                           params <- paste0('&asset=', asset, '&type=', type, '&start=', as.numeric(as.POSIXct(since)), '&end=', as.numeric(as.POSIXct(enddate)), '&ofs=', ofs) },
         'AddOrder'    = { url <- 'https://api.kraken.com/0/private/AddOrder'
                           params <- paste0('&pair=', pair, '&type=', type, '&ordertype=', ordertype, '&price=', price, '&volume=', volume) })

  # Call helper function
  res <- get_kraken_url(url, params)

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

  } else if (method == "Ledgers" | method == "Ledgers2") {

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
      orders[[oid]] <- data.table(order_id = oid,
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

# Testing
if (FALSE) {

  library(digest)
  library(caTools)
  library(cryptor)

  get_kraken("Ticker", pair = "DOTUSD")
  get_kraken("Trades", pair = "DOTUSD")
  get_kraken("Depth", pair = "DOTUSD")
  get_kraken("OHLC", pair = "DOTUSD")
  get_kraken("Spread", pair = "DOTUSD")
  get_kraken("AssetPairs", pair = "DOTUSD")
  get_kraken("Assets")
  get_kraken("Time")
  get_kraken("Balance")
  get_kraken("Ledgers", since = '2021-01-21', enddate = '2021-01-22', ofs = 1)
  get_kraken("CancelOrder", txid = 'OAMBZD-FWRTN-G63URX')
  get_kraken("CancelAll")
  get_kraken("CancelAllOrdersAfter")
  get_kraken("ClosedOrders")
  get_kraken("TradesHistory")
  get_kraken("TradeVolume", pair="DOTUSD,KEEPUSD")
  get_kraken("OpenPositions", txid = '')
  get_kraken("AddOrder", pair = 'DOTUSD', type = 'buy', ordertype = 'limit', price = 10,  volume = .1)

  'OpenPositions' = { url <- 'https://api.kraken.com/0/private/OpenPositions'
  params <- paste0('&txid=', txid) },


}

if (FALSE) {

  library(httr)
  url <- "https://wallet-api.staging.celsius.network/wallet/transactions?page=2&per_page=3"
  r <- GET(url, add_headers("X-Cel-Partner-Token: " ,"X-Cel-User-Token: 44a88547-1f1a-43ab-ad02-31f21446a63e"))
  stop_for_status(r)
  content(r, "parsed", "application/json")





  # url <- 'https://api.kraken.com/0/private/Ledgers'
  # param <- '&ofs=50'
  # URI_path <- gsub("^.*?kraken.com","", url)
  # nonce <- as.character(as.numeric(Sys.time()) * 1e6)
  # post_data <- paste0('nonce=', nonce, param)
  # # Calculate the SHA256 of the nonce and the POST data.
  # dig <- digest(object = paste0(nonce, post_data), algo = 'sha256', serialize = FALSE, raw = TRUE)
  # message <- c(charToRaw(URI_path), dig)
  # # Decode the API secret (the private part of the API key) from base64
  # key_decode <- base64decode(secret, what='raw')
  # # Calculate the HMAC of the URI path and the SHA256, using SHA512 as the HMAC hash and the decoded API secret as the HMAC key.
  # sign <- hmac(key = key_decode, object = message, algo = 'sha512', raw = TRUE)
  # # Encode the HMAC into base64.
  # httpheader <- c('API-Key' = key, 'API-Sign' = base64encode(sign))
  # curl_msg <- RCurl::getCurlHandle(useragent = paste("cryptor", packageVersion("cryptor")))
  # # curl
  # query_result_json <- try(RCurl::getURLContent(curl = curl_msg, url = url, postfields = post_data, httpheader = httpheader))
  # query_result_json

}
