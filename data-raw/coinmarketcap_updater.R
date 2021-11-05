library(cryptor)
cmc_path <- "/Users/rogerbos/data/cmc/"

# get_cmc_price <- function(id = NULL, time_start = '2018-01-01', time_end = Sys.Date()) {
#   
#   if (is.null(id)) stop("Error: Either a [numeric] `id` or [character] `symbol` must be specified.")
#   if (!is.numeric(id)) id <- get_cmc_id(toupper(id))
#   if (length(id) > 1) stop("Error: More than one id found for symbol.  Please use numberic id instead.")
#   
#   url <- paste0("https://web-api.coinmarketcap.com/v1/cryptocurrency/ohlcv/historical?id=", id, "&convert=USD&time_start='", time_start, "'&time_end='", time_end, "'")
#   j <- jsonlite::read_json(url)
#   rawdata <- rbindlist(j$data$quotes)
#   data <- rbindlist(rawdata$quote)
#   data[, symbol := j$data$symbol]
#   data[, name := j$data$name]
#   data[, .(symbol, timestamp, open, high, low, close, volume, market_cap, name)]
#   
# }

btc <- fread(cmc_path %+% "cmc_1.csv")
next_time_start <- as.Date(max(btc$timestamp) + 1)

# get_cmc_price(1)

for (id in 1:8310) {
  tmp <- try(get_cmc_price(id, time_start = '2020-12-31'))
  if (!inherits(tmp, "try-error")) {
    print(id)
    fname = cmc_path %+% "cmc_" %+% id %+% ".csv"
    file.test <- file.exists(fname)
    fwrite(tmp[timestamp >= next_time_start], file = cmc_path %+% "cmc_" %+% id %+% ".csv", col.names = !file.test, append = file.test)
    Sys.sleep(3)
  }
}

# update the mapping object
map <- list()
for (id in 1:9109) {
  fname = cmc_path %+% "cmc_" %+% id %+% ".csv"
  if (file.exists(fname)) {
    tmp <- fread(file = cmc_path %+% "cmc_" %+% id %+% ".csv", nrows = 1)
    tmp[, id := id]
    map[[id]] <- tmp[, .(id, symbol, name)]
  }
}
.map <- rbindlist(map)
fwrite(.map, file = cmc_path %+% "map.csv", col.names = TRUE, append = FALSE)
# save(.map, file='/Users/rogerbos/R_HOME/cryptor/R/sysdata.rda')
# save(map, file = "/Users/rogerbos/R_HOME/cryptor/data/map.RData")
usethis::use_data(.map, internal = TRUE, overwrite = TRUE)

if (FALSE) {
  
  for (id in 8310:9109) {
    tmp <- try(get_cmc_price(id, time_start = '2013-04-29', time_end = '2021-04-03'))
    if (!inherits(tmp, "try-error")) {
      print(id)
      fwrite(tmp, file = cmc_path %+% "cmc_" %+% id %+% ".csv", col.names = TRUE, append = FALSE)
      Sys.sleep(3)
    }
  }

  # for (id in 8310:9109) {
  #   oldfile = cmc_path %+% "cmc_hist_" %+% id %+% ".csv"
  #   newfile = cmc_path %+% "cmc_" %+% id %+% ".csv"
  #   file.rename(from=oldfile, to=newfile)
  # }
  
}