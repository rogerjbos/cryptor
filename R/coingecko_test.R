tmp <- get_coingecko_ohlc("bitcoin", days = "max")

tmp[tmp$date >= '2018-01-02' & tmp$date <= '2018-01-08', ]
