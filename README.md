#### cryptor package
R package to download crypto prices into R from the [CoinMarketCap](https://coinmarketcap.com/) web api

#### Install the cryptor package from GitHub:
```
# Installing via Github
devtools::install_github("rogerjbos/cryptor")
```

#### Load necessary packages
```
library(cryptor)
library(data.table)
```

#### Download prices based on cryptocurrency symbol
```
get_cmc_price("ada")
#       symbol                timestamp      open      high       low     close     volume  market_cap    name
#    1:    ADA 2018-01-02T23:59:59.999Z 0.7246760 0.7946460 0.6978560 0.7825870  289712000 20290188351 Cardano
#    2:    ADA 2018-01-03T23:59:59.999Z 0.7796810 1.0856700 0.7785780 1.0796601  657398016 27992420977 Cardano
#    3:    ADA 2018-01-04T23:59:59.999Z 1.0940300 1.3272099 1.0376500 1.1141200  593430016 28885867828 Cardano
#    4:    ADA 2018-01-05T23:59:59.999Z 1.1711500 1.2524199 0.9035030 0.9995590  508100000 25915636700 Cardano
#    5:    ADA 2018-01-06T23:59:59.999Z 0.9970000 1.0540100 0.9291830 1.0271500  297615008 26630990503 Cardano
#   ---                                                                                                       
# 1115:    ADA 2021-01-20T23:59:59.999Z 0.3689362 0.3761975 0.3319584 0.3753939 3281600988 11679436460 Cardano
# 1116:    ADA 2021-01-21T23:59:59.999Z 0.3754491 0.3787164 0.3014011 0.3102087 3567902823  9651363869 Cardano
# 1117:    ADA 2021-01-22T23:59:59.999Z 0.3100538 0.3591592 0.2835762 0.3495765 4066222238 10876193809 Cardano
# 1118:    ADA 2021-01-23T23:59:59.999Z 0.3493316 0.3572110 0.3367005 0.3456435 2781033956 10753827794 Cardano
# 1119:    ADA 2021-01-24T23:59:59.999Z 0.3455862 0.3686394 0.3387557 0.3538810 2539663711 11010118427 Cardano
```

If you happen to know the CoinMarketCap id, which is there unique valued identifier, you can use it as well:
```
get_cmc_price(4172) # id for Terra LUNA
#      symbol                timestamp      open      high       low     close   volume market_cap  name
#   1:   LUNA 2019-07-27T23:59:59.999Z 1.3274362 1.4028976 1.2219383 1.3105663  6033446          0 Terra
#   2:   LUNA 2019-07-28T23:59:59.999Z 1.3105663 1.3685945 1.2368579 1.2729889  1643709          0 Terra
#   3:   LUNA 2019-07-29T23:59:59.999Z 1.2622703 1.3759410 1.1754435 1.2946611  3966802          0 Terra
#   4:   LUNA 2019-07-30T23:59:59.999Z 1.2944352 1.3743331 1.2664844 1.2941931  1820705          0 Terra
#   5:   LUNA 2019-07-31T23:59:59.999Z 1.2853826 1.3284078 1.2652665 1.3001020  1155167          0 Terra
#  ---                                                                                                  
# 544:   LUNA 2021-01-20T23:59:59.999Z 0.9101775 0.9387266 0.8674542 0.9046737 24589191  438600958 Terra
# 545:   LUNA 2021-01-21T23:59:59.999Z 0.9047443 0.9064415 0.7957022 0.7962349 24484681  386003269 Terra
# 546:   LUNA 2021-01-22T23:59:59.999Z 0.7957680 0.8637399 0.7711625 0.8132274 27699537  394225761 Terra
# 547:   LUNA 2021-01-23T23:59:59.999Z 0.8132695 0.9007082 0.8062333 0.9007082 22818246  436629120 Terra
```

#### Looking up CoinMarketCap id's

You can look up the id, but with over 8,000 cryptocurrencies in existance, the symbols are sometimes re-used or not unique.  There is a function to return the CoinMarketCap id for a given symbol, but sometimes more than one id is returned.
```
get_cmc_id("luna")
# 1496 4172
```

Two ids show up for `luna`.  Which one do we really want?  If you get more than one id returns, then you have to search by name.
```
get_cmc_name("luna")
#      id symbol       name
# 1: 1496   LUNA  Luna Coin
# 2: 3019   LSTR Luna Stars
```

If we want Terra LUNA we actually don't want either of those, so lets search for `Terra` instead and see what we get.
```
get_cmc_name("terra")
#      id symbol                name
# 1:    4    TRC           Terracoin
# 2: 1809    TER           TerraNova
# 3: 4165 CREDIT         TerraCredit
# 4: 4172   LUNA               Terra
# 5: 5115    KRT            TerraKRW
# 6: 6370    SDT           Terra SDT
# 7: 7129    UST            TerraUSD
# 8: 8037    TVK Terra Virtua Kolect
```

Who knew `Terra` was such a popular crypto name.  For `Terra LUNA` we want id `4172`.

The mapping table with the CoinMarketCap id's will need to be updated periodically, so if you are looking for a brand new crypto, you may have to go to their website to find the necessary id, otherwise trying updating the package.

That's all the package does for now.  I will try to add more functionality later as need cases come up.  I hope this helps you.  Please star the github page if you find this package useful.
