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

#' Get prices from the CoinMarketCap web api using either Id or Symbol (Id prefered)
#' https://www.coingecko.com/en/api/documentation
#'
#' @name get_coingecko_price
#' @title get_coingecko_price
#' @encoding UTF-8
#' @concept Get prices from the CoinGecko web api
#' @param id string id or string symbol
#'
#' @return price
#'
#' @examples
#' get_coingecko_price("cardano")
#' get_coingecko_price("bitcoin")
#' get_coingecko_price("btc")
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_coingecko_price <- function(id = NULL) {

  # id <- 'basic-attention-token'
  if (is.null(id)) stop("Error: a [character] `id` must be specified.")
  if (!get_coingecko_verify_id(id)) {
    id <- get_coingecko_id_from_symbol(id)$Id
  }
  if (length(id) != 1) {
    stop("Error: more than one possible match for given symbol:")
    print(id)
  }

  url <- paste0("https://api.coingecko.com/api/v3/simple/price?ids=", id, "&vs_currencies=usd")
  j <- jsonlite::read_json(url)
  # return the latest price
  unlist(j[1])

}

# To update map!
# .map <<- fread("data-raw/gecko.csv")
# usethis::use_data(.map, internal = TRUE, overwrite = TRUE)
