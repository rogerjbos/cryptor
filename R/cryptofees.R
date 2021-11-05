#' Get crypto fees paid from the Cryptofees.info web api
#' https://cryptofees.info/api-docs
#'
#' @name get_cyrptofees_fees
#' @title get_cyrptofees_fees
#' @encoding UTF-8
#' @concept Get crypto fees paid from the Cryptofees.info web api
#' @param None
#'
#' @return list of protocols, fees, and events
#'
#' @examples
#' tmp <- get_cyrptofees_fees()
#'
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
get_cyrptofees_fees <- function() {

  j <- jsonlite::read_json(path="https://cryptofees.info/api/v1/fees", simplifyVector = TRUE)

  if (j$success) {
    protocols <- j$protocols[, c("id","name","tokenTicker","tokenCoingecko","website","blockchain","source","adapter",
                                 "category","bundle","tokenLaunch","protocolLaunch","subtitle")]
    events <- j$protocols$events
    # fees
    f <- j$protocols$fees
    fees <- data.frame(protocols$id, protocols$name, protocols$tokenCoingecko, t(sapply(f, '[[',2)))
    names(fees) <- c("id", "name", "tokenCoingecko", f[[1]]$date)
    return(list(protocols = protocols, fees = fees, events = events))
  }

}



