# library(tidyr)
# library(fit.factor.models)
#
# # Make sure data.table knows we know we're using it
# .datatable.aware = TRUE
#
# end <- 9999
# cmc <- list()
# i <- 1
# for (id in 1:end) {
#   tmp <- try(fread("/Users/rogerbos/data/cmc/cmc_" %+% id %+% ".csv"), silent = TRUE)
#   if (!inherits(tmp, "try-error") && nrow(tmp) > 150) {
#     tmp$id <- id
#     cmc[[i]] <- tmp[!duplicated(tmp$timestamp), ]
#     i <- i + 1
#   }
# }
# rd <- rbindlist(cmc) %>%
#   as.data.table()
# rd[, ret := close / lag(close) - 1, by = id]
# rd[, date := as.Date(timestamp)]
#
# retMat <- pivot_wider(as.data.frame(rd[date > (endDate - 365) & date <= endDate]),
#                       id_cols=c("id"),
#                       names_from = "date",
#                       values_from = "ret",
#                       values_fill=0)
# # Remove first date because NAs
# # retMat <- retMat[, -2]
# dim(retMat)
#
# spec_risk <- list()
# for (i in 365:ncol(retMat)) {
#
#   i <- ncol(retMat)
#   r <- retMat[, c(1, (i-364):i)]
#   mdate <- colnames(retMat)[i]
#   stat <- try(fit.statistical(r))
#   if (!inherits(stat, "try-error")) {
#     spec <- data.table(date = mdate, t(stat$specific.risk))
#     names(spec) <- c("date", r$id)
#     spec_risk[[i]] <- spec
#   }
# }
# specific_risk <- rbindlist(spec_risk)
# dim(specific_risk)
#
