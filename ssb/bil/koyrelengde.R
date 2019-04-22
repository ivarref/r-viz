library(PxWebApiData)
library(ggplot2)
library(reshape2)

# https://www.ssb.no/statbank/table/07311

url <- "http://data.ssb.no/api/v0/no/table/07311"
mf <- ApiData(url, returnMetaFrames = TRUE)
