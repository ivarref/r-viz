library(PxWebApiData)
library(ggplot2)
library(reshape2)

# Registrerte kjøretøy, etter kjøretøygruppe 1950 - 2017
# https://www.ssb.no/statbank/table/01960

# Endringer i befolkningen i løpet av året 1735 - 2018
# https://www.ssb.no/statbank/table/05803

# url <- "http://data.ssb.no/api/v0/no/table/01960"
pop <- ApiData("http://data.ssb.no/api/v0/no/table/05803",
               Tid=TRUE,
               ContentsCode=c("Befolkning"))
pop <- pop$`05803: Endringer i befolkningen i løpet av året, etter statistikkvariabel og år`
pop <- pop[pop$år >= 1950, ]
pop <- pop[, !names(pop) %in% c("statistikkvariabel")]
colnames(pop)[colnames(pop)=="value"] <- "befolkning"
pop
