library(PxWebApiData)
library(ggplot2)
library(reshape2)
library(dplyr)

# https://www.ssb.no/inntekt-og-forbruk/statistikker/ifhus/aar
# https://www.ssb.no/statbank/table/10318

url <- "http://data.ssb.no/api/v0/no/table/10318"
mf <- ApiData(url, returnMetaFrames = TRUE)
dforg <- ApiData(url, 
                 Tid=TRUE,
                 Desiler=TRUE,
                 ContentsCode=c("BereknFormue"))

df <- dforg$`10318: Del av berekna nettoformue, gjennomsnittleg berekna nettoformue og lågaste verdi i desil for hushald, etter desil, statistikkvariabel og år`
df <- df[, !(names(df) %in% c("statistikkvariabel"))]

df <- df[df$desil %in% c("Desil 1",
                         "Desil 2",
                         "Desil 3",
                         "Desil 4",
                         "Desil 5",
                         "Desil 6",
                         "Desil 7",
                         "Desil 8",
                         "Desil 9",
                         "Desil 10",
                         "Høgaste 0,1 prosent"),]
comb <- NULL
for (desil in unique(df$desil)) {
  subset <- df[df$desil == desil,]
  subset$value <- 100 * subset$value / subset$value[1]
  comb <- bind_rows(comb, subset)
}
df <- comb

ggplot(df, aes(x=år,
               y=value,
               color=desil, 
               group=desil))+
  geom_line()

