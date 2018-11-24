library(PxWebApiData)
library(ggplot2)
library(reshape2)

# https://www.ssb.no/statbank/table/11423/

url <- "http://data.ssb.no/api/v0/no/table/11423"
mf <- ApiData(url, returnMetaFrames = TRUE)
df <- ApiData(url, 
              Tid=c("2015", "2017"),
              Desiler=TRUE,
              Sektor=c("Sum alle sektorer"),
              ContentsCode=c("Gjennomsnittlig månedslønn (kr)"),
              Kjonn=c("Begge kjønn"))$`Lønn for ansatte, etter kjønn, desil, sektor, statistikkvariabel og år`

df <- df[, !(names(df) %in% c("kjønn", "sektor", "statistikkvariabel"))]

df$value <- (df$value*12)

ggplot(df, aes(x = reorder(desil, value),
               y = value,
               fill = år)) +
  geom_bar(stat = "identity", width = .75, position = "dodge2") +
  scale_y_continuous(breaks=c(0, 250e3, 500e3, 750e3, 1e6),
                     labels=c("0", "250 000", "500 000", "750 000", "1 mill.")) +
  coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(title="Gjennomsnittleg årslønn",
       subtitle="Etter desil. Alle sektorar, begge kjønn.",
       x="Desil",
       y="Gjennomsnittleg årslønn",
       fill="År",
       caption = "Kjelde: SSB (tabell 11423).") +
  scale_fill_brewer(palette = "Dark2")
