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

ggplot(df, aes(x = reorder(desil, value),
               y = value,
               fill = år)) +
  geom_bar(stat = "identity", width = .75, position = "dodge2") +
  coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(title="Gjennomsnittleg månadslønn",
       subtitle="Etter desil. Alle sektorar, begge kjønn.",
       x="Desil",
       y="Gjennomsnittleg månadslønn",
       fill="År",
       caption = "Kjelde: SSB (tabell 11423).") +
  scale_fill_brewer(palette = "Dark2")
