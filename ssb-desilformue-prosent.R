library(PxWebApiData)
library(ggplot2)
library(reshape2)

# https://www.ssb.no/inntekt-og-forbruk/statistikker/ifhus/aar
# https://www.ssb.no/statbank/table/10318
url <- "http://data.ssb.no/api/v0/no/table/10318"

mf <- ApiData(url, returnMetaFrames = TRUE)

df <- ApiData(url, 
              Tid=c("2017"),
              Desiler=TRUE,
              ContentsCode=c("Nettoformue"))

df <- df$`10318: Del av berekna nettoformue, gjennomsnittleg berekna nettoformue og lågaste verdi i desil for hushald, etter desil, statistikkvariabel og år`

df <- df[df$desil != "I alt", ]

df

ggplot(df, aes(x = reorder(desil, value),
               y = value)) +
  geom_bar(stat = "identity", width = .75, position = "dodge2") +
  coord_flip() +
  labs(title="Del av berekna nettoformue",
       subtitle="Hushaldningar gruppert etter desil. Data for 2017",
       x="Desil",
       y="Prosent",
       fill="År",
       caption = "Kjelde: SSB (tabell 10318)\nDiagram: Refsdal.Ivar@gmail.com") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Dark2")
