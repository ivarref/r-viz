library(PxWebApiData)
library(ggplot2)
library(reshape2)

# https://www.ssb.no/inntekt-og-forbruk/statistikker/ifhus/aar
# https://www.ssb.no/statbank/table/10318

mf <- ApiData(url, returnMetaFrames = TRUE)

url <- "http://data.ssb.no/api/v0/no/table/10318"
df <- ApiData(url, 
              Tid=c("2017"),
              Desiler=TRUE,
              ContentsCode=c("BereknFormue"))

df

df <- df$`10318: Del av berekna nettoformue, gjennomsnittleg berekna nettoformue og lågaste verdi i desil for hushald, etter desil, statistikkvariabel og år`

df <- df[, !(names(df) %in% c("statistikkvariabel"))]

fmt <- function(n) {
  if (n>=1e6) {
    return(sprintf("%s.", gsub("\\.", ",", sprintf("%.1f mill", n/1e6))))
  }
  return(format(n, digits=9, decimal.mark=",",
                big.mark=" ",small.mark=".", , small.interval=3))
}

df$vMill = df$value / 1e6
df$vReadable = mapply(fmt, df$value)

ggplot(df, aes(x = reorder(desil, value),
               y = vMill,
               label = vReadable)) +
  expand_limits(y = 325) +
  geom_bar(stat = "identity", width = .75, position = "dodge2") +
  geom_text(color="black", size=3, position=position_dodge(width=0.9), hjust=-0.15) +
  coord_flip() +
  labs(title="Gjennomsnittleg berekna nettoformue, 2017",
       subtitle="Hushaldningar gruppert etter desil",
       x="Desil",
       y="Millionar kroner",
       fill="År",
       caption = "Kjelde: SSB (tabell 10318)\nDiagram: Refsdal.Ivar@gmail.com") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Dark2")
