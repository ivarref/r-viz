library(PxWebApiData)
library(ggplot2)
library(reshape2)

# https://www.ssb.no/statbank/table/11423/

url <- "http://data.ssb.no/api/v0/no/table/11423"
mf <- ApiData(url, returnMetaFrames = TRUE)
df <- ApiData(url, 
              Tid=c("2016", "2017"),
              Desiler=TRUE,
              Sektor=c("Sum alle sektorer"),
              ContentsCode=c("Gjennomsnittlig månedslønn (kr)"),
              Kjonn=c("Begge kjønn"))$`Lønn for ansatte, etter kjønn, desil, sektor, statistikkvariabel og år`

df <- df[, !(names(df) %in% c("kjønn", "sektor", "statistikkvariabel"))]

df$value <- (df$value*12)

wide <- dcast(df, desil ~ år, value.var="value")
wide$endring <- (wide$`2017` - wide$`2016`)

wide$value <- wide$`2017`

fmt <- function(n) {
  if (n>=1e6) {
    return(sprintf("%s.", gsub("\\.", ",", sprintf("%.1f mill", n/1e6))))
  }
  return(format(n, digits=9, decimal.mark=",",
                big.mark=" ",small.mark=".", , small.interval=3))
}
wide$endring = mapply(fmt, wide$endring)

wide$hack <- 1300e3

lim <- c(0, 1350e3)
breaks <- c(0, 250e3, 500e3, 750e3, 1e6, 1.25e6)
labels <- c("0", "250K", "500K", "750K", "1M", "1,25M")

ggplot(wide, 
       aes(x = reorder(desil, value),
           label=endring,
           y = value)) +
  geom_bar(stat = "identity", width = .65, position = "dodge2") +
  scale_y_continuous(limits = lim, breaks=breaks, labels=labels) +
  coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  geom_text(aes(y = hack), color="red", size=3, hjust="right") +
  labs(title="Gjennomsnittleg årslønn",
       subtitle="Etter desil. Alle sektorar, begge kjønn.",
       subsubtitle="mjau",
       x="Desil",
       y="Gjennomsnittleg årslønn",
       fill="År",
       caption = "Kjelde: SSB (tabell 11423).") +
  scale_fill_brewer(palette = "Dark2")
