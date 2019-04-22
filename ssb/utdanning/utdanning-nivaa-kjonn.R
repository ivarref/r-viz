library(PxWebApiData)
library(ggplot2)
library(reshape2)

# https://www.ssb.no/statbank/table/09430/

url <- "http://data.ssb.no/api/v0/no/table/09430"
mf <- ApiData(url, returnMetaFrames = TRUE)
df <- ApiData(url, Alder="30-34",
              Tid=c("1980", "2000", "2017"),
              InnvandrKat="I alt",
              Fagfelt=TRUE,
              UtdNivaa=c("Universitets- og høgskolenivå kort",
                         "Universitets- og høgskolenivå lang"),
              ContentsCode=c("Personer 16 år og over"),
              Kjonn=c("Menn", "Kvinner"))$`Personer 16 år og over, etter alder, kjønn, utdanningsnivå, fagfelt, innvandringskategori, statistikkvariabel og år`

df$dato <- df$år
df <- df[, !(names(df) %in% c("alder", "år", "innvandringskategori", "statistikkvariabel"))]
df <- df[df$fagfelt != "Fagfelt i alt", ]
df <- df[df$fagfelt != "Allmenne fag", ]
df <- df[df$fagfelt != "Uoppgitt fagfelt", ]

wide <- dcast(df, kjønn + dato + fagfelt ~ utdanningsnivå, value.var="value")
wide[is.na(wide)] <- 0
wide$`Høgare utdanning` = 
  wide$`Universitets- og høgskolenivå kort` + 
  wide$`Universitets- og høgskolenivå lang`

df <- melt(wide, id.vars=c("kjønn", "dato", "fagfelt"))
df <- df[df$variable == "Høgare utdanning",]
df <- df[, !(names(df) %in% c("variable"))]

wide <- dcast(df, dato + fagfelt ~ kjønn, value.var="value")
wide$kvinneAndel = (100*wide$Kvinner) / (wide$Kvinner + wide$Menn)
wide$kvinneAndel2017 =((100*wide$Kvinner) / (wide$Kvinner + wide$Menn)) * ifelse(wide$dato=="2017", 1, 0)

df <- wide
df <- df[order(df$kvinneAndel2017),]

ggplot(df, aes(x = reorder(fagfelt, kvinneAndel2017),
               y = kvinneAndel,
               fill = dato)) +
  geom_bar(stat = "identity", width = .75, position = "dodge2") +
  scale_y_continuous(limits = c(0, 100)) +
  coord_flip() +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(title="Kvinneandel i ulike fagfelt",
       subtitle="Høgare utdanning, aldersgruppa 30-34 år",
       x="Fagfelt",
       y="Prosent",
       fill="År",
       caption = "Kjelde: SSB (tabell 09430).") +
  scale_fill_brewer(palette = "Dark2")