library(PxWebApiData)
library(ggplot2)
library(reshape2)

url <- "http://data.ssb.no/api/v0/no/table/09430"
mf <- ApiData(url, returnMetaFrames = TRUE)
df <- ApiData(url, Alder="30-34",
              Tid=TRUE,
              InnvandrKat="I alt",
              Fagfelt="Fagfelt i alt",
              UtdNivaa=c("Videregående skole-nivå",
                         "Grunnskolenivå",
                         "Uoppgitt eller ingen fullført utdanning"),
              ContentsCode="Personer 16 år og over (prosent)",
              Kjonn=c("Menn", "Kvinner"))

df <- df$`Personer 16 år og over, etter alder, kjønn, utdanningsnivå, fagfelt, innvandringskategori, statistikkvariabel og år`
df$dato <- as.Date(df$år, "%Y")
df <- df[ , !(names(df) %in% c("alder", "år", "innvandringskategori", "statistikkvariabel", "fagfelt"))]

wide <- dcast(df, kjønn + dato ~ utdanningsnivå, value.var="value")
wide[is.na(wide)] <- 0
wide$`Lav utdanning` = 
  wide$`Videregående skole-nivå` + 
  wide$Grunnskolenivå +
  wide$`Uoppgitt eller ingen fullført utdanning`
  
df <- melt(wide, id.vars=c("kjønn", "dato"))
df <- df[df$variable == "Lav utdanning",]

ggplot(df,
       aes(x=dato,
           y=value,
           color=kjønn)) +
  geom_line() +
  ggtitle("'Låg' utdanning etter kjønn i aldersgruppa 30-34 år",
          subtitle="Vidaregåande skule som høgste fullførte utdanning") +
  expand_limits(y = 0) +
  labs(x="Tid",
       y="Prosent",
       caption = "Kjelde: SSB (tabell 09430).")
