library(PxWebApiData)
library(ggplot2)
library(reshape2)

url <- "http://data.ssb.no/api/v0/no/table/09430"
mf <- ApiData(url, returnMetaFrames = TRUE)
df <- ApiData(url, Alder="30-34",
              Tid=TRUE,
              InnvandrKat="I alt",
              Fagfelt="Fagfelt i alt",
              UtdNivaa=c("Universitets- og høgskolenivå kort", 
                         "Universitets- og høgskolenivå lang", 
                         "Videregående skole-nivå",
                         "Fagskolenivå",
                         "Grunnskolenivå"),
              ContentsCode="Personer 16 år og over (prosent)",
              Kjonn=c("Menn", "Kvinner"))

df <- df$`Personer 16 år og over, etter alder, kjønn, utdanningsnivå, fagfelt, innvandringskategori, statistikkvariabel og år`
df$dato <- as.Date(df$år, "%Y")
df <- df[ , !(names(df) %in% c("alder", "år", "innvandringskategori", "statistikkvariabel", "fagfelt"))]

wide <- dcast(df, kjønn + dato ~ utdanningsnivå, value.var="value")
wide[is.na(wide)] <- 0
wide$`Høgare utdanning` = 
  wide$Fagskolenivå + 
  wide$`Universitets- og høgskolenivå kort` + 
  wide$`Universitets- og høgskolenivå lang`

df <- melt(wide, id.vars=c("kjønn", "dato"))
df <- df[df$variable == "Høgare utdanning",]

ggplot(df,
       aes(x=dato,
           y=value,
           color=kjønn)) +
  geom_line() +
  ggtitle("Høgare utdanning etter kjønn i aldersgruppa 30-34 år",
          subtitle="Inkluderer fagskule, universitets- og høgskolenivå (kort og lang)") +
  expand_limits(y = 0) +
  labs(x="Tid",
       y="Prosent",
       caption = "Kjelde: SSB (tabell 09430).")
