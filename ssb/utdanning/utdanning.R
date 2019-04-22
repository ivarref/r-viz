library(PxWebApiData)
library(ggplot2)

url <- "http://data.ssb.no/api/v0/no/table/09430"
mf <- ApiData(url, returnMetaFrames = TRUE)

UtdNivaa <- "Universitets- og høgskolenivå lang"

df <- ApiData(url, Alder="30-34",
              Tid=TRUE,
              InnvandrKat="I alt",
              Fagfelt="Fagfelt i alt",
              UtdNivaa=UtdNivaa,
              ContentsCode="Personer 16 år og over (prosent)",
              Kjonn=c("Menn", "Kvinner"))

df <- df$`Personer 16 år og over, etter alder, kjønn, utdanningsnivå, fagfelt, innvandringskategori, statistikkvariabel og år`
df <- df[ , !(names(df) %in% c("utdanningsnivå"))]
df$dato <- as.Date(df$år, "%Y")

ggplot(df,
       aes(x=dato,
           y=value,
           color=kjønn)) +
  geom_line() +
  ggtitle("Utdanningsnivå etter kjønn", subtitle="Universitets- og høgskolenivå lang") +
  expand_limits(y = 0) +
  labs(x="Tid",
       y="Prosent",
       caption = "Kjelde: SSB (tabell 09430)")
