library(eurostat)
library(ggplot2)
library(tidyr)
library(dplyr)

df <- get_eurostat("tesov190")
geo <- unique(df$geo)
df <- df[df$geo %in% c("NO", "SE", "PT", "DK", "IE", "TR", "FI", "UK", "EL", "DE", "MK", "EU28", "IT", "ES"), ] 
df <- label_eurostat(df)

levels(df$geo)[levels(df$geo)=="Germany (until 1990 former territory of the FRG)"] <- "Germany"
levels(df$geo)[levels(df$geo)=="Former Yugoslav Republic of Macedonia, the"] <- "Macedonia"
levels(df$geo)[levels(df$geo)=="European Union (current composition)"] <- "EU28"
levels(df$geo)[levels(df$geo)=="United Kingdom"] <- "UK"

geomap = data.frame(short=geo, long=levels(df$geo))
df <- df[df$time == "2016-01-01", ] 
df <- df[df$hhtyp %in% c("Single person"), ] 

ggplot(df, aes(x=reorder(geo, -values),
               y=values))+
  geom_bar(stat="identity") +
  labs(x="Land / Område", y="Prosent") + 
  ggtitle("Aleinebuande", subtitle="Del av folkesetnaden")
