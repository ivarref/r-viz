library(eurostat)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)

# https://ec.europa.eu/eurostat/web/transport/data/database

date <- "2017-01-01"

pop <- get_eurostat("tps00001")
pop <- label_eurostat(pop)

pop <- rename(pop, Population.Value=values)
pop <- pop[ , !(names(pop) %in% c("indic_de"))]
pop <- pop[pop$time == date, ]

df <- get_eurostat("avia_paoc", select_time = "Y")
geo <- unique(df$geo)
df <- df[!df$geo %in% c("EU27"), ] 

df <- label_eurostat(df)

df <- df[df$tra_meas == "Passengers carried", ]
df <- df[df$tra_cov == "National transport", ]
df <- df[df$schedule == "Total", ]

df <- df[ , !(names(df) %in% c("tra_meas","tra_cov", "schedule"))]
df <- df[df$time == date, ]

wide <- inner_join(df, pop)
wide$per_capita = wide$values / (wide$Population.Value)
wide$geo = factor(wide$geo)

wide <- wide[order(-wide$per_capita),]
wide <- head(wide, 15)
ren <- function(from, to) {
  levels(wide$geo)[levels(wide$geo) == from] <<- to
}
ren("Germany (until 1990 former territory of the FRG)", "Germany")
ren("Former Yugoslav Republic of Macedonia, the", "Macedonia")
ren("European Union - 28 countries", "EU28 gj.snitt")

ren("Norway", "Noreg")
ren("Sweden", "Sverige")
ren("Greece", "Hellas")
ren("Spain", "Spania")
ren("Italy", "Italia")
ren("France", "Frankrike")
ren("United Kingdom", "Storbritannia")
ren("Denmark", "Danmark")
ren("Germany", "Tyskland")
ren("Croatia", "Kroatia")
ren("Switzerland", "Sveits")
ren("Austria", "Austerrike")

ggplot(wide, aes(x=reorder(geo, per_capita),
                 y=per_capita))+
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  ggtitle("Innanlands flytrafikk", subtitle="Antall reiser per innbyggjar, 2017") +
  labs(x="Land / Område",
       y="Antall reiser per innbyggjar per år",
       caption = "Kjelde: Eurostat (tabell avia_paoc og tps00001, 2017)")
