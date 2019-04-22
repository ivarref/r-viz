library(eurostat)
library(tidyverse)
library(ggplot2)

# https://ec.europa.eu/eurostat/statistics-explained/index.php/Passenger_cars_in_the_EU#Overview
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=road_eqs_carhab&lang=en

df <- get_eurostat("road_eqs_carhab", select_time = "Y")
maxtime <- max(df$time)
df <- label_eurostat(df)

df <- df[df$time == maxtime, ]

ren <- function(from, to) {
  levels(df$geo)[levels(df$geo) == from] <<- to
}
ren("Germany (until 1990 former territory of the FRG)", "Germany")
ren("North Macedonia", "Macedonia")
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
ren("Cyprus", "Kypros")
ren("Poland", "Polen")
ren("Estonia", "Estland")
ren("Belgium", "Belgia")
ren("Czechia", "Tsjekkia")
ren("Netherlands", "Nederland")
ren("Lithuania", "Litauen")
ren("Ireland", "Irland")
ren("Hungary", "Ungarn")
ren("Macedonia", "N. Makedonia")
ren("Turkey", "Tyrkia")


ggplot(df, aes(x=reorder(geo, values),
               label=values,
               y=values))+
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(size=3, position=position_dodge(width=0.9), hjust=-0.15) +
  coord_flip() +
  ggtitle("Personbilar per innbyggjar",
          subtitle="Antall per 1000 innbyggjar, 2016") +
  labs(x="Land",
       y="Antall personbilar per 1000 innbyggjar",
       caption = "Kjelde: Eurostat (tabell road_eqs_carhab, 2016)")
