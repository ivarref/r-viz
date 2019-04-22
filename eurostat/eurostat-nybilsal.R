library(eurostat)
library(tidyverse)
library(ggplot2)

# https://ec.europa.eu/eurostat/statistics-explained/index.php/Passenger_cars_in_the_EU#Overview
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=road_eqr_carmot&lang=en

df <- get_eurostat("road_eqr_carmot", select_time = "Y")
maxtime <- max(df$time)
df <- label_eurostat(df)

df <- df[df$time == maxtime, ]
df <- df[df$engine == "Total", ]
df <- df[df$mot_nrg == "Total", ]
df

date <- "2017-01-01"
pop <- get_eurostat("tps00001")
pop <- label_eurostat(pop)
pop <- pop[pop$time == date, ]
pop

wide <- inner_join(df, pop, by=c("geo", "time"))
wide$value <- 1000*wide$values.x / wide$values.y
wide <- wide %>%
  select(geo, value)

ren <- function(from, to) {
  wide$geo[wide$geo == from] <<- to
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

wide$readable <- as.integer(wide$value)

ggplot(wide, aes(x=reorder(geo, value),
                 label=readable,
                 y=value))+
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(size=3, position=position_dodge(width=0.9), hjust=-0.15) +
  coord_flip() +
  ggtitle("Nybilsalget", subtitle="Antall nye personbilar per 1000 innbyggjar, 2017") +
  labs(x="Land / Område",
       y="Antall nye personbilar per 1000 innbyggjar",
       caption = "Kjelde: Eurostat (tabell road_eqr_carmot og tps00001, 2017)")
