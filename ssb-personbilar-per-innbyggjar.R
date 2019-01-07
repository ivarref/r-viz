library(PxWebApiData)
library(ggplot2)
library(reshape2)
library(dplyr)

# Registrerte kjøretøy, etter kjøretøygruppe 1950 - 2017
# https://www.ssb.no/statbank/table/01960

# Endringer i befolkningen i løpet av året 1735 - 2018
# https://www.ssb.no/statbank/table/05803

pop <- ApiData("http://data.ssb.no/api/v0/no/table/05803",
               Tid=TRUE,
               ContentsCode=c("Befolkning"))
pop <- pop$`05803: Endringer i befolkningen i løpet av året, etter statistikkvariabel og år`
pop <- pop[pop$år >= 1950, ]
pop <- pop[, !names(pop) %in% c("statistikkvariabel")]
colnames(pop)[colnames(pop)=="value"] <- "befolkning"

df <- ApiData("http://data.ssb.no/api/v0/no/table/01960",
              Tid=TRUE,
              ContentsCode=c("Personbiler", 
                             "Busser",
                             "Lastebiler",
                             "Varebiler"
                             ))
df <- df$`01960: Registrerte kjøretøy, etter statistikkvariabel og år`
df <- inner_join(df, pop)

df$per1000Innb <- df$value / (df$befolkning/1000)

df$statistikkvariabel[df$statistikkvariabel == "Busser"] <- "Bussar"
df$statistikkvariabel[df$statistikkvariabel == "Lastebiler"] <- "Lastebilar"
df$statistikkvariabel[df$statistikkvariabel == "Personbiler"] <- "Personbilar"
df$statistikkvariabel[df$statistikkvariabel == "Varebiler"] <- "Varebilar"

brks <- c("Personbilar", "Varebilar", "Lastebilar", "Bussar")

# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(4)

ggplot(df,
       aes(x=år,
           y=per1000Innb,
           group=statistikkvariabel,
           color=statistikkvariabel))+
  geom_line(size=1.2) +
  scale_x_discrete(breaks = seq(1950, 2017, 10)) +
  scale_color_manual(breaks = brks, labels = brks, values = cols) +
  labs(title="Antall køyretøy per 1000 innbyggjar",
       subtitle="Noreg, 1950-2017",
       x="År",
       color="Køyretøytype",
       y="Antall køyretøy per 1000 innbyggjar",
       caption = "Diagram: Refsdal.Ivar@gmail.com\nKjelde: SSB")
  