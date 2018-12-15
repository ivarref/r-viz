library(eurostat)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

df <- get_eurostat("ilc_di01")
# http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di01&lang=en
# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dv-vd/inc-rev/index-eng.cfm
df <- label_eurostat(df)

remove_geos <- c("European Union (before the accession of Croatia)",
                 "Euro area (12 countries)",
                 "Euro area (18 countries)",
                 "Euro area (19 countries)",
                 "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)",
                 "European Union (15 countries)",
                 "European Union (EU6-1972, EU9-1980, EU10-1985, EU12-1994, EU15-2004, EU25-2006, EU27-2013, EU28)")

df <- df[!df$geo %in% remove_geos, ] 

filter_geos <- c("France")

df <- df[df$geo %in% filter_geos, ] 

unique(df$geo)

filter_quantiles <- c("Fifth percentile",
                      "First quantile",
                      "Fourth decile",
                      "Ninety-fifth percentile",
                      "Ninety-ninth percentile")
quantiles <- unique(df$quantile)
df <- df[df$quantile %in% filter_quantiles, ] 

indic_il <- unique(df$indic_il)
df <- df[df$indic_il %in% c("Top cut-off point"), ]
df <- df[df$currency %in% c("Euro"), ]
df <- df[, !(names(df) %in% c("currency", "indic_il"))]
wide <- dcast(df, time ~ quantile, value.var="values")
wide <- wide[complete.cases(wide), ]

#for (k in filter_quantiles) {
#  print(k)
#  wide[[k]] <- 100 * wide[[k]] / wide[[k]][1]
#}

wide

df <- melt(wide, id.vars=c("time"))

ggplot(df, aes(x = time,
               colour=variable,
               y = value)) +
  geom_line(size=1.3) +
  labs(title="Frankrike. Inntekt etter desil og persentil",
       x="Tid",
       y="Euro, top cut-off point",
       caption = "Kjelde: Eurostat (tabell ilc_di01).") +
  scale_fill_brewer(palette = "Dark2")
