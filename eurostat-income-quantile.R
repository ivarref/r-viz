library(eurostat)
library(ggplot2)
library(tidyr)
library(dplyr)

df <- get_eurostat("ilc_di01")
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
                      "Ninety-fifth percentile")
quantiles <- unique(df$quantile)
df <- df[df$quantile %in% filter_quantiles, ] 

indic_il <- unique(df$indic_il)
df <- df[df$indic_il %in% c("Top cut-off point"), ]
df <- df[df$currency %in% c("Euro"), ]
df <- df[, !(names(df) %in% c("currency", "indic_il"))]
df
