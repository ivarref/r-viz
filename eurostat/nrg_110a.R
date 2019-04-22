library(eurostat)
library(ggplot2)
library(tidyr)

df <- get_eurostat("nrg_110a")
pop <- get_eurostat("tps00001")
geo = sort(unique(df$geo))
df <- df[(df$geo %in% c("NO", "SE", "DK", "FI")) & 
         (df$unit == "KTOE"),]
df <- df[order(df$time),]

dfl <- label_eurostat(df)

products = sort(unique(dfl$product))
indicators = sort(unique(dfl$indic_nrg))

dfl <- dfl[dfl$product %in% c(#"Gasoline (without bio components)", 
                              "Biogasoline", 
                              "Biodiesels",
                              "Other liquid biofuels"),]

dfl <- dfl[dfl$indic_nrg == "Final Energy Consumption - Transport",]
dflWide <- spread(data=dfl, key=product, value=values)

dflWide$values = #dflWide$`Gasoline (without bio components)` + 
                 dflWide$Biogasoline + dflWide$Biodiesels + dflWide$`Other liquid biofuels`

ggplot(dflWide,
       aes(x=time,
           y=values,
           color=geo))+
  geom_line()
