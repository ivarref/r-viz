library(eurostat)
library(ggplot2)
library(tidyr)
library(dplyr)

df <- get_eurostat("nrg_110a")
df <- df[(df$geo %in% c("NO", "SE", "DK", "FI", "NL")) & 
           (df$unit == "KTOE"),]
df <- label_eurostat(df)
df <- rename(df, Energy.Value=values)

pop <- get_eurostat("tps00001")
#pop <- pop[pop$geo %in% c("NO"),]
pop <- label_eurostat(pop)
pop <- rename(pop, Population.Value=values)

dfl <- df
dfl <- dfl[dfl$product %in% c("Gasoline (without bio components)", 
  "Biogasoline", 
  "Biodiesels",
  "Other liquid biofuels"),]
dfl <- dfl[dfl$indic_nrg == "Final Energy Consumption - Transport",]
dflWide <- spread(data=dfl, key=product, value=Energy.Value)

dflWide <- inner_join(dflWide, pop)

dflWide$sum = (1000*1000 * (dflWide$`Gasoline (without bio components)` + 
                            dflWide$Biogasoline + 
                            dflWide$Biodiesels + 
                            dflWide$`Other liquid biofuels`)) / dflWide$Population.Value

ggplot(dflWide,
       aes(x=time,
           y=sum,
           color=geo))+
  geom_line()
