library(eurostat)
library(ggplot2)

df <- get_eurostat("tessi250")
geo <- unique(df$geo)
df <- df[df$geo %in% c("ES", "DE", "NO"), ] 
df <- label_eurostat(df)
levels(df$geo)[levels(df$geo)=="Germany (until 1990 former territory of the FRG)"] <- "Germany"

df <- df[df$worktime %in% c("Part-time"), ]

ggplot(df,
       aes(x=time,
           y=values,
           color=geo))+
  geom_line()
