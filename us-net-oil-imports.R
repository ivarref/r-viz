library(RCurl)
library(readxl)
library(ggplot2)
library(reshape2)
library(zoo)
library(lubridate)

# https://www.eia.gov/dnav/pet/PET_MOVE_NETI_A_EP00_IMN_MBBLPD_M.htm
url <- "https://www.eia.gov/dnav/pet/xls/PET_MOVE_NETI_A_EP00_IMN_MBBLPD_M.xls"
fil <- "PET_MOVE_NETI_A_EP00_IMN_MBBLPD_M.xls"
download.file(url, destfile=fil, method="libcurl")

df <- read_excel(fil, sheet="Data 1", skip=2)

colnames(df)[colnames(df)=="U.S. Net Imports of Crude Oil and Petroleum Products (Thousand Barrels per Day)"] <- "Total"

for (nam in names(df)) {
  prefix <- "U.S. Net Imports from "
  suffix <- " of Crude Oil and Petroleum Products (Thousand Barrels per Day)"
  if (startsWith(nam, prefix) & endsWith(nam, suffix)) {
    nam2 <- substr(nam, nchar(prefix)+1, nchar(nam)-(nchar(suffix)))
    colnames(df)[colnames(df)==nam] <- nam2
  } else {
    print(nam)
  }
}

for (nam in names(df)) {
  k <- 12
  if (nam != "Date") {
    df[[nam]] <- rollmean(df[[nam]], k=k, fill=NA, align = 'right')
  }
}

df <- df[df$Date >= as.POSIXct("2010-01-01 00:00:00", tz="UTC"),]

dfLatestSorted <- df[year(df$Date) == max(year(df$Date)),]
dfLatestSorted <- dfLatestSorted[month(dfLatestSorted$Date) == max(month(dfLatestSorted$Date)),]
dfLatestSorted <- melt(dfLatestSorted, id.vars=c("Date"))
dfLatestSorted <- dfLatestSorted[order(dfLatestSorted$value),]
dfLatestSorted <- dfLatestSorted[complete.cases(dfLatestSorted), ]

dfLatestSorted

df2 <- df[, names(df) %in% c("Date", 
                             "Total",
                             "OPEC Countries",
                             "Non-OPEC Countries"
                             #"Canada"        #1
                             #"Saudi Arabia", #2
                             #"Venezuela"     #3
                             #"Iraq",         #4
                             #"Russia",       #5
                             #"Nigeria",      #6
                             #"Columbia",     #7
                             #"Algeria",      #8
                             #"Kuwait",       #9
                             #"Angola"        #10
                             )]

#df2[is.na(df2)] <- 0

df3 <- melt(df2, id.vars=c("Date"))

ggplot(df3, aes(x=Date, y=value, fill=variable)) +
  labs(title="USA, netto import av petroleum og andre væsker",
       subtitle="12 månadar glidande gjennomsnitt",
       x="Tid",
       y="Tusen fat per dag",
       fill="Område",
       caption = "Kjelde: EIA.") +
  geom_area(data=df3[df3$variable != "Total", ])
  # + geom_line(data=df4[df4$variable == "Total", ])
