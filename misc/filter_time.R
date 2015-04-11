library(timeDate)
library(lubridate)

#For cleaning up SHFE data. Filter on times.

df.one<-read.csv(file="data/SHFE_au.csv", header=TRUE, sep=",")
df.two<-read.csv(file="data/SHFE_ag.csv", header=TRUE, sep=",")
df.x<-merge(df.one, df.two, by="datetime")

df.x<-read.csv(file="data/SHFE_auag.csv", header=TRUE, sep=",")

df.datetime<-data.frame(as.POSIXlt(as.character(df.x[,1])))

hour(df.datetime[2])
minute(df.datetime[2])

df.datetime$hour<-hour(df.datetime[,1])
df.datetime$min<-minute(df.datetime[,1])


df.datetime[1:100,]

df.datetime$int<-0

colnames(df.datetime)[1] <- "datetime"
df.datetime$int[(df.datetime$hour %in% c(9) & df.datetime$min>=5)]<-1
df.datetime$int[(df.datetime$hour %in% c(10) & df.datetime$min %in% c(0,5,10,15))]<-1
df.datetime$int[(df.datetime$hour %in% c(10) & df.datetime$min>=35)]<-1
df.datetime$int[(df.datetime$hour %in% c(11) & df.datetime$min<=30)]<-1
df.datetime$int[(df.datetime$hour %in% c(13) & df.datetime$min>=35)]<-1
df.datetime$int[(df.datetime$hour %in% c(14))]<-1
df.datetime$int[(df.datetime$hour %in% c(15) & df.datetime$min==0)]<-1
df.datetime$int[(df.datetime$hour %in% c(21,22,23,24,1))]<-1
df.datetime$int[(df.datetime$hour %in% c(2) & df.datetime$min<=30)]<-1

df.x$filter<-df.datetime$int

df.all<-df.x[df.x$filter==1,c(1:11)]
df.au<-df.x[df.x$filter==1,c(1:6)]
df.ag<-df.x[df.x$filter==1,c(1,7:11)]
write.table(df.au, "data/SHFE_au_clean.csv", row.names=F, sep=",")

write.table(df.ag, "data/SHFE_ag_clean.csv", row.names=F, sep=",")

write.table(df.all, "data/SHFE_auag_clean.csv", row.names=F, sep=",")
