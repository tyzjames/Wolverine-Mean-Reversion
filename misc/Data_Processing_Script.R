library(quantmod)
source("misc/data_processing_func.R")

#Read data
path="D:/Trading/Ernie/GC/"
fileList.ask<-list.files(path=path, pattern="(Ask)")
fileList.bid<-list.files(path=path, pattern="(Bid)")

df.gc.ask<-readDataFromFolder(fileList.ask, path)
df.gc.bid<-readDataFromFolder(fileList.bid, path)
df.gc.ask<-cleanMarketData(df.gc.ask)
df.gc.bid<-cleanMarketData(df.gc.bid)

df.gc.merge<-merge(df.gc.ask, df.gc.bid, by="datetime")

#Get Midpoint
df.gc<-data.frame(datetime=df.gc.merge$datetime, open=((df.gc.merge$open.x+df.gc.merge$open.y)/2), high=((df.gc.merge$high.x+df.gc.merge$high.y)/2),
                  low=((df.gc.merge$low.x+df.gc.merge$low.y)/2), close=((df.gc.merge$close.x+df.gc.merge$close.y)/2))

path="D:/Trading/Ernie/SI/"
# fileList<-list.files(path=path, pattern="FUT_M.csv")
fileList.ask<-list.files(path=path, pattern="(Ask)")
fileList.bid<-list.files(path=path, pattern="(Bid)")

df.si.ask<-readDataFromFolder(fileList.ask, path)
df.si.bid<-readDataFromFolder(fileList.bid, path)
df.si.ask<-cleanMarketData(df.si.ask)
df.si.bid<-cleanMarketData(df.si.bid)

df.si.merge<-merge(df.si.ask, df.si.bid, by="datetime")

#Get Midpoint
df.si<-data.frame(datetime=df.si.merge$datetime, open=((df.si.merge$open.x+df.si.merge$open.y)/2), high=((df.si.merge$high.x+df.si.merge$high.y)/2),
                  low=((df.si.merge$low.x+df.si.merge$low.y)/2), close=((df.si.merge$close.x+df.si.merge$close.y)/2))



write.table(df.gc, file="Gold 1min.csv", sep=",", row.names=F, col.names=T)
write.table(df.si, file="Silver 1min.csv", sep=",", row.names=F, col.names=T)

# #Upload data into a new table in the server
myconn <-dbConnect(MySQL(), user="tyzjames", password="System34", host="acensus.cnneqcdmtwtz.ap-southeast-1.rds.amazonaws.com", dbname="acensusDB")
dbWriteTable(conn=myconn, name='GC_1v4', value=as.data.frame(df.gc.clean))
dbWriteTable(conn=myconn, name='SI_1v4', value=as.data.frame(df.si.clean))

is.OHLC(df.si)
