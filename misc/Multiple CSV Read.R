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

# df.gc.clean<-cleanMarketData(df.gc)
# df.gc.clean<-df.gc.clean[order(df.gc.clean$datetime),]

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

# #Upload data into a new table in the server
myconn <-dbConnect(MySQL(), user="tyzjames", password="System34", host="acensus.cnneqcdmtwtz.ap-southeast-1.rds.amazonaws.com", dbname="acensusDB")
dbWriteTable(conn=myconn, name='GC_1v3', value=as.data.frame(df.gc.clean))
dbWriteTable(conn=myconn, name='SI_1v3', value=as.data.frame(df.si.clean))

#Read all files from a folder into a data frame but excluding overlap period. Takes data up till 6 days before the contract month starts.
readDataFromFolder<- function (fileNames, path) {
  #   fileNames<-fileList
  temp<-paste(path, fileNames[1], sep="")
  df.temp<-read.csv(file=temp, sep=",", header=T)
  df.temp$Date<-as.character(df.temp$Date)
  df.temp$Date<-paste(substr(df.temp$Date, 1,4), substr(df.temp$Date, 5,6), substr(df.temp$Date,7,8),sep="-")
  df.temp$Date<-as.Date(df.temp$Date)
  
  endDate<-as.Date(paste(paste("20", substr(fileNames[1],4,5),sep=""), substr(fileNames[1],6,7), "01", sep="-")) -6
  
  df.temp<-df.temp[df.temp$Date<endDate,]
  
  for (i in 2:length(fileNames)) {
    
    temp<-paste(path, fileNames[i], sep="")
    
    df.x<-read.csv(file=temp, sep=",", header=T)
    df.x$Date<-paste(substr(df.x$Date, 1,4), substr(df.x$Date, 5,6), substr(df.x$Date,7,8),sep="-")
    df.x$Date<-as.Date(df.x$Date)
    df.x<-df.x[df.x$Date>endDate,]    
    endDate<-as.Date(paste(paste("20", substr(fileNames[i],4,5),sep=""), substr(fileNames[i],6,7), "01", sep="-")) -6
    
    df.x$Date<-as.Date(df.x$Date)
    
    df.temp<- rbind(df.temp, df.x[df.x$Date<=endDate ,])    
  }
  return (df.temp)
}

#Read all files from a folder into a data frame
readDataFromFolder<- function (fileNames, path) {
  temp<-paste(path, fileNames[1], sep="")
  df.temp<-read.csv(file=temp, sep=",", header=T)
  
  for (i in 2:length(fileNames)) {
    temp<-paste(path, fileNames[i], sep="")
    df.temp<- rbind(df.temp, read.csv(file=temp, sep=",", header=T))    
  }
  return (df.temp)
}

#Combine Time and Date into a single column
cleanMarketData<-function(dataframe.input) {
  dataframe.input$Date<-as.character(dataframe.input$Date)
  #   dataframe.input$Date<-paste(substr(dataframe.input$Date, 1,4), substr(dataframe.input$Date, 5,6), substr(dataframe.input$Date,7,8),sep="-")
  #   dataframe.input$Date <- as.Date(dataframe.input$Date)
  
  dataframe.input$Time<-as.character(dataframe.input$Time)
  dataframe.input$Time[nchar(dataframe.input$Time) == 1]<-paste("000", dataframe.input$Time[nchar(dataframe.input$Time) == 1],sep="")
  dataframe.input$Time[nchar(dataframe.input$Time) == 2]<-paste("00", dataframe.input$Time[nchar(dataframe.input$Time) == 2],sep="")
  dataframe.input$Time[nchar(dataframe.input$Time) == 3]<-paste("0", dataframe.input$Time[nchar(dataframe.input$Time) == 3],sep="")
  
  dataframe.input$Time<- paste(substr(dataframe.input$Time,1,2), substr(dataframe.input$Time,3,4),"00",sep=":")
  dataframe.input$Period<- paste(dataframe.input$Date, dataframe.input$Time)
#   dataframe.input$Period<- as.POSIXlt(dataframe.input$Period)
  
  dataframe.input<-dataframe.input[,c("Period","Open","High","Low","Close","Volume")]
  colnames(dataframe.input)<-c("datetime","open","high","low","close","vol")
  return (dataframe.input)
}

#Query
dbGetQuery(myconn, "SELECT * FROM GC_1")