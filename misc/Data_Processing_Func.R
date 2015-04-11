library(lubridate)
library(RMySQL)

readDataFromFolder<- function (fileNames, path) {
  #   fileNames<-fileList.ask
  temp<-paste(path, fileNames[1], sep="")
  df.temp<-read.csv(file=temp, sep=",", header=T)
  df.temp$Date<-as.character(df.temp$Date)
  df.temp$Date<-as.Date(paste(substr(df.temp$Date, 1,4), substr(df.temp$Date, 5,6), substr(df.temp$Date,7,8),sep="-"))
  
  #Get contract month from the filename. Take data up till day 27 before of month before contract month before rolling over to next contract.
  endDate<-as.Date(paste(paste("20", substr(fileNames[1],4,5),sep=""), substr(fileNames[1],6,7), "27", sep="-"))
  month(endDate)<-month(endDate)-1
  
  df.temp<-df.temp[df.temp$Date<=endDate,]

  for (i in 2:length(fileNames)) {
    temp<-paste(path, fileNames[i], sep="")
    
    df.x<-read.csv(file=temp, sep=",", header=T)
    df.x$Date<-as.Date(paste(substr(df.x$Date, 1,4), substr(df.x$Date, 5,6), substr(df.x$Date,7,8),sep="-"))
    df.x<-df.x[df.x$Date>endDate,]    
 
    endDate<-as.Date(paste(paste("20", substr(fileNames[i],4,5),sep=""), substr(fileNames[i],6,7), "27", sep="-"))
    month(endDate)<-month(endDate)-1
    
    df.x$Date<-as.Date(df.x$Date)
    
    df.temp<- rbind(df.temp, df.x[df.x$Date<=endDate ,])    
  }
  return (df.temp)
}

#Combine Time and Date into a single column
cleanMarketData<-function(dataframe.input) {
  dataframe.input$Date<-as.character(dataframe.input$Date)
  
  dataframe.input$Time<-as.character(dataframe.input$Time)
  dataframe.input$Time[nchar(dataframe.input$Time) == 1]<-paste("000", dataframe.input$Time[nchar(dataframe.input$Time) == 1],sep="")
  dataframe.input$Time[nchar(dataframe.input$Time) == 2]<-paste("00", dataframe.input$Time[nchar(dataframe.input$Time) == 2],sep="")
  dataframe.input$Time[nchar(dataframe.input$Time) == 3]<-paste("0", dataframe.input$Time[nchar(dataframe.input$Time) == 3],sep="")
  
  dataframe.input$Time<- paste(substr(dataframe.input$Time,1,2), substr(dataframe.input$Time,3,4),"00",sep=":")
  dataframe.input$Period<- paste(dataframe.input$Date, dataframe.input$Time)
  
  dataframe.input<-dataframe.input[,c("Period","Open","High","Low","Close","Volume")]
  colnames(dataframe.input)<-c("datetime","open","high","low","close","vol")
  return (dataframe.input)
}