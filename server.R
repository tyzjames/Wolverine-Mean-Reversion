library(shiny)
library(shinydashboard)
library(scales)
source("data_processing.R")
source("dataload.R")

options(shiny.trace=TRUE)
options(digits=5)

asset.cor<<-0

shinyServer(function(input, output, session) {
  
  load.data<-reactive({
    getData(input$asset.type)
  })
  
  refresh.data<-function(p.x, stdev, thres, signal.type, x.size, y.size, updateProgress=NULL){
    df.b1<<-processData(df.a1, p.x, mult[1], tsize[1])
    df.b2<<-processData(df.a2, p.x, mult[2], tsize[2])
    df.cMerged<<-mergeData(df.b1, df.b2, p.x)
    df.dProcessed<<-processSignals(df.cMerged, p.x, stdev, thres, signal.type, tsize, x.size, y.size, updateProgress)
    df.eBT<- backTest(df.dProcessed, tsize, x.size, updateProgress)
    df.eBTx<<- df.eBT
    return(df.eBT)
  }
  
  #Observe Run Test button to refresh data
  rdata<-reactive({
    if (input$generate == 0)
      return()
    
    progress <- shiny::Progress$new()
    progress$set(message = "Running", value = 0)
    on.exit(progress$close()) # Close the progress when this reactive exits (even if there's an error)
    
    updateProgress <<- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    isolate({
      cat("Refreshing data")
      load.data()
      return(refresh.data(input$period.x, input$stdev.value, input$thres, input$signal.type, input$x.size, input$y.size, updateProgress))
    })
  }) #End of observe
  
  output$plotEquityCurve<- renderPlot({
    if (input$generate == 0)
      return()
    isolate({
      df.eBT<-rdata()
      par(mar=c(2,2,2,0), mfrow=c(2,1))
      
      #Plot Running Equity Curve
      par(new = F)
      plot(x=df.eBT[1:nrow(df.eBT),"total.pl"], type="l", col="black", main = "Equity Curve")
      abline(h=c(0),col="red",lty=2)
      
      #Plot Swing
      par(new = F)
      barplot(df.eBT[1:nrow(df.eBT),"daily.pl"],col="black",xlab="",ylab="",axes=T, border=T, main="P/L Swing")
    })
  })
  
  #Plot S1S2 curve as well as open prices for both objects
  output$plotGS<-renderPlot({
    if (input$generate == 0)
      return()
    isolate({
      df.eBT<-rdata()
      par(mfrow=c(3,1),mar=c(2,2,2,0))
      
      #Plot S1S2
      par(new=F)
      plot(x=df.eBT[1:nrow(df.eBT),"s1s2"], type="l", col="red", xlab="",ylab="", axes=F, frame.plot=T)
      lines(x=df.eBT$s1s2.bband[,1],col="black", lty=1)
      lines(x=df.eBT$s1s2.bband[,2],col="black", lty=1)
      lines(x=df.eBT$s1s2.bband[,3],col="black", lty=1)
#       abline(h=c(input$thres, input$thres*-1,0),col="black", lty=2)
      Axis(side=1,labels=F)
      title(main="S1S2")
      
      #Plot zScore
      par(new = F)
      plot(x=df.eBT[1:nrow(df.eBT),"zScore"], type="l",col="red",xlab="",ylab="",axes=T, main="z-Score")
      lines(x=df.eBT$z.bband[,1],col="black", lty=1)
      lines(x=df.eBT$z.bband[,2],col="black", lty=1)
      lines(x=df.eBT$z.bband[,3],col="black", lty=1)
      
      #Plot Open Prices
      par(new=F)
      plot(x=df.eBT[1:nrow(df.eBT),"open.x"], type="l", col="red", xlab="",ylab="", axes=F, frame.plot=T)
      par(new=T)   
      plot(x=df.eBT[1:nrow(df.eBT),"open.y"], type="l", col="blue", xlab="",ylab="", axes=F)
      title(main="Open Prices")
      
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(nname," - Backtest ", Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(processDownloadData(rdata()), file)
    }
  )
  
  output$getTable<-renderDataTable({
    if (input$generate == 0)
      return()
    isolate({
      df.eBTemp <- processDownloadData(rdata())
      return(df.eBTemp)
    })
  }
  , options=list(orderClasses=F, searching=F, autoWidth=T, lengthChange=F, ordering=F, scrollY={"500px"}, pageLength=500 )
  )
  
  
  output$getStatsText<-renderUI({
    if (input$generate == 0)
      return()
    isolate({
      df.eBT<-rdata()
      ttc<-sum(df.eBT$inTrade[df.eBT$inTrade==2]) #2 = enter trade
      avg.win<-mean(df.eBT$daily.pl[df.eBT$daily.pl>0])
      avg.loss<-mean(df.eBT$daily.pl[df.eBT$daily.pl<0])
      win.trds<-sum(df.eBT$inTrade[df.eBT$daily.pl>0])/ttc*100
      
      #       str<-paste("Contract: ",input$pair.type,"<br/>")
      str<-paste(nname,"<br/>")
      str<-paste(str,"Asset Correlation: ",round(asset.cor,2),"<br/>")
      str<-paste(str,"Total Trade Count: ",comma(ttc),"<br/>")
      str<-paste(str,"Total Equity: ",dollar(sum(df.eBT$daily.pl)),"<br/>")
      #       str<-paste(str,"Winning Trades: ",paste(round(win.trds,1),"%",sep=""),"<br/>")
      #       str<-paste(str,"Losing Trades: ",paste(round(100-win.trds,1),"%",sep=""),"<br/>")
      str<-paste(str,"Max Win: ",dollar(max(df.eBT$daily.pl)),"<br/>")
      str<-paste(str,"Max Loss: ",dollar(min(df.eBT$daily.pl)),"<br/>")
      str<-paste(str,"Average Win: ",dollar(avg.win),"<br/>")
      str<-paste(str,"Average Loss: ",dollar(avg.loss),"<br/>")
      str<-paste(str,"Expected Value: ",dollar((avg.win*(win.trds/100))+(avg.loss*(1-(win.trds/100)))),"<br/>")
      #       str<-paste(str,"Max Drawdown: ",round(min(df.eBT$drawdown),2),"<br/>")
      str<-paste(str,"Risk Reward Ratio: ",round(avg.win/abs(avg.loss),2))
      return (HTML(str))
    })
  })
  
})