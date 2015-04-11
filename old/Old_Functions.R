#Old Backtest function
zzbackTest<- function(df.ab, p.x, x.mult=1, x.ts=1, y.mult=1, y.ts=1, s1s2.slp=1, x.size=1, updateProgress=NULL){
  
  df.x<-df.ab
  xrow<-nrow(df.x)
  
  # 1=buy gold short silver, 2= buy silver short gold, -1 = exit trade
  inTrade<-0
  hasEntered<-F
  
  #Matrix order: Row [Price, Size, Long/Short, Multiplier, Ticksize] Col[X,Y]
  order<-matrix(0,2,5)
  order[,4]<-c(1, 1)
  order[,5]<-c(x.ts, y.ts)
  #   drawdown<-matrix(0,2,5)
  rOrder<-matrix(0,2,5)
  
  df.x$pl <- 0
  df.x$cum.pl<-0
  df.x$bar.pl<-0
  df.x$running.pl<-0
  
  df.x$position<-""
  #   df.x$drawdown<-0
  df.x$x.size<-0
  df.x$y.size<-0
  df.x$x.price<-""
  df.x$y.price<-""
  df.x$x.profit<-0
  df.x$y.profit<-0
  
  print("Running Backtest")
  
  for (i in p.x:xrow) {
    
    if (inTrade==0){
      if (df.x$signal[i]==1){  #Criteria for entering trade [s1s2 is negative]
        #Buy gold sell silver
        df.x$position[i]<-"Enter"
        hasEntered<-F
        
        if (asset.cor>0){
          order[,3]<-c(1,-1)
        } else {
          order[,3]<-c(-1,-1) #If s1s2 is positive, long both
        }
        
        inTrade<-1
      } else if (df.x$signal[i]==2){  #Criteria for entering trade [s1s2 is positive]
        
        #Buy silver sell gold
        df.x$position[i]<-"Enter"
        hasEntered<-F
        
        if (asset.cor>0){
          order[,3]<-c(-1,1)
        } else {
          order[,3]<-c(1,1) #If s1s2 is positive, short both as both are overvalued
        }
        
        inTrade<-2
      }
    } else if (inTrade!=0) {
      
      #Get order info when entering trade
      if (hasEntered==F){
        order[,c(1,2)]<-c(df.x$open.x[i],df.x$open.y[i],x.size,df.x$ATR.ratio[i-1]) #use ATR ratio from previous datapoint
        #         drawdown[,]<-order[,]
        hasEntered<-T
      } 
      
      if (hasEntered & df.x$position[i-1]!="Exit"){
        
        df.x$position[i]<-"Hold"
        
        if(order[1,3]==-1){
          df.x$x.price[i]<-paste("S:",order[1,1])
        } else{
          df.x$x.price[i]<-paste("L:",order[1,1])
        }
        
        if(order[2,3]==-1){
          df.x$y.price[i]<-paste("S:",order[2,1])
        } else{
          df.x$y.price[i]<-paste("L:",order[2,1])
        }
        
        df.x$x.size[i]<-order[1,2]
        df.x$y.size[i]<-order[2,2]
        
        rOrder[,]<-order[,]
        
      } #End of Matrix price and size calculation
      
      #Get current profit and loss per bar   
      if (df.x$position[i-1]=="Enter"){
        #On enter, use Close - Open price
        rOrder[1,1]<-(df.x$close.x[i]-df.x$open.x[i])
        rOrder[2,1]<-(df.x$close.y[i]-df.x$open.y[i]) 
      } else if (df.x$position[i] %in% c("Hold","Exit")){
        #while holding, use today's close minus ytd close
        rOrder[1,1]<-(df.x$close.x[i]-df.x$close.x[i-1])
        rOrder[2,1]<-(df.x$close.y[i]-df.x$close.y[i-1])
      } else if (df.x$position[i-1]=="Exit"){
        #On exit, use today's open - ytd close
        rOrder[1,1]<-(df.x$open.x[i]-df.x$close.x[i-1])
        rOrder[2,1]<-(df.x$open.y[i]-df.x$close.y[i-1])     
      }
      
      #Calculate total profits
      xProfit<-round(apply(rOrder,1,prod),2)
      df.x$x.profit[i]<-xProfit[1]
      df.x$y.profit[i]<-xProfit[2]
      df.x$bar.pl[i]<-sum(xProfit)
      
      #Criteria for exiting trade
      if (df.x$filter[i]==0 & df.x$position[i-1]!="Exit"){ 
        df.x$position[i]<-"Exit"
      } 
      
      #Calculate profit/loss on exit
      if(df.x$position[i-1]=="Exit") {
        
        order[1,1]<-df.x$open.x[i]-order[1,1]
        order[2,1]<-df.x$open.y[i]-order[2,1]
        total.p<-apply(order,1,prod)
        df.x$pl[i]<- sum(total.p)
        rOrder[c(1:3)]<-0
        order[,c(1:3)]<-0 #Reset values to 0
        inTrade<-0
      }
      
    } #End of inTrade
    
    if (is.function(updateProgress) & i%%(xrow%/%10)==0) {
      text <- paste("Current Progress:", paste(as.character(round((i/xrow)*100)),"%",sep=""))
      updateProgress(detail = text)
    }
    
  } #End of For Loop
  
  #Round numbers to nearest sig fig
  tt<- c("s1s2","open.x","open.y","close.x","close.y","ATR.ratio","s1s2.slope","cor","pl","bar.pl","cum.pl","running.pl")
  df.x[,tt]<-round(df.x[,tt],3)
  
  df.x$cum.pl<-cumsum(df.x$pl)
  df.x$running.pl<-cumsum(df.x$bar.pl)
  
  return (df.x)
}