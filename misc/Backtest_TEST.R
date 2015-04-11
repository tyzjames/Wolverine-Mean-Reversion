yyy<-processSignals(m, 10,0.3)
xxx<-backTest(yyy,10)


mode(df.p$filter)

x<-processData(df.one)
y<-processData(df.two)
m<-mergeData(x, y)
n<-processSignals(m)
b<-backTest(n)

asset.cor<<-cor(matrix(c(m$open.x, m$open.y), ncol=2))[1,2]
m$s1s2<- round(m$relCls.x - m$relCls.y,3)


n[n$signal!=0,]

df.x<-n
#Today's close minus today's open to calculate profit for Enter period
df.x$clOp.x<-df.x$close.x-df.x$open.x
df.x$clOp.y<-df.x$close.y-df.x$open.y

#Today's close minus ytd close to calculate profit for holding period
df.x$clPcl.x<-df.x$close.x-df.x$pClose.x
df.x$clPcl.y<-df.x$close.y-df.x$pClose.y

#Today's open minus ytd close to calculate profit for exit period
df.x$opPcl.x<-df.x$open.x-df.x$pClose.x
df.x$opPcl.y<-df.x$open.y-df.x$pClose.y

t<-df.x[(df.x$signal!=0 | df.x$filter==0),]
df.x$fSignal[df.x$fFilter==0]<- -1

if (asset.cor>0){
  df.x$pos.x[df$fSignal==1 & asset.cor>0]<-   1
  df.x$pos.y[df$fSignal==1 & asset.cor>0]<-   -1
  df.x$pos.x[df$fSignal==2 & asset.cor>0]<-   -1
  df.x$pos.y[df$fSignal==2 & asset.cor>0]<-   -1
} else {
  df.x$pos.x[df$fSignal==1 & asset.cor<=0]<-  -1  
  df.x$pos.y[df$fSignal==1 & asset.cor<=0]<-  -1
  df.x$pos.x[df$fSignal==2 & asset.cor<=0]<-  1
  df.x$pos.y[df$fSignal==2 & asset.cor<=0]<-  1
}

df.x$pos.x[1:3]<-df.x$pos.x[startRow]


cc<-backTest2(n)

df.x<-n