library(RMySQL)

getData<-function(asset.type)
  if (asset.type == "Gold/Silver 1 min"){
    nname <<- "Gold/Silver 1 min"
    #     myconn <-dbConnect(MySQL(), user="tyzjames", password="**", host="acensus.cnneqcdmtwtz.ap-southeast-1.rds.amazonaws.com", dbname="acensusDB")
    #     df.a1<<-dbGetQuery(myconn, "SELECT * FROM GC_1v3")
    #     df.a2<<-dbGetQuery(myconn, "SELECT * FROM SI_1v3")
    df.a1<<-read.csv(file="data/clean/new/Gold 1min.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/new/Silver 1min.csv", header=TRUE, sep=",")
    tsize<<-c(10,5)
    mult<<-c(10,1000)  
  } else if (asset.type == "Gold/Silver 10 min"){
    nname <<- "Gold/Silver 10 min"
    df.a1<<-read.csv(file="data/clean/Gold 10 min.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/Silver 10 min.csv", header=TRUE, sep=",")
    tsize<<-c(10,5)
    mult<<-c(10,1000)  
  } else if (asset.type == "Gold/Silver 15 min"){
    #       print("GS 10")
    nname <<- "Gold/Silver 15 min"
    df.a1<<-read.csv(file="data/clean/Gold 15 min New.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/Silver 15 min New.csv", header=TRUE, sep=",")
    tsize<<-c(10,5)
    mult<<-c(10,1000)
  } else if (asset.type == "Gold/Silver 60min"){
    #       print("GS 60")
    nname <<- "Gold/Silver 60 min"
    df.a1<<-read.csv(file="data/clean/Gold 60 min.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/Silver 60 min.csv", header=TRUE, sep=",")
    tsize<<-c(10,5)
    mult<<-c(10,1000)
  } else if (asset.type == "DSX / EP 10 min"){
    #       print("DSXEP 10")
    nname <<- "DSX / EP 10 min"
    df.a1<<-read.csv(file="data/clean/20140226/DSX 10 min.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/20140226/EP 10 min.csv", header=TRUE, sep=",")
    tsize<<-c(10,0.5)
    mult<<-c(10,100)
  } else if (asset.type == "NKD/Yen 60 min" ){
    #       print("NKD/Yen 60 min" )
    nname <<- "NKD/Yen 60min" 
    df.a1<<-read.csv(file="data/clean/NKD 60 min.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/Yen 60 min.csv", header=TRUE, sep=",")
    tsize<<-c(5,12.5)
    mult<<-c(1,1)
  } else if (asset.type == "SHFE AU/AG 5 min"){
    nname<<- "SHFE AU/AG 5 min"
    df.a1<<-read.csv(file="data/clean/SHFE_au_clean.csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/SHFE_ag_clean.csv", header=TRUE, sep=",")
    tsize<<--c(10,15)
    mult<<-c(100,1)
  } else if (asset.type == "UOB Data GC/SI 1 min"){
    nname<<- "GC/SI 1 min"
    df.a1<<-read.csv(file="data/clean/GC 1 min (Clean).csv", header=TRUE, sep=",")
    df.a2<<-read.csv(file="data/clean/SI 1 min (Clean).csv", header=TRUE, sep=",")
    tsize<<-c(10,5)
    mult<<-c(10,1000)
  } else if (asset.type == "UOB Data GC/SI 10 min"){
    nname<<- "GC/SI 10 min"
    df.a1<<-df.gc
    df.a2<<-df.si
    tsize<<-c(10,5)
    mult<<-c(10,1000)
  }

