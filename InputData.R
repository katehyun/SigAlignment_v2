rm(list=ls())
install.packages("RPostgreSQL")
library( RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- f.dbinfo (drv)


# rs <- dbSendQuery(con, "SELECT * FROM wimdata.wimdata_ird_processed_112;")
# fetch(rs,n=-1) 
rsWIM <- dbGetQuery(con, 
    "SELECT id, ts_controller, ts_fieldunit, vehnum, lane, 
     ((vehlencm/100+1.8288) / (speedkmh*1000/3600 )::float) as duration, speedkmh, vehlencm, numaxles  
     FROM wimdata.wimdata_ird_processed_112
     where ts_fieldunit > 1415692800000 and ts_fieldunit < 1415775600000 
     and speedkmh > 0;");
# 11/11/2014

 
rsSIG <- dbGetQuery(con, 
      " SELECT id, lane, timestamp, timestamp_sys, samples, vehicle_count, duration, 
        extract(hour from timestamp_full::time) as hr,
        extract(minute from timestamp_full::time) as min,  extract(second from timestamp_full::time) as sec, 
        1000* ( 1415692800 + 3600*extract(hour from timestamp_full::time) + 60*extract(minute from timestamp_full::time) 
        + extract(second from timestamp_full::time)) as utc
        FROM public.signature
        where timestamp_full > '2014-11-11 00:00:000' and timestamp_full < '2014-11-11 23:00:000'; ")

dbDisconnect(con);


rsWIM[,3] <- as.numeric(rsWIM[,3])
rsWIM[,6] <- as.numeric(rsWIM[,6])
rsSIG[,7] <- as.numeric(rsSIG[,7])
rsSIG[,11] <- as.numeric(rsSIG[,11])

rsWIM <- format(rsWIM, scientific=FALSE)
rsSIG <- format(rsSIG, scientific=FALSE)

#cleanup : duration
rsWIM <- subset(rsWIM, rsWIM[,6] < 1.5)
rsSIG <- subset(rsSIG, rsSIG[,7] < 1.5)


# lane
rsWIM_lane1 <- subset(rsWIM, as.numeric( rsWIM[,5]) == 1)
rsWIM_lane2 <- subset(rsWIM, as.numeric( rsWIM[,5]) == 2)
rsSIG_lane1 <- subset(rsSIG, as.numeric (rsSIG[,2]) == 1)
rsSIG_lane2 <- subset(rsSIG, as.numeric (rsSIG[,2]) == 2)

rsSIG_lane1 <- rsSIG_lane1[order( rsSIG_lane1[,11]),]
rsSIG_lane2 <- rsSIG_lane2[order( rsSIG_lane2[,11]),]

write.table(rsWIM_lane1, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/test/WIM1.txt", sep="\t",row.names=FALSE)
write.table(rsWIM_lane2, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/test/WIM2.txt", sep="\t",row.names=FALSE)
write.table(rsSIG_lane1, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/test/SIG1.txt", sep="\t",row.names=FALSE)
write.table(rsSIG_lane2, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/test/SIG2.txt", sep="\t",row.names=FALSE)

# FUNCTION
f.findNN <- function (WIM, SIG, SeqTs){
  
  MinDuration <- c()
  SumofDuration <- vector()
  
  for (z in SeqTs) {
    
    t <- t+1;
    
    
    X <- as.numeric(WIM[,3])
    y <- data.frame()
    
    for (i in 1:length(SIG[,1])){  
      y[i,1] <- as.numeric(SIG[i,11]) + z
    }
    
    cl <- seq(c(length(X)))
    nn<-knn(X, y, cl, k = 1)
    
    
    indices = unlist( attr(nn, "nn.index"))
    dists = unlist( attr(nn,"nn.dist"))
    
    
    uniqueIxs <- unique(indices)
    nnResult <- cbind(indices, dists)
    
    maxindex <- c()
    temp <- vector()
    
    
    
    # ID , Duration
    nnResult <- cbind( nnResult, as.numeric (SIG[,11]), as.numeric (SIG[,7]) )
    nnResult <- cbind (nnResult, WIM[ as.numeric (nnResult[,1]),3],  WIM[ as.numeric(nnResult[,1]) ,6] )
    nnResult <- cbind (nnResult, abs ( as.numeric (nnResult[,4]) - as.numeric (nnResult[,6])),
                       abs ( as.numeric (nnResult[,3]) - as.numeric (nnResult[,5])))
    nnResult <- cbind( ave(nnResult[,1], nnResult[,1], FUN = seq_along), nnResult) 
    #         nnResult <- nnResult[, -5:-6]
    
    result <- data.frame("var1"=character(1), "var2"=character(1), "var3"=character(1), "var4"=character(1),
                         "var5"=character(1), "var6"=character(1), "var7"=character(1), "var8"=character(1),"var9"=character(1),
                         stringsAsFactors=FALSE)
    
    for (a in uniqueIxs){
      
      temp <-  subset(nnResult, as.numeric(nnResult[,2])==a) 
      whichmin <- which.min(temp[,8]) # Option 2 : duration
      #           whichmin <- which.min(temp[,9]) # option 1 : ts
      result <- rbind(result, temp[whichmin,])
      
    }
    
    Matches <- result[2:length(result[,1]),]
    
    
    SumofDuration[t] <- sum(as.numeric (Matches[,8]), na.rm = TRUE)
    MinDuration <- SeqTs [ which.min(SumofDuration) ]
  }
  
  return (list ( matrix = Matches, SumofDuration = SumofDuration, MinDuration = MinDuration))
}



f.findNN_Target <- function (WIM, SIG, TargetDuration){
  
  nnResult <- data.frame()
  z = TargetDuration
  t <- t+1;
  
  
  X <- as.numeric(WIM[,3])
  y <- data.frame()
  
  for (i in 1:length(SIG[,1])){  
    y[i,1] <- as.numeric(SIG[i,11]) + z
  }
  
  cl <- seq(c(length(X)))
  nn<-knn(X, y, cl, k = 1)
  
  
  indices = unlist( attr(nn, "nn.index"))
  dists = unlist( attr(nn,"nn.dist"))
  
  
  uniqueIxs <- unique(indices)
  nnResult <- cbind(indices, dists)
  
  maxindex <- c()
  temp <- vector()
  
  
  
  # ID , Duration
  nnResult <- cbind( nnResult,   as.numeric (SIG[,1]), as.numeric (SIG[,11]), as.numeric (SIG[,7]) )
  nnResult <- cbind (nnResult,WIM[ as.numeric (nnResult[,1]),1], WIM[ as.numeric (nnResult[,1]),3],  WIM[ as.numeric(nnResult[,1]) ,6] )
  nnResult <- cbind (nnResult, abs ( as.numeric (nnResult[,4]) - as.numeric (nnResult[,7])), #ts
                     abs ( as.numeric (nnResult[,5]) - as.numeric (nnResult[,8]))) #duration
  nnResult <- cbind( ave(nnResult[,1], nnResult[,1], FUN = seq_along), nnResult) 
  
  result <- data.frame("var1"=character(1), "var2"=character(1), "var3"=character(1), "var4"=character(1),
                       "var5"=character(1), "var6"=character(1), "var7"=character(1), "var8"=character(1),
                       "var9"=character(1), "var10"=character(1), "var11"=character(1),
                       stringsAsFactors=FALSE)
  
  for (a in uniqueIxs){
    
    temp <-  subset(nnResult, as.numeric(nnResult[,2])==a) 
    whichmin <- which.min(temp[,11]) #duration
    #     whichmin <- which.min(temp[,10])  #ts
    result <- rbind(result, temp[whichmin,])
    
  }
  
  FinalMatches <- result[2:length(result[,1]),]
  
  return (list ( matrix = FinalMatches))
}

#end
###############################################
###############################################
###############################################