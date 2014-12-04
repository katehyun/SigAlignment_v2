install.packages("FNN")
library(FNN)

# rm(nnIndex, nnResult, y)
# z<-1000 # for test

WIM <- rsWIM_lane1
# WIM <- subset (WIM, as.numeric (WIM[,8]) > 800)
WIM <- subset (WIM, as.numeric (WIM[,6]) > 0.35)
SIG <- rsSIG_lane1
SIG <- subset (SIG, as.numeric (SIG[,7]) > 0.35)
SIG <- SIG[order( SIG[,11]),]

# CHANGE THIS !!! 
startts <- -1000;
endts <-2000;
SeqTs <- seq( startts , endts, by = 1000)

t<-0
SumDuration <- vector(mode="numeric", length=length(SeqTs))
Init_NN <- data.frame() 
Final_NN <- data.frame()
# LOAD FUNCTION #

Init_NN <- f.findNN(WIM, SIG, SeqTs)
TargetDuration <- Init_NN$MinDuration
SumDuration <- Init_NN$SumofDuration

Final_NN <- f.findNN_Target(WIM, SIG, TargetDuration)
Match <- Final_NN$matrix  
MatchID <- cbind(Match[,4], Match[,7])

write.table(MatchID, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/Matchtest0708_laneNotSp.txt", sep="\t",row.names=FALSE)


rsSIG_lane1 [,12] <- MatchID[,2][match ( as.numeric(rsSIG_lane1[,1]) ,as.numeric (MatchID[,1]) )]
rsSIG_lane1 [,13] <- Match[,9][match ( as.numeric(rsSIG_lane1[,1]) ,as.numeric (MatchID[,1]) )]
rsSIG_lane1 [,14] <- Match[,8][match ( as.numeric(rsSIG_lane1[,1]) ,as.numeric (MatchID[,1]) )]

# save table
# write.table(SIG, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/SIGtest0708_laneNotSp.txt", sep="\t",row.names=FALSE)
# write.table(WIM, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/WIMtest0708_laneNotSp.txt", sep="\t",row.names=FALSE)
# write.table(Match, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/Matchtest0708_laneNotSp.txt", sep="\t",row.names=FALSE)
# write.table(rsSIG_lane2, "C:/Users/Kate Hyun/Dropbox/Kate/SigAlignment/Result/SIGRawtest0717_lane2.txt", sep="\t",row.names=FALSE)

    # for (z in SeqTs) {
    #   t <- t+1
    #   Init_NN[t] <- f.findNN(WIM, SIG, z)
    # }
    # 
    # TargetDuration <- which.min (Init_NN$SumofDuration)
    # SumofDuration <- Init_NN $ DurationMat

# 
# # FUNCTION
# f.findNN <- function (WIM, SIG, SeqTs){
# 
# MinDuration <- c()
# SumofDuration <- vector()
# 
#   for (z in SeqTs) {
#     
#     t <- t+1;
# 
#         
#  X <- as.numeric(WIM[,3])
#  y <- data.frame()
#         
#         for (i in 1:length(SIG[,1])){  
#             y[i,1] <- as.numeric(SIG[i,11]) + z
#         }
#         
#         cl <- seq(c(length(X)))
#         nn<-knn(X, y, cl, k = 1)
#         
#         
#         indices = unlist( attr(nn, "nn.index"))
#         dists = unlist( attr(nn,"nn.dist"))
#         
#         
#         uniqueIxs <- unique(indices)
#         nnResult <- cbind(indices, dists)
#         
#         maxindex <- c()
#         temp <- vector()
# 
#        
#         
#         # ID , Duration
#         nnResult <- cbind( nnResult, as.numeric (SIG[,11]), as.numeric (SIG[,7]) )
#         nnResult <- cbind (nnResult, WIM[ as.numeric (nnResult[,1]),3],  WIM[ as.numeric(nnResult[,1]) ,6] )
#         nnResult <- cbind (nnResult, abs ( as.numeric (nnResult[,4]) - as.numeric (nnResult[,6])),
#                            abs ( as.numeric (nnResult[,3]) - as.numeric (nnResult[,5])))
#         nnResult <- cbind( ave(nnResult[,1], nnResult[,1], FUN = seq_along), nnResult) 
# #         nnResult <- nnResult[, -5:-6]
#         
#         result <- data.frame("var1"=character(1), "var2"=character(1), "var3"=character(1), "var4"=character(1),
#                              "var5"=character(1), "var6"=character(1), "var7"=character(1), "var8"=character(1),"var9"=character(1),
#                              stringsAsFactors=FALSE)
#         
#         for (a in uniqueIxs){
#           
#           temp <-  subset(nnResult, as.numeric(nnResult[,2])==a) 
#           whichmin <- which.min(temp[,8]) # Option 2 : duration
# #           whichmin <- which.min(temp[,9]) # option 1 : ts
#           result <- rbind(result, temp[whichmin,])
#         
#         }
#         
#         Matches <- result[2:length(result[,1]),]
# 
#         
#         SumofDuration[t] <- sum(as.numeric (Matches[,8]), na.rm = TRUE)
#         MinDuration <- SeqTs [ which.min(SumofDuration) ]
# }
#           
# return (list ( matrix = Matches, SumofDuration = SumofDuration, MinDuration = MinDuration))
#             }
# 
# 
# 
# f.findNN_Target <- function (WIM, SIG, TargetDuration){
#   
#   
#   z = TargetDuration
#   t <- t+1;
#   
#   
#   X <- as.numeric(WIM[,3])
#   y <- data.frame()
#   
#   for (i in 1:length(SIG[,1])){  
#     y[i,1] <- as.numeric(SIG[i,11]) + z
#   }
#   
#   cl <- seq(c(length(X)))
#   nn<-knn(X, y, cl, k = 1)
#   
#   
#   indices = unlist( attr(nn, "nn.index"))
#   dists = unlist( attr(nn,"nn.dist"))
#   
#   
#   uniqueIxs <- unique(indices)
#   nnResult <- cbind(indices, dists)
#   
#   maxindex <- c()
#   temp <- vector()
#   
#   
#   
#   # ID , Duration
#   nnResult <- cbind( nnResult, as.numeric (SIG[,11]), as.numeric (SIG[,7]) )
#   nnResult <- cbind (nnResult, WIM[ as.numeric (nnResult[,1]),3],  WIM[ as.numeric(nnResult[,1]) ,6] )
#   nnResult <- cbind (nnResult, abs ( as.numeric (nnResult[,4]) - as.numeric (nnResult[,6])),
#                      abs ( as.numeric (nnResult[,3]) - as.numeric (nnResult[,5])))
#   nnResult <- cbind( ave(nnResult[,1], nnResult[,1], FUN = seq_along), nnResult) 
#  
#   result <- data.frame("var1"=character(1), "var2"=character(1), "var3"=character(1), "var4"=character(1),
#                        "var5"=character(1), "var6"=character(1), "var7"=character(1), "var8"=character(1),"var9"=character(1),
#                        stringsAsFactors=FALSE)
#   
#   for (a in uniqueIxs){
#     
#     temp <-  subset(nnResult, as.numeric(nnResult[,2])==a) 
#     whichmin <- which.min(temp[,8]) 
# #     whichmin <- which.min(temp[,9]) 
#     result <- rbind(result, temp[whichmin,])
#     
#   }
#   
#   FinalMatches <- result[2:length(result[,1]),]
#   
#   return (list ( matrix = FinalMatches))
# }
# 
# #end
# ###############################################
# ###############################################
# ###############################################
# 
# 
# 
# 
# 
# f.findNN_Target <- function (WIM, SIG, TargetDuration){
#   
#  
#     z = TargetDuration
#     
# #     rm(nnIndex, nnResult, y, nn , indices, dists)
#     
#     
#     X <- as.numeric(WIM[,3])
#     y <- data.frame()
#     
#     for (i in 1:length(SIG[,1])){  
#       y[i,1] <- as.numeric(SIG[i,11]) + z
#     }
#     
#     cl <- seq(c(length(X)))
#     nn<-knn(X, y, cl, k = 1)
#     
#     
#     indices = unlist( attr(nn, "nn.index"))
#     dists = unlist( attr(nn,"nn.dist"))
#     
#     
#     nnIndex <- unique(indices)
#     nnResult <- cbind(indices, dists)
#     
#     maxindex <- c()
#     temp <- vector()
#     nnIndex<-  cbind(nnIndex, c(0))
#     
#     
#     
#     
#     for (j in 1:length(nnIndex[,1])){
#       for (k in 1: length(nnResult[,1])) {
#         if (nnResult[k,1] == nnIndex[j,1]) {
#           
#           temp <-  rbind( abs( as.numeric( WIM[ nnResult[k,1], 6])  -  as.numeric( SIG[k,7] )), temp ) #duration
#           
#           if (nnResult[k,2] == 0) { break }
#           
#         }    
#       }
#       
#       maxindex <- which.min(temp)
#       
#       if (length(maxindex) > 1) {
#         maxindex = 1
#       }
#       
#       nnIndex[j,2] <- as.numeric(maxindex)
#       temp <- vector()
#       
#     }
#     
#     
#     # numbering 
#     nnResult <- cbind( ave(nnResult[,1], nnResult[,1], FUN = seq_along), nnResult) 
#     nnResult <- cbind( nnResult, SIG[,11]) # SIG ID (utc)
#     nnResult <- cbind(nnResult , WIM[as.numeric ( nnResult[,2]),3] ) # wIM ID (utc)
#     
#     # duration comp
#     nnResult <- cbind(nnResult ,SIG[,7][match ( nnResult[,4],  SIG[,11] ) ] ) 
#     nnResult <- cbind(nnResult ,WIM[,6][match ( nnResult[,5] , WIM[, 3] ) ] )
#     colnames(nnResult) <- c("num", "nnID", "dist", "SIG", "WIM" , "SIGDuration" ,"WIMDuration")
#     nnResult  <- nnResult[order(as.numeric (nnResult[,2])),]
#     
#     nnIndex <- nnIndex[,c(2, 1)]
#     Matches <-  subset(nnResult[,4:7], as.numeric(nnResult[,1]) %in% as.numeric(nnIndex[,1]) &
#                          as.numeric (nnResult[,2]) %in% as.numeric (nnIndex[,2]) )
#     
# #     DurDiff <- abs ( as.numeric ( Matches[,3] ) - as.numeric ( Matches[,4] ) )
# #     SumofDuration[t] <- sum(DurDiff, na.rm = TRUE)
# #     MinDuration <- SeqTs [ which.min(SumofDuration) ]
#     
#  
#   
#   return (list ( matrix = Matches))
# }
# 
# 
# 
# ############# end
# test <- nnResult[ match ( nnIndex[,1], nnResult[,2]) & (nnIndex[,2] , nnResult[,1]))
# 
# nnIndex2 <- aggregate( nnResult[,8], list (nnResult[,2]),  min(nnResult[,8]), na.rm=TRUE)
# 
# 
# nnIndex <- nnIndex[order(as.numeric(nnIndex[,1])), ]
# nnIndex <- nnIndex[,c(2, 1)]
# 
# Matches <- data.frame()
# 
# nnResultIdx <- cbind(as.numeric (nnResult[,1]), as.numeric(nnResult[,2]))
# 
# nnResult[match (nnIndex[,1:2], nnResultIdx[,1:2] ) ]
# 
# test <-  which (nnResultIdx == nnIndex)
# 
# Matches <-  subset(nnResult[,4:7],  as.numeric(nnResult[,1]) %in% as.numeric(nnIndex) & as.numeric (nnResult[,2]) %in% as.numeric (nnIndex[,2]) ])
# 
# test <- intersect (nnResultIdx, nnIndex )
# match(nnResultIdx, nnIndex)
# 
# nnResultIdx[nnIndex[,1] == nnResultIdx[,1] & nnIndex[,2] == nnResultIdx[,2]  ]
# 
# Matches <-  merge(nnResult[,4:7], by= [ as.numeric(nnResult[,1]) %in% as.numeric(nnIndex[,1]) &
#                                           as.numeric (nnResult[,2]) %in% as.numeric (nnIndex[,2]) ])
# 
# z <- 0
# Matches <- data.frame()
# 
# for (j in 1:length(nnIndex[,1])){
#   for (k in 1: length(nnResult[,1])) {
#     
#     if (nnResult [k,1] == nnIndex [j,1] & nnResult [k,2] == nnIndex [j,2] ) {
#       
#       temp <-  nnResult[k,4:7] 
#       
#     }
#   }
#   Matches <- rbind(Matches, temp)
#   temp <- vector()
# }
# 
# 
# 
# for (j in 1:length(nnIndex[,1])){
#   for (k in 1: length(nnResult[,1])) {
#     if (nnResult[k,1] == nnIndex[j,1]) {
#       
#       temp <-  rbind( abs( as.numeric( WIM[ nnResult[k,1], 6])  -  as.numeric( SIG[k,7] )), temp ) #duration
#       
#       if (nnResult[k,2] == 0) { break }
#       
#     }    
#   }
#   
#   maxindex <- which.min(temp)
#   
#   if (length(maxindex) > 1) {
#     maxindex = 1
#   }
#   
#   nnIndex[j,2] <- as.numeric(maxindex)
#   temp <- vector()
#   
# }
# 
# 
# # numbering 
# nnResult <- cbind( ave(nnResult[,1], nnResult[,1], FUN = seq_along), nnResult) 
# nnResult <- cbind( nnResult, SIG[,11]) # SIG ID (utc)
# nnResult <- cbind( nnResult , WIM[as.numeric ( nnResult[,2]),3] ) # wIM ID (utc)
# 
# # duration comp
# nnResult <- cbind(nnResult ,SIG[,7][match ( nnResult[,4],  SIG[,11] ) ] ) 
# nnResult <- cbind(nnResult ,WIM[,6][match ( nnResult[,5] , WIM[, 3] ) ] )
# colnames(nnResult) <- c("num", "nnID", "dist", "SIG", "WIM" , "SIGDuration" ,"WIMDuration")
# nnResult  <- nnResult[order(as.numeric (nnResult[,2])),]
# 
# nnIndex <- nnIndex[,c(2, 1)]
# Matches <-  subset(nnResult[,4:7], as.numeric(nnResult[,1]) %in% as.numeric(nnIndex[,1]) &
#                      as.numeric (nnResult[,2]) %in% as.numeric (nnIndex[,2]) )
# 
# DurDiff <- abs ( as.numeric ( Matches[,3] ) - as.numeric ( Matches[,4] ) )
# SumofDuration[t] <- sum(DurDiff, na.rm = TRUE)
# MinDuration <- SeqTs [ which.min(SumofDuration) ]
# 
# }
# 
# return (list ( matrix = Matches, SumofDuration = SumofDuration, MinDuration = MinDuration))
# }
