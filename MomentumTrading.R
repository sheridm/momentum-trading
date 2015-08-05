#setwd("~/Google Drive/AlgoTesting")   #air
setwd("C:/Users/matt/Google Drive/AlgoTesting")

#install.packages("quantmod")
install.packages("zoo")

library(quantmod)
library(zoo)

rm(list=ls())

sDate <- (Sys.Date() - 300) #Start Date
eDate <- Sys.Date() #End   Date
brk <- 90 #channel 
ATR.sm <- 20 #small ATR window
mAveIndex <- 200 #determines if overall market is bullish
mAveStock <- 100 #determines if individual stocks are bullish
fName <- c("tickersUK.csv")   #tickersFTAS.csv","tickersFutures.csv","tickersEU.csv","tickersS&P.csv")
maxGap <- 0.15
account <- 30000
impact <- 0.001

get.symbol <- function(ticker) {  
  tryCatch(temp <- adjustOHLC(getSymbols(ticker, auto.assign=FALSE, 
                                   from = sDate, to = eDate, warning = FALSE),use.Adjust=TRUE)[,1:4],    
           error = function(e) {
             message("-ERROR-")
             message(paste("Error for ticker symbol  :", ticker, "\n"))
             message("Here's the original error message: ")
             message(e)
             message("")
             return(NULL)},
           finally = {
             message(paste("Data was processed for symbol:", "[", ticker, "]" ))
             message("\n","******************************************", "\n")  
           }) 
}

get.symbol.quandl <- function(ticker) {  
  tryCatch(temp <- Quandl(ticker, "xts", sDate, eDate)
                                        [,c("Open","High","Low","Settle")],    
           error = function(e) {
             message("-ERROR-")
             message(paste("Error for ticker symbol  :", ticker, "\n"))
             message("Here's the original error message: ")
             message(e)
             message("")
             return(NULL)},
           finally = {
             message(paste("Data was processed for symbol:", "[", ticker, "]" ))
             message("\n","******************************************", "\n")  
           }) 
}


mylm <- function(prices, toggle=1) #linear regression model
{
  yTrim <- log(prices[(length(prices)-(brk-1)):length(prices)] )
  xTrim <- 1:90
  temp <- lm(yTrim~xTrim)
  if(toggle == 1){
    return(summary(temp)$r.squared)}
  else{
    return(((1+temp$coefficients[2])^250)-1)  }
}


myFunc <- function(x)
{  
  ticker_symbol <- read.csv(x, header = T,stringsAsFactors = F)
  result <- do.call(cbind,lapply(ticker_symbol$SYMBOLS,get.symbol))
  result <- na.locf(result)
#write.table(result, file = "FTAS.csv")#, sep = ",", row.names= index(result), col.names = NA)

#----------------------------- ATR calcs ------------------------------------------
num <- 1:ncol(result)
ind <- matrix(c(num,rep(NA,4-ncol(result)%%4)), byrow = TRUE, ncol=4)
ind <- data.frame(t(na.omit(ind[,-1]))) #remove the Open px

atrs <- do.call(cbind,lapply(ind,function(i) try(ATR(result[,i],
                                                     n=ATR.sm, maType="EMA")$atr)))
trx <- do.call(cbind,lapply(ind,function(i) try(ATR(result[,i],
                                                     n=ATR.sm, maType="EMA")$tr)))

#------------------------------- MA calcs ----------------------------------------
ind <- ind[-(1:2),] #remove the Hi & Lo px to leave Close only
maInd <- do.call(cbind,lapply(ind,function(i) try(SMA(result[,i], n=mAveIndex))))
maStk <- do.call(cbind,lapply(ind,function(i) try(SMA(result[,i], n=mAveStock))))

relTrb <- do.call(cbind,lapply(ind, function(i) trx[,i/4]/lag(result[,i],1)))

#-------------------Create Features for Today -------------------------------------

df.l <- nrow(result)

# create dataframe with rownames = tickers and first column = last close

df <- as.data.frame(t(Cl(result[df.l])))
colnames(df) <- "LastClose"
rownames(df) <- gsub(".Close","",row.names(df))

df$PrevClose <- as.vector(t(Cl(result[df.l-1])))
df$rSqd <- apply(Cl(result),2,mylm,1)
df$grad <- apply(Cl(result),2,mylm,2)
df$atrs <- as.vector(t(atrs[nrow(atrs)]))
df$maInd <- as.vector(t(maInd[nrow(maInd)]))
df$maStk <- as.vector(t(maStk[nrow(maStk)]))
df$maxTr <- apply(relTrb[(nrow(relTrb)-brk):nrow(relTrb),],2,max,na.rm = TRUE)
df$rank <- df$rSqd * df$grad
df$shares <- (account * impact)/df$atrs
df$cost <- df$shares * df$LastClose
#
#----------------------- filters -------------------------------------------------
# 
df.sub <-df[!(df$LastClose < df$maStk),]
df.sub <-df.sub[!(df.sub$maxTr>maxGap),] 
df.sort<- df.sub[order(-df.sub$rank),]

cols <- c("LastClose","maStk","atrs","maxTr","grad","rSqd","rank","shares","cost")
return(round(df.sort[1:20,cols],2))
}

myOutput <- lapply(fName,myFunc)
print(myOutput)

