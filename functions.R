library(shiny)
library(quantmod)
library(ggplot2)
library(tidyr)
library(xtable)
library(lubridate)
library(tseries)
library(PerformanceAnalytics)
library(knitr)
library(Hmisc)
library(rmarkdown)
library(googlesheets)
library(DT)
library(parallel)
library(doSNOW)

options(download.file.method = "libcurl")


getstrategydata<- function(){
  
  ######## doing strategy analysis
  
  market <- c("chineseA", "nasdaq", "nyse", "index")
  strategydata <- data.frame()
  
  for (j in 1:length(market)){
    selectmarket <- market[j]
    filenames <- list.files(path = "strategy_data/" ,pattern = paste0(selectmarket,"*"))
    
    for (i in filenames){
      tempdata <- readRDS(paste0("strategy_data/",i))
      tempdata$market <- selectmarket
      tempdata$algorithm <- unlist(strsplit(unlist(strsplit(i, "_"))[3], "[.]"))[1]
      subtemp <- tempdata[,c(14,30,16,17,18,20,21,22,24,25,27,29)]
      strategydata <- rbind(subtemp, strategydata)
    }
  }
  
  strategydata$totalratio <- strategydata$`stock_alg_Total Returns` / strategydata$`stock Total Returns`
  
  stracolnames <- c("Stock", "Strategy", "Scale", "Strategy Return",
                    "Strategy Annualized Return", "Strategy Max Drawdown", 
                    "Stock Return", "Stock Annualized Return", 
                    "Stock Max Drawdown", "Total Buy", "Winning Percent", 
                    "Market", "Strategy vs Stock Ratio")
  
  colnames(strategydata) <- stracolnames
  strategydata
}

substrategydata <- function(strategydata, input){
  
  if (input$market == "ChineseA" ){
    tempdata <- strategydata[ strategydata$Market == "chineseA"  ,]
  } else if (input$market == "US" ){
    tempdata <- strategydata[ strategydata$Market %in% c("nyse", "nasdaq")  ,]
  } else if (input$market == "US Index" ){
    tempdata <- strategydata[ strategydata$Market == "index"  ,]
  }
  
  if (input$marketstrategy != "All"){
    tempdata <- tempdata[tempdata$Strategy == input$marketstrategy , ]
  }
  
  if (input$StrategyScale != "All"){
    tempdata <- tempdata[tempdata$Scale == input$StrategyScale , ]
  }
  
  if (input$StrategyGreaterStock != "All"){
    if (input$StrategyGreaterStock == "Win"){
      tempdata <- tempdata[ (tempdata$`Strategy Return` > tempdata$`Stock Return`) ,]
    } else {
      tempdata <- tempdata[ (tempdata$`Strategy Return` < tempdata$`Stock Return`) ,]
    }
  }
  
  if (input$StrategyGreaterStockRatio != "All"){
    tempdata <- tempdata[ eval(parse(text=paste0("tempdata$`Strategy vs Stock Ratio` ", input$StrategyGreaterStockRatio))) , ]
  }
  
  if (input$StrategyTotalReturn != "All"){
    tempdata <- tempdata[ eval(parse(text=paste0("tempdata$`Stock Return` ", input$StrategyTotalReturn))) , ]
  }
  
  if (input$StrategyMaxDrawdown != "All"){
    tempdata <- tempdata[ eval(parse(text=paste0("tempdata$`Strategy Max Drawdown` ", input$StrategyMaxDrawdown))) , ]
  }
  
  tempdata[,-c(1:3,12)] <- round(tempdata[,-c(1:3,12)], digits = 2)

  return(tempdata)
}

get_current <- function(gap_data){
    
    cl <- makeCluster(4) # Make clusters
    registerDoSNOW(cl) # use the above cluster
    
    x1 <- foreach(i=1:nrow(gap_data), .combine='rbind')%dopar%{
      
      source('functions.R')
      
      #for (i in 1:nrow(gap_data)){
      
      subgap <- gap_data[i,]
      
      tempinput <- strategy_to_input(subgap$Strategy)
      tempinput$symbol <- subgap$Stock
      tempinput$scale <- subgap$Scale
      tempinput$strategy <- "Off"
      
      tradedata <- plot_pre(tempinput)
      
      subgap$Date <- as.character(last(tradedata$date))
      subgap$Price <- last(tradedata$orig_price)
      subgap$Previous_status <- stockstatus(tradedata$keep_indicator[ nrow(tradedata) -1])
      subgap$Current_status <- stockstatus(tradedata$keep_indicator[ nrow(tradedata) ])
      
      return(subgap)
    }
    # }
    stopCluster(cl)

  
  
  return(x1)
}

stockstatus <- function(keep_indicator){
  
  ifelse( keep_indicator == 1, "Buy", 
          ifelse(keep_indicator == -1, "Sell", 
                 ifelse(keep_indicator <  -1, "Empty",  "Keep"
                 )
  ))
  
}

strategy_to_input <- function(strategy){
  input <- list()
  input$dates <- c(as.Date("1995-01-01"), Sys.Date() )
  
  
  if (strategy == "MACDcross"){
    input$transform <- "Normal"
    input$index <- "MACD"
    input$differential <- "None"
    input$indexscale <- 10
    input$num <- 0
    input$hover <-  NULL
    input$useIndex1 <- "Index 1"
    input$close <- 1
    input$open <- 1
    input$rf <- 0.04
    input$stoploss <- 0
    input$direction <- "Buy Only"
    input$diff_index_strategy <- "Yes"
    input$diff_index <- "EMA9"
    input$connum <- 1
    
  } else if (strategy == "EMA5cross10"){
    input$transform <- "Normal"
    input$index <- "EMA5"
    input$differential <- "None"
    input$indexscale <- 10
    input$num <- 0
    input$hover <-  NULL
    input$useIndex1 <- "Stock"
    input$close <- 1
    input$open <- 1
    input$rf <- 0.04
    input$stoploss <- 0
    input$direction <- "Buy Only"
    input$diff_index_strategy <- "Yes"
    input$diff_index <- "EMA10"
    input$connum <- 1
    
  } else if (strategy == "SMA5cross20"){
    input$transform <- "Normal"
    input$index <- "SMA5"
    input$differential <- "None"
    input$indexscale <- 10
    input$num <- 0
    input$hover <-  NULL
    input$useIndex1 <- "Stock"
    input$close <- 1
    input$open <- 1
    input$rf <- 0.04
    input$stoploss <- 0
    input$direction <- "Buy Only"
    input$diff_index_strategy <- "Yes"
    input$diff_index <- "SMA20"
    input$connum <- 1
    
  } else if (strategy == "SMA5cross10"){
    input$transform <- "Normal"
    input$index <- "SMA5"
    input$differential <- "None"
    input$indexscale <- 10
    input$num <- 0
    input$hover <-  NULL
    input$useIndex1 <- "Stock"
    input$close <- 1
    input$open <- 1
    input$rf <- 0.04
    input$stoploss <- 0
    input$direction <- "Buy Only"
    input$diff_index_strategy <- "Yes"
    input$diff_index <- "SMA10"
    input$connum <- 1
    
  } else if (strategy == "EMA5cross20"){
    input$transform <- "Normal"
    input$index <- "EMA5"
    input$differential <- "None"
    input$indexscale <- 10
    input$num <- 0
    input$hover <-  NULL
    input$useIndex1 <- "Stock"
    input$close <- 1
    input$open <- 1
    input$rf <- 0.04
    input$stoploss <- 0
    input$direction <- "Buy Only"
    input$diff_index_strategy <- "Yes"
    input$diff_index <- "EMA20"
    input$connum <- 1
  }
  
  
  
  return(input)
}

diff2 <- function(x) {diff(diff(x))}
diff3 <- function(x) {diff(diff2(x))}

topercent <- function(x){
  paste0(round( as.numeric(x)*100,2  ) ,"%")
  #as.numeric(x)  
}

fib <- c(3,8,13,21,34,55,89,144,233,377)
neednum <- c(4,5,7,9,10,12,20, 24,26, 30,50,60,100)

indexname <- c(paste0("SMA", sort(c(neednum,fib ))),
               paste0("EMA", sort(c(neednum,fib ))))

checklist <- as.list(indexname)


test_input <- function(x){
  input <- list()
  input$symbol <- x
  input$dates <- c(Sys.Date()-20000, Sys.Date())
  input$showdates <- c(Sys.Date()-20000, Sys.Date())
  input$scale <- "Monthly"
  input$transform <- "Normal"
  input$strategy <- "On"
  input$index <- "MACD"
  input$differential <- "None"
  input$diff_index <- "EMA9"
  input$diff_index_strategy <- "No"
  input$indexscale <- 10
  input$num <-0
  input$connum <- 1
  return(input)
}

plot_pre <- function(input){
  
  #input$dates <- as.Date(input$dates)
  
  #input$dates[1] <- max(input$dates[1], "1995-01-01")
  #input$dates[2] <- min(input$dates[2], "2009-12-31")
  
  #data=getSymbols(input$symbol,from=input$dates[1],to=input$dates[2],
  #                src='yahoo',auto.assign=F)
  #data=getSymbols('SPY',from= Sys.Date()-20000 ,to=Sys.Date(),  src='yahoo',auto.assign=F)
  #data=getSymbols('000300.ss',from= Sys.Date()-20000 ,to=Sys.Date(),  src='google',auto.assign=F)
  
  data <- readRDS(paste0(input$symbol,".rds"))
  #data2 <- read.csv("spx.csv")
  #data2$Date <- as.Date(as.character(data2$Date), format = "%m/%d/%Y")
  #rownames(data2) <- data2$Date
  #data2 <- data2[,-1]
  #colnames(data2) <- c("data.Open","data.High","data.Low","data.Close",  
  #                     "data.Adjusted","data.Volume")
  # saveRDS(data2, "SPX.rds")
  #load("/Users/minchunzhou/Dropbox/Minchun/My_stock/R.stock/spy_0630.RData")
  data <- as.data.frame(data)
  #data2 <- to.weekly(data2) 
  
  if (input$scale == "Daily"){
    data$date  <- as.Date(rownames(data))
  } else if (input$scale == "Weekly"){
    data <- to.weekly(data) 
    data$date  <- as.Date(rownames(data))
  } else if (input$scale == "Monthly") {
    data <- to.monthly(data) 
    data$date  <- as.Date(paste0("1 " ,rownames(data)), format="%d %b %Y" ) %m+% months(1) -1
  }
  
  data <- subset(data, date >= input$dates[1] )
  data <- subset(data, date <= input$dates[2] )
  
  anal_data <- data[,6]
  data$orig_price <- anal_data
  
  if (input$transform == "Log-Transform"){
    anal_data <- log(anal_data)
  }
  
  if (input$index == "MACD"){
    mymacd  <- MACD( anal_data, nFast=12, nSlow=26, nSig=9, maType=EMA, percent = FALSE)
    data$index <-  mymacd[,1]
  } else if  ( substr(input$index,1,3) == "SMA"){
    mysma <- SMA(anal_data,n= as.numeric(substring(input$index,4)) )
    data$index <- mysma
  } else if (substr(input$index,1,3) == "EMA"){
    myema <- EMA(anal_data,n= as.numeric(substring(input$index,4)))
    data$index <- myema
  } else if (input$index == "Stock"){
    data$index <- anal_data
  }
  
  #subdata$diff_index <- EMA(subdata$diff,n= 9)
  
  # macd and diff
  
  data$price <- anal_data
  
  if (input$differential == "None"){
    data$diff <- data$index #+ macd[t]
  } else if (input$differential == "1st"){
    data$diff <- c( NA,diff(data$index)*input$indexscale ) #+ macd[t]
  } else if (input$differential == "2nd"){
    data$diff <- c(NA, NA,diff2(data$index)*input$indexscale) #+ macd[t]
  } else if (input$differential == "3rd"){
    data$diff <- c(NA, NA, NA,diff3(data$index)*input$indexscale) #+ macd[t]
  }
  
  
  data$diff_index <- 0
  
  if (input$useIndex1 == "Stock"){
    if  ( substr(input$diff_index,1,3) == "SMA"){
      data$diff_index <- SMA(anal_data,n= as.numeric(substring(input$diff_index,4)) )
    } else if (substr(input$diff_index,1,3) == "EMA"){
      data$diff_index <- EMA(anal_data,n= as.numeric(substring(input$diff_index,4)))
    }
  } else {
    
    if  ( substr(input$diff_index,1,3) == "SMA"){
      data$diff_index <- SMA(data$diff,n= as.numeric(substring(input$diff_index,4)) )
    } else if (substr(input$diff_index,1,3) == "EMA"){
      data$diff_index <- EMA(data$diff,n= as.numeric(substring(input$diff_index,4)))
    }
    
  }
  
  subdata <- data[, c("date", "diff", "price","orig_price","diff_index" ) ]
  
  
  if (  !( (substr(input$index,1,3) %in% c("SMA","EMA","Sto") ) & 
           (input$differential == "None") ) ){
    
    if (input$diff_index_strategy == "No"){
      subdata$threhold <- (subdata$diff >= input$num)
    } else if (input$diff_index_strategy == "Yes"){
      subdata$threhold <- (subdata$diff >= subdata$diff_index)
    } 
    
    
    subdata$Consecutive <- 1
    subdata$last <- NA
    
    for (i in ( sum(is.na(subdata$threhold)) +2 ) :length(subdata$threhold)){
      if (sign(subdata$threhold[i] == subdata$threhold[i-1])){
        subdata$Consecutive[i] <- subdata$Consecutive[i-1]+1
      } else {
        subdata$Consecutive[i] <-1
        subdata$last[i-1] <-subdata$Consecutive[i-1]
      }
    }
  } else {
    
    
    if (input$diff_index_strategy == "No"){
      # if index threshold = 0, then calculate the time that index greater than the price
      if (input$num == 0) {
        subdata$threhold <- (subdata$diff <= subdata$price)
      } else {
        subdata$threhold <- (subdata$diff >= input$num)
      }
      
    } else if (input$diff_index_strategy == "Yes"){
      subdata$threhold <- (subdata$diff >= subdata$diff_index)
      
    }
    
    
    subdata$Consecutive <- 1
    subdata$last <- NA
    
    for (i in ( sum(is.na(subdata$threhold))+ 2 ):length(subdata$threhold)){
      if (sign(subdata$threhold[i] == subdata$threhold[i-1])){
        subdata$Consecutive[i] <- subdata$Consecutive[i-1]+1
      } else {
        subdata$Consecutive[i] <-1
        subdata$last[i-1] <-subdata$Consecutive[i-1]
      }
    }
    
    
  }
  
  subdata$indicator <- (subdata$Consecutive ==input$connum) * 
    ifelse( subdata$threhold == TRUE , 1,-1  )
  
  subdata$keep_indicator <- subdata$Consecutive * 
    ifelse( subdata$threhold == TRUE , 1,-1  )
  
  if (!is.null(input$checkGroup)){
    
    mycheckall <- data.frame(row.names=row.names(subdata))
    for (i in 1: length(input$checkGroup)){
      
      if ( substr(input$checkGroup[i],1,3) == "SMA"){
        mycheck <- SMA(anal_data,n= as.numeric(substring(input$checkGroup[i],4)) )
      } else if (substr(input$checkGroup[i],1,3) == "EMA"){
        mycheck <- EMA(anal_data,n= as.numeric(substring(input$checkGroup[i],4)))
      }
      
      mycheckall <- cbind(mycheckall, mycheck)
      
    }
    
    colnames(mycheckall) <- paste0("check", 1: length(input$checkGroup))
    
    subdata <- cbind(subdata, mycheckall)
  }
  
  return(subdata)
}

plot_single <- function(input, v1, h1, hover){
  
  subdata <- plot_pre(input)
  
  subdata <- subdata[ (subdata$date > input$showdates[1]) &
                        ( subdata$date < input$showdates[2])  ,  ]
  
  colorset <- rainbow(length(indexname)+1)
  
  
  # price
  #if (input$index == "MACD"){
  if (  !( (substr(input$index,1,3) %in% c("SMA","EMA","Sto") ) & 
           (input$differential == "None") )  ){
    
    #subdata <- data[, c("date", "diff", "price") ]
    #colnames(subdata) <- c("Date", input$index, "Stock")
    #subdata$MACD_Stock <- subdata$MACD + subdata$Stock
    
    #print(subdata  %>% 
    #        gather(key,value, Stock , MACD_Stock)  %>%
    #        ggplot(aes(Date, value, colour=key)) +
    #        geom_line())
    
    plot(subdata$date, subdata$price, type="l", pch="*", xlab="", ylab="Price",
         ylim = c(min(subdata$diff + subdata$price, subdata$price, na.rm = TRUE),
                  max(subdata$diff + subdata$price, subdata$price, na.rm = TRUE)),
         main=paste0(input$symbol, " ", input$scale))
    lines(subdata$date, subdata$diff + subdata$price  , lty= 2, col=colorset[1]) 
    
    if (input$strategy == "On" & (sum(abs(subdata$indicator), na.rm = TRUE) > 0) ){
      for (i in 1: sum(abs(subdata$indicator), na.rm = TRUE) ){
        
        subind <- subdata[subdata$indicator %in% c(1,-1) ,]
        
        if (subind$indicator[i] == 1){
          abline(v= subind$date[i], lty=3, col="red")
          
        } else if (subind$indicator[i] == -1){
          
          abline(v= subind$date[i], lty=3, col="green")
          
        }
      }
    }
  } else {
    plot(subdata$date, subdata$price, type="l", pch="*", xlab="", ylab="Price",
         main=paste0(input$symbol, " ", input$scale))
    lines(subdata$date, subdata$diff , lty= 2, col=colorset[1]) 
    
    if (input$strategy == "On" & (sum(abs(subdata$indicator), na.rm = TRUE) > 0) ){
      for (i in 1: sum(abs(subdata$indicator), na.rm = TRUE) ){
        
        subind <- subdata[subdata$indicator %in% c(1,-1) ,]
        
        if (subind$indicator[i] == 1){
          abline(v= subind$date[i], lty=3, col="red")
          
        } else if (subind$indicator[i] == -1){
          abline(v= subind$date[i], lty=3, col="green")
        }      
      }
    }
  }
  
  if (!is.null(input$checkGroup)){
    for (i in 1: length(input$checkGroup)){
      lines(subdata$date, subdata[, paste0("check",i)] , lty= 6, col=colorset[1+i]) 
    }
  }
  
  if (!is.null(v1$click1$x)){
    near1 <- nearPoints(subdata, v1$click1,
                        xvar="date", yvar="price",threshold=20,maxpoints=1)
    
    points(near1$date, near1$price, col="red")  
    
  }
  
  if (!is.null(v1$click2$x)){
    near2 <- nearPoints(subdata, v1$click2,
                        xvar="date", yvar="price",threshold=20,maxpoints=1)
    
    points(near2$date, near2$price, col="red")  
    
    neardata <- rbind(near1, near2)
    
    clickdata1 <- data.frame(x = neardata$date ,y = neardata$price )
    #clickdata1 <- data.frame(x =c(v1$click1$x, v1$click2$x) ,y = c(v1$click1$y, v1$click2$y) )
    
    m1 <- lm(y~x, clickdata1)
    abline(m1, lty=1, col="red")
  }
  
  
  if (hover == TRUE){
    if (!is.null(h1$hover1$x)){
      #near1 <- nearPoints(subdata, input$hover,
      #xvar="date", yvar="price",threshold=20,maxpoints=1)
      hoverindex <- which.min(abs(subdata$date -  as.Date(h1$hover1$x)))
      hoverdate <- subdata$date[hoverindex]
      hoverdiff <- subdata$diff[hoverindex]
      abline(v=hoverdate, h = hoverdiff, lty=2)
      #abline(v=hoverdate, lty=2)
    } else {
      hoverindex <- which.min(abs(subdata$date -  Sys.Date()))
      hoverdate <- subdata$date[hoverindex]
      hoverdiff <- subdata$diff[hoverindex]
      abline(v=hoverdate, h = hoverdiff, lty=2)
      #abline(v=hoverdate, lty=2)
    }
    
  }
  
  
  
}

plot_index <- function(input, v2, h1, h2, hover){
  subdata <- plot_pre(input)
  subdata <- subdata[ (subdata$date > input$showdates[1]) &
                        ( subdata$date < input$showdates[2])  ,  ]
  
  #colnames(subdata) <- c("Date", "Index", "Stock")
  #print(ggplot(subdata, aes(Date, Index)) + 
  #        geom_line() + 
  #        geom_count(color = "green",
  #                   data = nearPoints(diamonds2, input$click)      )
  
  if (input$differential == "None"){
    main <- paste0( input$index)
  } else {
    main <- paste0( input$index, " ", input$differential, " Differentiation")
  }
  
  plot( subdata$date,subdata$diff, type="l" ,  
        ylim=c(min(subdata$diff , na.rm = TRUE),
               max(subdata$diff , na.rm = TRUE) ), 
        col="blue" , xlab="", ylab="Trend", main=main)
  
  #lines( subdata$date, subdata$trend, lty=1, col="red")
  if (input$diff_index != "None"){
    lines( subdata$date, subdata$diff_index, lty=1, col="red")
  } 
  
  if (!is.null(v2$click1$x)){
    near1 <- nearPoints(subdata, v2$click1,
                        xvar="date", yvar="diff",threshold=20,maxpoints=1)
    
    points(near1$date, near1$diff)  
    
  }
  
  if (!is.null(v2$click2$x)){
    near2 <- nearPoints(subdata, v2$click2,
                        xvar="date", yvar="diff",threshold=20,maxpoints=1)
    
    points(near2$date, near2$diff)  
    
    neardata <- rbind(near1, near2)
    
    clickdata2 <- data.frame(x =neardata$date ,y = neardata$diff )
    #clickdata <- data.frame(x =c(v2$click1$x, v2$click2$x) ,y = c(v2$click1$y, v2$click2$y) )
    
    
    m2 <- lm(y~x, clickdata2)
    abline(m2, lty=1)
  }
  
  if (hover == TRUE){
    if (!is.null(h1$hover1$x)){
      #near1 <- nearPoints(subdata, input$hover,
      #xvar="date", yvar="price",threshold=20,maxpoints=1)
      hoverdate <- subdata$date[which.min(abs(subdata$date -  as.Date(h1$hover1$x)))]
      hoverdiff <- subdata$diff[which.min(abs(subdata$date -  as.Date(h1$hover1$x)))]
      abline(v=hoverdate, h = hoverdiff, lty=2)
    } else {
      hoverdate <- subdata$date[which.min(abs(subdata$date -  Sys.Date()))]
      hoverdiff <- subdata$diff[which.min(abs(subdata$date -  Sys.Date()))]
      abline(v=hoverdate, h = hoverdiff, lty=2)
    }
    
  }
  
  
}

plot_multiplot1 <- function(input, h2, hover){
  
  multi_input <- list()
  multi_input$symbol <- input$multi_symbol
  multi_input$dates <- input$multi_dates
  multi_input$showdates <- input$multi_showdates
  multi_input$scale <- input$multi_scale_plot1
  #multi_input$scale_plot2 <- input$multi_scale_plot2
  multi_input$transform <- input$multi_transform
  multi_input$index <- input$multi_index
  multi_input$differential <- input$multi_differential
  multi_input$indexscale <- input$multi_indexscale
  multi_input$num <- input$multi_num
  multi_input$connum <- input$multi_connum
  multi_input$checkGroup <- input$multi_checkGroup
  multi_input$hover <- input$multi_hover
  multi_input$diff_index <- input$multi_diff_index
  multi_input$diff_index_strategy <- input$multi_diff_index_strategy
  multi_input$useIndex1 <- input$multi_useIndex1
  
  
  subdata <- plot_pre(multi_input)
  
  subdata <- subdata[ (subdata$date > input$multi_showdates[1]) &
                        ( subdata$date < input$multi_showdates[2])  ,  ]
  
  colorset <- rainbow(length(indexname)+1)
  
  # price
  #if (input$index == "MACD"){
  if (  !( (substr(input$multi_index,1,3) %in% c("SMA","EMA","Sto") ) & 
           (input$multi_differential == "None") )  ){
    
    #subdata <- data[, c("date", "diff", "price") ]
    #colnames(subdata) <- c("Date", input$index, "Stock")
    #subdata$MACD_Stock <- subdata$MACD + subdata$Stock
    
    #print(subdata  %>% 
    #        gather(key,value, Stock , MACD_Stock)  %>%
    #        ggplot(aes(Date, value, colour=key)) +
    #        geom_line())
    
    plot(subdata$date, subdata$price, type="l", pch="*", xlab="", ylab="Price",
         ylim = c(min(subdata$diff + subdata$price, subdata$price, na.rm = TRUE),
                  max(subdata$diff + subdata$price, subdata$price, na.rm = TRUE) ),
         main=paste0(input$multi_symbol, " ", input$multi_scale_plot1, " with ", input$multi_index))
    lines(subdata$date, subdata$diff + subdata$price  , lty= 2, col=colorset[1]) 
    
    if (input$multi_strategy == "On" & (sum(abs(subdata$indicator), na.rm = TRUE) >0) ){
      for (i in 1: sum(abs(subdata$indicator), na.rm = TRUE) ){
        
        subind <- subdata[subdata$indicator %in% c(1,-1) ,]
        
        if (subind$indicator[i] == 1){
          abline(v= subind$date[i], lty=3, col="red")
          
        } else if (subind$indicator[i] == -1){
          
          abline(v= subind$date[i], lty=3, col="green")
          
        }
        
        
      }
      
    }
    
    
  } else {
    plot(subdata$date, subdata$price, type="l", pch="*", xlab="", ylab="Price",
         main=paste0(input$multi_symbol, " ", input$multi_scale_plot1, " with ", input$multi_index))
    lines(subdata$date, subdata$diff , lty= 2, col=colorset[1]) 
    
    if (input$multi_strategy == "On" & (sum(abs(subdata$indicator), na.rm = TRUE) >0) ){
      for (i in 1: sum(abs(subdata$indicator), na.rm = TRUE) ){
        
        subind <- subdata[subdata$indicator %in% c(1,-1) ,]
        
        if (subind$indicator[i] == 1){
          abline(v= subind$date[i], lty=3, col="red")
          
        } else if (subind$indicator[i] == -1){
          
          abline(v= subind$date[i], lty=3, col="green")
          
        }      
      }
      
    }
    
    
  }
  
  if (!is.null(input$multi_checkGroup)){
    
    for (i in 1: length(input$multi_checkGroup)){
      
      lines(subdata$date, subdata[, paste0("check",i)] , lty= 6, col=colorset[1+i]) 
      
    }
  }
  
  if (hover==TRUE){
    
    if (!is.null(h2$multi_hover$x)){
      #near1 <- nearPoints(subdata, input$hover,
      #xvar="date", yvar="price",threshold=20,maxpoints=1)
      hoverdate <- subdata$date[which.min(abs(subdata$date -  as.Date(h2$multi_hover$x)))]
      abline(v=hoverdate, lty=2)
    } else {
      hoverdate <- subdata$date[which.min(abs(subdata$date -  Sys.Date()))]
      abline(v=hoverdate, lty=2)
    }
  }
  
}

plot_multiplot2 <- function(input, h2, hover) {
  
  multi_input <- list()
  multi_input$symbol <- input$multi_symbol
  multi_input$dates <- input$multi_dates
  multi_input$showdates <- input$multi_showdates
  multi_input$scale <- input$multi_scale_plot2
  multi_input$transform <- input$multi_transform
  multi_input$index <- input$multi_index
  multi_input$differential <- input$multi_differential
  multi_input$indexscale <- input$multi_indexscale
  multi_input$num <- input$multi_num
  multi_input$connum <- input$multi_connum
  multi_input$checkGroup <- input$multi_checkGroup
  multi_input$hover <- input$multi_hover
  multi_input$diff_index <- input$multi_diff_index
  multi_input$diff_index_strategy <- input$multi_diff_index_strategy
  multi_input$useIndex1 <- input$multi_useIndex1
  
  
  subdata <- plot_pre(multi_input)    
  subdata <- subdata[ (subdata$date > input$multi_showdates[1]) &
                        ( subdata$date < input$multi_showdates[2])  ,  ]
  
  colorset <- rainbow(length(indexname)+1)
  
  # price
  #if (input$index == "MACD"){
  if (  !( (substr(input$multi_index,1,3) %in% c("SMA","EMA","Sto") ) & 
           (input$multi_differential == "None") )  ){
    
    #subdata <- data[, c("date", "diff", "price") ]
    #colnames(subdata) <- c("Date", input$index, "Stock")
    #subdata$MACD_Stock <- subdata$MACD + subdata$Stock
    
    #print(subdata  %>% 
    #        gather(key,value, Stock , MACD_Stock)  %>%
    #        ggplot(aes(Date, value, colour=key)) +
    #        geom_line())
    
    plot(subdata$date, subdata$price, type="l", pch="*", xlab="", ylab="Price",
         ylim = c(min(subdata$diff + subdata$price, subdata$price, na.rm = TRUE),
                  max(subdata$diff + subdata$price, subdata$price, na.rm = TRUE) ),
         main=paste0(input$multi_symbol, " ", input$multi_scale_plot2, " with ", input$multi_index))
    lines(subdata$date, subdata$diff + subdata$price  , lty= 2, col=colorset[1]) 
    
    if (input$multi_strategy == "On" & (sum(abs(subdata$indicator), na.rm = TRUE) >0) ){
      for (i in 1: sum(abs(subdata$indicator), na.rm = TRUE) ){
        
        subind <- subdata[subdata$indicator %in% c(1,-1) ,]
        
        if (subind$indicator[i] == 1){
          abline(v= subind$date[i], lty=3, col="red")
          
        } else if (subind$indicator[i] == -1){
          
          abline(v= subind$date[i], lty=3, col="green")
          
        }
        
        
      }
      
    }
    
    
  } else {
    plot(subdata$date, subdata$price, type="l", pch="*", xlab="", ylab="Price",
         main=paste0(input$multi_symbol, " ", input$multi_scale_plot2, " with ", input$multi_index))
    lines(subdata$date, subdata$diff , lty= 2, col=colorset[1]) 
    
    if (input$multi_strategy == "On" & (sum(abs(subdata$indicator), na.rm = TRUE) >0) ){
      for (i in 1:  sum(abs(subdata$indicator), na.rm = TRUE)  ){
        
        subind <- subdata[subdata$indicator %in% c(1,-1) ,]
        
        if (subind$indicator[i] == 1){
          abline(v= subind$date[i], lty=3, col="red")
          
        } else if (subind$indicator[i] == -1){
          
          abline(v= subind$date[i], lty=3, col="green")
          
        }      
      }
      
    }
    
    
    
  }
  
  if (!is.null(input$multi_checkGroup)){
    
    for (i in 1: length(input$multi_checkGroup)){
      
      lines(subdata$date, subdata[, paste0("check",i)] , lty= 6, col=colorset[1+i]) 
      
    }
  }
  
  if (hover==TRUE){
    
    if (!is.null(h2$multi_hover$x)){
      #near1 <- nearPoints(subdata, input$hover,
      #xvar="date", yvar="price",threshold=20,maxpoints=1)
      hoverdate <- subdata$date[which.min(abs(subdata$date -  as.Date(h2$multi_hover$x)))]
      abline(v=hoverdate, lty=2)
    } else {
      hoverdate <- subdata$date[which.min(abs(subdata$date -  Sys.Date()))]
      abline(v=hoverdate, lty=2)
    }
  }
  
}

plot_backtest <- function(input){
  
  tradedata <- list()
  
  test_input <- list()
  test_input$symbol <- input$test_stock 
  test_input$dates <- input$test_dates
  test_input$scale <- input$test_scale_plot1
  #test_input$scale_plot2 <- input$test_scale_plot2
  test_input$transform <- input$test_transform
  test_input$index <- input$test_index
  test_input$differential <- input$test_differential
  test_input$indexscale <- input$test_indexscale
  test_input$num <- input$test_num
  test_input$hover <- input$test_hover
  test_input$diff_index_strategy <- input$test_diff_index_strategy
  
  if (input$test_diff_index_strategy == "No" ) {
    test_input$useIndex1 <- "Stock"
    test_input$diff_index <- "None"
  } else {
    test_input$useIndex1 <- input$test_useIndex1
    test_input$diff_index <- input$test_diff_index
  }
  
  
  test_input$connum <- input$test_open
  subdata_open <- plot_pre(test_input)
  
  test_input$connum <- input$test_close
  subdata_close <- plot_pre(test_input)
  
  test_input$symbol <- input$test_target
  subdata_target <- plot_pre(test_input)
  
  # create open index
  subdata_open$open_indicator <- 2* subdata_open$indicator
  #levels(subdata_open$indicator) <- c("Short", "Keep", "Buy")
  #colnames(subdata_open)[ncol(subdata_open)] <- "open_indicator"
  
  # create close index
  subdata_close$close_indicator <- subdata_close$indicator
  #levels(subdata_close$indicator) <- c("Sell", "Keep", "Cover")
  #colnames(subdata_close)[ncol(subdata_close)] <- "close_indicator"
  
  # put all data together
  subdata <- cbind( subdata_open[,c("date","orig_price", "open_indicator")] ,
                    subdata_close[,"close_indicator"], 
                    subdata_target[,"orig_price"]   )
  colnames(subdata)[4:5] <- c("close_indicator", "target_price")
  
  # trading indicator
  subdata$indicator <- subdata$open_indicator + subdata$close_indicator
  
  # buy and sell signal
  buy_number <- sum( subdata$open_indicator == 2 , na.rm = TRUE)
  null_buy_indicator <- which(subdata$open_indicator == 2  )
  sell_indicator <- which(subdata$close_indicator == -1  )
  
  buy_indicator <- vector()
  
  for (i in 1:(length(sell_indicator) +1 )){
    
    subbuy <- ifelse( i==1,  subset(null_buy_indicator, null_buy_indicator < first( sell_indicator)),
                      ifelse( i== (length(sell_indicator)+1), subset(null_buy_indicator,null_buy_indicator >  last( sell_indicator)) , 
                              subset(null_buy_indicator,
                                     (null_buy_indicator < sell_indicator[i]) & (null_buy_indicator > sell_indicator[i-1]))))
    
    buy_indicator <- c(buy_indicator, subbuy)
    
  }
  
  if (sum(is.na(buy_indicator)) >0  ){
    buy_indicator <- buy_indicator[ -which(is.na(buy_indicator))]
  }
  
  
  # buy stock
  
  if (sell_indicator[1] < buy_indicator[1] ){
    allbuydata <- subdata[ 1: (sell_indicator[1]+1) ,]
    allbuydata$buypercent <- allbuydata$orig_price / allbuydata$orig_price[1]
    
    keepdata <- subdata[ (sell_indicator[1]+2) :  (buy_indicator[1]) ,] 
    keepdata$buypercent <- last(allbuydata$buypercent)
    allbuydata <- rbind(allbuydata,keepdata)
    
  } else {
    allbuydata <- subdata[ 1: buy_indicator[1] ,]
    allbuydata$buypercent <- allbuydata$orig_price / allbuydata$orig_price[1]
  }
  
  
  buy_list <- list()
  
  for (i in 1:length(buy_indicator)){
    
    sell_buy <- sell_indicator[first(which((sell_indicator > buy_indicator[i])  == TRUE))]
    
    if (is.na(sell_buy)){
      sell_buy <- nrow(subdata)
    }
    
    buydata <- subdata[ (buy_indicator[i]+1) : min((sell_buy+1), nrow(subdata))  ,] 
    
    
    buydata$buypercent <- buydata$orig_price / buydata$orig_price[1] 
    #ifelse(is.null(allbuydata$buypercent), 1, last(allbuydata$buypercent)) 
    
    if (nrow(buydata) > 2) {
      if (input$stoploss > 0){
        buydata$previousmax <- buydata$buypercent
        
        for (j in 2: nrow(buydata)){
          buydata$previousmax[j] <- max(buydata$buypercent[1:j] )
        }
        test_stoploss <- (buydata$buypercent < (buydata$previousmax * (1-input$stoploss/100)))
        if (sum(test_stoploss > 0)){
          stoploss_indicator <- first(which(test_stoploss))
          buydata$buypercent[ min( (stoploss_indicator+1), nrow(buydata) ) : nrow(buydata)] <- buydata$buypercent[min( (stoploss_indicator+1), nrow(buydata) )]
        }
        buydata <- buydata[, -which(colnames(buydata) == "previousmax")]
        
      }
      
    }
    

    attributes(buydata)$sell_buy <- sell_buy
    
    buy_list <- append(buy_list, list(buydata) )
    
    buydata$buypercent <- buydata$buypercent * last(allbuydata$buypercent)
    
    
    if ((sell_buy+2) < nrow(subdata)){
      
      if ( i < length(buy_indicator) ){
        keepdata <- subdata[ (sell_buy+2):  (buy_indicator[i+1])  ,] 
      } else {
        keepdata <- subdata[ (sell_buy+2):  nrow(subdata)  ,] 
      }
      
      keepdata$buypercent <- last(buydata$buypercent)
      allbuydata <- rbind(allbuydata, buydata,keepdata)
    } else {
      allbuydata <- rbind(allbuydata, buydata)
    }
    
  }
  
  tradedata$buy_list <- buy_list
  
  # short and cover signal
  short_number <- sum( subdata$open_indicator == -2 , na.rm = TRUE)
  null_short_indicator <- which(subdata$open_indicator == -2  )
  cover_indicator <- which(subdata$close_indicator == 1  )
  
  
  short_indicator <- vector()
  
  for (i in 1:(length(cover_indicator) +1 )){
    
    subshort <- ifelse( i==1,  subset(null_short_indicator, null_short_indicator < first( cover_indicator)),
                        ifelse( i== (length(cover_indicator)+1), subset(null_short_indicator,null_short_indicator >  last( cover_indicator)) , 
                                subset(null_short_indicator,
                                       (null_short_indicator < cover_indicator[i]) & (null_short_indicator > cover_indicator[i-1]))))
    
    short_indicator <- c(short_indicator, subshort)
    
  }
  
  if (sum(is.na(short_indicator)) >0  ){
    short_indicator <- short_indicator[ -which(is.na(short_indicator))]
  }
  
  
  # short stock
  # for short, assume no opening at the begining.
  allshortdata <- subdata[ 1: short_indicator[1] ,]
  allshortdata$shortpercent <- 1
  
  short_list <- list()
  
  for (i in 1:length(short_indicator)){
    
    cover_short <- cover_indicator[first(which((cover_indicator > short_indicator[i])  == TRUE))]
    
    if (is.na(cover_short)){
      cover_short <- nrow(subdata)
    }
    
    shortdata <- subdata[ (short_indicator[i]+1) : min((cover_short+1), nrow(subdata))  ,] 
    
    shortdata$shortpercent <- 2- shortdata$orig_price / shortdata$orig_price[1] 
    #ifelse(is.null(allshortdata$shortpercent), 1, last(allshortdata$shortpercent)) 
    
    if (nrow(shortdata) > 2) {
      if (input$stoploss > 0){
        shortdata$previousmax <- shortdata$shortpercent
        
        for (j in 2: nrow(shortdata)){
          shortdata$previousmax[j] <- max(shortdata$shortpercent[1:j] )
        }
        test_stoploss <- (shortdata$shortpercent < (shortdata$previousmax * (1-input$stoploss/100)))
        if (sum(test_stoploss > 0)){
          stoploss_indicator <- first(which(test_stoploss))
          shortdata$shortpercent[ min( (stoploss_indicator+1), nrow(shortdata) ) : nrow(shortdata)] <- shortdata$shortpercent[min( (stoploss_indicator+1), nrow(shortdata) )]
        }
        shortdata <- shortdata[, -which(colnames(shortdata) == "previousmax")]
        
      }
      
    }
    
    
    attributes(shortdata)$cover_short <- cover_short
    
    short_list <- append(short_list, list(shortdata))
    
    shortdata$shortpercent <- shortdata$shortpercent * last(allshortdata$shortpercent)
    
    if ( ((cover_short+2) < nrow(subdata))){
      
      if ( i < length(short_indicator) ){
        keepdata <- subdata[ (cover_short+2):  (short_indicator[i+1])  ,] 
      } else {
        keepdata <- subdata[ (cover_short+2):  nrow(subdata)  ,] 
      }
      
      keepdata$shortpercent <- last(shortdata$shortpercent)
      allshortdata <- rbind(allshortdata, shortdata,keepdata)
      
    } else {
      allshortdata <- rbind(allshortdata, shortdata)
    }
    
  }
  
  tradedata$short_list <- short_list
  
  if (input$test_direction == "Buy Only"){
    
    allbuydata$target_price <- allbuydata$target_price/allbuydata$target_price[1]
    
    tradedata$allbuydata <- allbuydata
    
    allbuydata$buypercent <- allbuydata$buypercent -1
    allbuydata$target_price <- allbuydata$target_price-1 
    tradedata$plot_backtest <- ggplot(allbuydata) + geom_line(aes(x = date, y = buypercent, color="Algorithm" ))+
      geom_line(aes(x = date, y = target_price, color="Benchmark" ))+ ylab("Return") +  
      annotate("text",x= max(allbuydata$date), y = 0,
               label = "By www.MinchunZhou.com", vjust=1, hjust=1)
    
    
  } else if (input$test_direction == "Short Only"){
    
    allshortdata$target_price <- allshortdata$target_price/allshortdata$target_price[1]
    tradedata$allshortdata <- allshortdata
    allshortdata$shortpercent <- allshortdata$shortpercent -1
    allshortdata$target_price <- allshortdata$target_price -1
    
    tradedata$plot_backtest <- ggplot(allshortdata) + geom_line(aes(x = date, y = shortpercent, color="Algorithm" ))+
      geom_line(aes(x = date, y = target_price, color="Benchmark" ))+ ylab("Return") +
      annotate("text",x= max(allshortdata$date), y = 0, label = "By www.MinchunZhou.com", vjust=1, hjust=1)
    
 
  } else if (input$test_direction == "Both"){
    
    # Both buy and short
    # assume to have opening at the begining 
    
    buyinfo <- data.frame( indicator = buy_indicator, type = "Buy", 
                           order = 1:length(buy_indicator)  )
    shortinfo <- data.frame(indicator =  short_indicator, type = "Short", 
                            order = 1:length(short_indicator))
    
    tradeinfo <- rbind(buyinfo, shortinfo)
    tradeinfo <- tradeinfo[ order(tradeinfo$indicator) ,]
    
    if (sell_indicator[1] < buy_indicator[1] ){
      alltradedata <- subdata[ 1: (sell_indicator[1]+1) ,]
      alltradedata$tradepercent <- alltradedata$orig_price / alltradedata$orig_price[1]
      
      keepdata <- subdata[ (sell_indicator[1]+2) :  (tradeinfo$indicator[1]) ,] 
      keepdata$tradepercent <- last(alltradedata$tradepercent)
      alltradedata <- rbind(alltradedata,keepdata)
      
    } else {
      alltradedata <- subdata[ 1: buy_indicator[1] ,]
      alltradedata$tradepercent <- alltradedata$orig_price / alltradedata$orig_price[1]
    }
    
    for (i in 1:nrow(tradeinfo)){
      
      if ( tradeinfo$type[i] == "Buy" ){
        
        buydata <- buy_list[[ tradeinfo$order[i] ]]
        colnames(buydata)[7] <-"tradepercent" 
        
        buydata$tradepercent <- buydata$tradepercent * last(alltradedata$tradepercent)
        
        
        sell_buy <- attributes(buy_list[[tradeinfo$order[i]]])$sell_buy
        
        if ((sell_buy+2) < nrow(subdata)){
          
          if ( i < length(tradeinfo$indicator) ){
            keepdata <- subdata[ (sell_buy+2):  (tradeinfo$indicator[i+1])  ,] 
          } else {
            keepdata <- subdata[ (sell_buy+2):  nrow(subdata)  ,] 
          }
          
          keepdata$tradepercent <- last(buydata$tradepercent)
          alltradedata <- rbind(alltradedata, buydata,keepdata)
        } else {
          alltradedata <- rbind(alltradedata, buydata)
        }
        
        
      } else if (tradeinfo$type[i] == "Short" ){
        
        
        shortdata <- short_list[[ tradeinfo$order[i] ]]
        colnames(shortdata)[7] <-"tradepercent" 
        
        shortdata$tradepercent <- shortdata$tradepercent * last(alltradedata$tradepercent)
        
        cover_short <- attributes(short_list[[tradeinfo$order[i] ]])$cover_short
        
        if ((cover_short+2) < nrow(subdata)){
          
          if ( i < length(tradeinfo$indicator) ){
            keepdata <- subdata[ (cover_short+2):  (tradeinfo$indicator[i+1])  ,] 
          } else {
            keepdata <- subdata[ (cover_short+2):  nrow(subdata)  ,] 
          }
          
          keepdata$tradepercent <- last(shortdata$tradepercent)
          alltradedata <- rbind(alltradedata, shortdata,keepdata)
        } else {
          alltradedata <- rbind(alltradedata, shortdata)
        }
        
        
      }
      
    }
    
    alltradedata$target_price <- alltradedata$target_price/alltradedata$target_price[1]
    tradedata$alltradedata <- alltradedata
    
    alltradedata$tradepercent <- alltradedata$tradepercent -1
    alltradedata$target_price <- alltradedata$target_price - 1
    tradedata$plot_backtest <- ggplot(alltradedata) + geom_line(aes(x = date, y = tradepercent, color="Algorithm" ))+
      geom_line(aes(x = date, y = target_price, color="Benchmark" )) + ylab("Return") +  
      annotate("text",x= max(alltradedata$date), y = 0, label = "By www.MinchunZhou.com", vjust=1, hjust=1)
    
    
  }
  
  return(tradedata)
}

table_stock <- function(input, h1){
  
  subdata <- plot_pre(input)
  subdata <- subdata[,c("date","price", "diff", "Consecutive", "diff_index")]
  
  # nearPoints(subdata, input$plot_click2, xvar="date", yvar="diff",threshold=20,maxpoints=1)
  #hoverdateindex <- which.min(abs(subdata$date -  as.Date(h1$hover1$x)))
  
  if (!is.null(h1$hover1$x)){
    #near1 <- nearPoints(subdata, input$hover,
    #xvar="date", yvar="price",threshold=20,maxpoints=1)
    hoverdateindex <- which.min(abs(subdata$date -  as.Date(h1$hover1$x)))
    
  } else {
    hoverdateindex <- which.min(abs(subdata$date -  Sys.Date()))
  }
  
  
  colnames(subdata)[1:5] <- c("Date", "Price","Value","Consecutive", "Diff's Index")
  subdata$Value <- round(subdata$Value,5)
  
  printout <- t(subdata[hoverdateindex,c("Date", "Price","Value","Consecutive","Diff's Index")])
  colnames(printout) <- "Value"
  rownames(printout) <- c("Date", "Stock Price","Index Value","Consecutive","Diff's Index")
  return(printout)
  
}

table_multiframe <- function(input, h2){
  
  multi_input <- list()
  multi_input$symbol <- input$multi_symbol
  multi_input$dates <- input$multi_dates
  multi_input$showdates <- input$multi_showdates
  multi_input$scale <- input$multi_scale_plot2
  multi_input$transform <- input$multi_transform
  multi_input$index <- input$multi_index
  multi_input$differential <- input$multi_differential
  multi_input$indexscale <- input$multi_indexscale
  multi_input$num <- input$multi_num
  multi_input$connum <- input$multi_connum
  multi_input$checkGroup <- input$multi_checkGroup
  multi_input$hover <- input$multi_hover
  multi_input$diff_index <- input$multi_diff_index
  multi_input$diff_index_strategy <- input$multi_diff_index_strategy
  multi_input$useIndex1 <- input$multi_useIndex1
  
  subdata <- plot_pre(multi_input)
  subdata <- subdata[,c("date","price", "diff","Consecutive")]
  
  
  
  # nearPoints(subdata, input$plot_click2, xvar="date", yvar="diff",threshold=20,maxpoints=1)
  
  #hoverdateindex <- which.min(abs(subdata$date -  as.Date(h1$hover1$x)))
  
  if (!is.null(h2$multi_hover$x)){
    #near1 <- nearPoints(subdata, input$hover,
    #xvar="date", yvar="price",threshold=20,maxpoints=1)
    hoverdateindex <- which.min(abs(subdata$date -  as.Date(h2$multi_hover$x)))
    
  } else {
    hoverdateindex <- which.min(abs(subdata$date -  Sys.Date()))
  }
  
  
  colnames(subdata)[1:3] <- c("Date", "Price","Value")
  subdata$Value <- round(subdata$Value,5)
  
  printout <- t(subdata[hoverdateindex,c("Date", "Price","Value","Consecutive")])
  colnames(printout) <- "Value"
  rownames(printout) <- c("Date", "Stock Price","Index Value","Consecutive")
  return(printout)
}

table_tradinghistory <- function(tradedata, input){
  
  # trading history
  if (input$test_direction == "Buy Only"){
    allbuydata <- tradedata$allbuydata
    table_output <- allbuydata[ (!is.na(allbuydata$indicator)) & (allbuydata$indicator %in% c(3, 2,1, -1, -2, -3) ) ,]
    table_output$indicator <- ifelse(table_output$open_indicator %in% 1:3 , "Buy", "Sell" )
    table_output <- table_output[ , c("date", "orig_price","indicator") ]
    
  } else if (input$test_direction == "Short Only"){ 
    allshortdata <- tradedata$allshortdata
    
    table_output <- allshortdata[ (!is.na(allshortdata$indicator)) & (allshortdata$indicator %in% c(3, 2,1, -1, -2, -3) ) ,]
    table_output$indicator <- ifelse(table_output$open_indicator %in% -(1:3), "Short", "Cover" )
    table_output <- table_output[ , c("date", "orig_price","indicator") ]
    
  } else if (input$test_direction == "Both"){
    alltradedata <- tradedata$alltradedata
    
    table_output <- alltradedata[ (!is.na(alltradedata$indicator)) & (alltradedata$indicator !=0 ) ,]
    
    signal <- c( "Short/Sell", "Short" , "Sell", "Cover", "Buy", "Cover/Buy" )
    signal_number <- c(-3:-1, 1:3)
    
    for (i in 1:6){
      table_output$indicator[table_output$indicator == signal_number[i] ] <- signal[i]
    }
    
    table_output <- table_output[ , c("date", "orig_price","indicator") ]
    
  }
  
  colnames(table_output) <- c("Date", "Price", "Signal")
  table_output$Price <-round(table_output$Price,2)
  unique(table_output)
}

table_tradinganalyse <- function(tradedata, input){
  
  winfun <- function(data){
    data[1,7] < last(data[,7])
  }
  
  
  if (input$test_direction == "Buy Only"){
    testdata <- tradedata$allbuydata
    colnames(testdata)[7] <- "percent"
    totalbuy <- length(tradedata$buy_list)
    
    winbuy <- sum(sapply(tradedata$buy_list, winfun) ) /totalbuy
    totalshort <- 0
    winshort <- 0
    
  } else if (input$test_direction == "Short Only"){ 
    testdata <- tradedata$allshortdata
    colnames(testdata)[7] <- "percent"
    totalshort <- length(tradedata$short_list)
    winshort <- sum(sapply(tradedata$short_list, winfun) ) /totalshort
    winbuy <- 0
    totalbuy <- 0
    
  } else if (input$test_direction == "Both"){
    testdata <- tradedata$alltradedata
    colnames(testdata)[7] <- "percent"
    totalbuy <- length(tradedata$buy_list)
    totalshort <- length(tradedata$short_list)
    winbuy <- sum(sapply(tradedata$buy_list, winfun) ) /totalbuy
    winshort <- sum(sapply(tradedata$short_list, winfun) ) /totalshort
    
  }
  
  teststat <-  c("Total Returns", "Annualized Returns","Volatility","Max Drawdown",
                 "Alpha", "Beta", "Sharpe","Sortino","Information Ratio", "Total Buy",
                 "Total Short")
  backtest_result <- as.data.frame(matrix(0, nrow = 2, ncol=length(teststat)))
  rownames(backtest_result) <- c("Strategy", input$test_target)
  colnames(backtest_result) <- teststat
  
  test_period_ind <- ifelse(input$test_scale_plot1 == "Daily", 250, 
                            ifelse(input$test_scale_plot1 == "Weekly", 52, 12))
  
  Rf <- input$test_rf/100
  testdata$rm  <- testdata$percent -1
  
  test_period <-nrow(testdata)
  
  testdata$target_percent <- testdata$target_price/testdata$target_price[1] 
  testdata$rm_target  <- testdata$target_percent-1
  
  total_return_strategy <- last(testdata$percent) 
  backtest_result$`Total Returns`[1] <- topercent(total_return_strategy)
  
  total_return_benchmark <- last(testdata$target_percent) 
  backtest_result$`Total Returns`[2] <- topercent(total_return_benchmark)
  
  annual_return_strategy <- total_return_strategy^(test_period_ind/test_period) - 1 
  backtest_result$`Annualized Returns`[1] <- topercent(annual_return_strategy)
  
  annual_return_benchmark <- (total_return_benchmark)^(test_period_ind/test_period) - 1 
  backtest_result$`Annualized Returns`[2] <- topercent(annual_return_benchmark)
  
  testdata$Dp <- testdata$orig_price -  Lag(testdata$orig_price, 1 )
  testdata$Dp[1] <- 0 
  
  testdata$Dm <- testdata$target_price -  Lag(testdata$target_price, 1 )
  testdata$Dm[1] <- 0 
  
  
  testdata$rp <- testdata$percent -  Lag(testdata$percent, 1 )
  testdata$rp[1] <- 0 
  
  testdata$rp_target <- testdata$target_percent -  Lag(testdata$target_percent,  1 )
  testdata$rp_target[1] <- 0 
  
  
  beta <- cov(testdata$percent,testdata$target_percent )/var(testdata$target_percent)
  backtest_result$Beta[1] <- beta
  
  alpha<-  annual_return_strategy - 
    (Rf  +  beta  * (annual_return_benchmark-0.04))
  backtest_result$Alpha[1] <- round(alpha, 4)
  
  
  Algorithm_Volatility <-  sqrt(test_period_ind/test_period * 
                                  sum( (testdata$rp - mean(testdata$rp))^2   ) )
  backtest_result$Volatility[1] <- Algorithm_Volatility
  
  benchmark_Volatility <-  sqrt(test_period_ind/test_period * 
                                  sum( (testdata$rp_target - mean(testdata$rp_target))^2   ) )
  backtest_result$Volatility[2] <- benchmark_Volatility
  
  strategy_maxdrawdown <- maxdrawdown(testdata$percent)
  backtest_result$`Max Drawdown`[1] <-topercent( strategy_maxdrawdown$maxdrawdown/
                                                   testdata$percent[strategy_maxdrawdown$from[1]])
  
  benchmark_maxdrawdown <- maxdrawdown(testdata$target_percent)
  backtest_result$`Max Drawdown`[2] <- topercent(benchmark_maxdrawdown$maxdrawdown/
                                                   testdata$percent[benchmark_maxdrawdown$from])
  
  testdata$percent_avg_i <-  cumsum(testdata$rm) / (1:nrow(testdata)-1)
  testdata$percent_avg_i[1] <- 0
  testdata$percent_ft <- as.numeric( testdata$rm < testdata$percent_avg_i)
  
  Downside_Risk <- sqrt(test_period_ind/test_period * 
                          sum( (testdata$rm - testdata$percent_avg_i)^2 * testdata$percent_ft ) )
  
  Sortino <- (annual_return_strategy - Rf)/Downside_Risk
  backtest_result$Sortino[1] <- Sortino
  
  ind_sharp <- (annual_return_strategy - Rf ) / Algorithm_Volatility
  backtest_result$Sharpe[1] <- ind_sharp 
  
  Information_Ratio <- (annual_return_strategy - annual_return_benchmark) / 
    sqrt(test_period_ind)/
    sd(testdata$percent - testdata$target_percent)
  
  backtest_result$`Information Ratio`[1] <-   round(Information_Ratio,2)
  backtest_result$`Information Ratio`[2] <- "Winning Percent"
  
  backtest_result$`Total Buy`[1] <- totalbuy
  backtest_result$`Total Buy`[2] <-topercent(winbuy)
  
  backtest_result$`Total Short`[1] <- totalshort
  backtest_result$`Total Short`[2] <- topercent(winshort)
  
  
  return(backtest_result)
  
}

#### test input function
test_plot_backtest <- function(input){
  input <- list()
  tradedata <- list()
  input$stoploss <- 0
  input$test_direction <-  "Buy Only"
  test_input <- list()
  test_input$symbol <- input$test_stock  <- "SPX"
  test_input$dates <- input$test_dates <- c("1950-01-03", "2017-12-22")
  test_input$scale <- input$test_scale_plot1 <- "Weekly"
  #test_input$scale_plot2 <- input$test_scale_plot2
  test_input$transform <- input$test_transform <- "Normal"
  test_input$index <- input$test_index <- "EMA10"
  test_input$differential <- input$test_differential <- "None"
  test_input$indexscale <- input$test_indexscale <- 10
  test_input$num <- input$test_num <- 0
  test_input$hover <- input$test_hover <- NULL
  test_input$diff_index_strategy <- input$test_diff_index_strategy <- "No"
  test_input$useIndex1 <- input$test_useIndex1 <- "Stock"
  test_input$diff_index <- input$test_diff_index <- "EMA40"
  test_input$connum <- input$test_open <- 1
  test_input$connum <- input$test_close <- 1
  test_input$symbol <- input$test_target <- "SPX"
  test_input$stoploss <- input$stoploss <- 0
  
  
  tradedata <- plot_backtest(input)
  
  dat <- tradedata$allbuydata
  dat <- dat[, c(1,2,5,6,7) ]
  dat$indicator <- as.factor(dat$indicator)
  levels(dat$indicator) <- c("Sell", "Hold", "Buy")
  
  write.csv(dat, "~/Desktop/Jimmy/weekly_EMA10_EMA40.csv")
  
  return(tradedata)
}