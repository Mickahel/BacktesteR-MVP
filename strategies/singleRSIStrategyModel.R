singleRSIStrategy = setRefClass("singleRSIStrategy",
                                fields = list(
                                  RSI="ANY"
                                ),
                                methods=list(
                                  addIndicators = function(indicatorsParameters){
                                    .self$RSI = RSIClassGenerator$new(
                                      periods = indicatorsParameters[1],
                                      amountOfDataFromToday = indicatorsParameters[1],
                                      lowerBand = indicatorsParameters[2],
                                      upperBand = indicatorsParameters[3]
                                    )
                                  },
                                  checkForSignals = function(data){
                                    #get value of the RSI
                                    rsiValue = .self$RSI$calculateRSI(data);
                                    #if < lowerband buy
                                    if (rsiValue < .self$RSI$lowerBand){
                                      return("BUY")
                                    }
                                    #if > upperband, sell
                                    else if (rsiValue > .self$RSI$upperBand){
                                      return("SELL")
                                    }
                                    else{    
                                      return("IDLE")
                                    }
                                  },
                                  startingPoint = function(){
                                    return(.self$RSI$amountOfDataFromToday)
                                  },
                                  amountOfDataFromToday = function(){
                                    return(.self$RSI$amountOfDataFromToday)
                                  },
                                  plotIndicators = function(cleanData){
                                    rsiValues = c()
                                    startingValue = .self$startingPoint()+1
                                    amountOfDataFromToday = .self$amountOfDataFromToday()
                                    for(dateIndex in startingValue:nrow(cleanData)){
                                      dataInputForStrategy = cleanData[(dateIndex-amountOfDataFromToday):dateIndex-1,]
                                      rsiValue = .self$RSI$calculateRSI(dataInputForStrategy)
                                      rsiValues= c(rsiValues, rsiValue)
                                    }
                                    plot(cleanData$Date,c(seq(0, 0, length.out=startingValue-1), rsiValues), xlab="Date",ylab="RSI Value", type="l", ylim=c(1,100), main="RSI Indicator from Starting Point")
                                    abline(h=.self$RSI$upperBand, col="blue")
                                    abline(h=.self$RSI$lowerBand, col="blue") 
                                  }
                                )
                                )