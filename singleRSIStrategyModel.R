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
                                  }
                                )
                                )