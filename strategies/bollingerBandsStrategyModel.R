bollingerBandsStrategy = setRefClass("bollingerBandsStrategy",
                                fields = list(
                                  BollingerBands="ANY"
                                ),
                                methods=list(
                                  addIndicators = function(indicatorsParameters){
                                    .self$BollingerBands = BollingerBandsGenerator$new(
                                      periods = indicatorsParameters[1],
                                      amountOfDataFromToday = indicatorsParameters[1]
                                    )
                                  },
                                  checkForSignals = function(data){
                                    lastPrice = tail(data, n=1)
                                    bbValue = .self$BollingerBands$calculateBollingerBands(data);
                                    #if < lowerband buy
                                    if (lastPrice$Price < bbValue$BOLD){
                                      return("BUY")
                                    }
                                    #if > upperband, sell
                                    else if (lastPrice$Price > bbValue$BOLU){
                                      return("SELL")
                                    }
                                    else{    
                                      return("IDLE")
                                    }
                                  },
                                  startingPoint = function(){
                                    return(.self$BollingerBands$amountOfDataFromToday)
                                  },
                                  amountOfDataFromToday = function(){
                                    return(.self$BollingerBands$amountOfDataFromToday)
                                  },
                                  plotIndicators = function(cleanData){
                                    BBValuesU = c()
                                    BBValuesD = c()
                                    startingValue = .self$startingPoint()+1
                                    amountOfDataFromToday = .self$amountOfDataFromToday()
                                    for(dateIndex in startingValue:nrow(cleanData)){
                                      dataInputForStrategy = cleanData[(dateIndex-amountOfDataFromToday):dateIndex-1,]
                                      BBValue = .self$BollingerBands$calculateBollingerBands(dataInputForStrategy)
                                      BBValuesU= c(BBValuesU, BBValue$BOLU)
                                      BBValuesD= c(BBValuesD, BBValue$BOLD)
                                    }
                                    plot(cleanData$Date,cleanData$Price, type="l",xlab="Date",ylab="Timeseries Values", main="Bollinger Bands Indicator from Starting Point")
                                    lines(cleanData$Date,c(rep(NA, length.out=startingValue-1), BBValuesU), col="red", type="l",)
                                    lines(cleanData$Date,c(rep(NA, length.out=startingValue-1), BBValuesD),col="green",type="l")
                                    
                                    }
                                )
                                )