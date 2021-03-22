plotRSI = function(inputParameters, financialData){
  RSIIndicator =  RSIClassGenerator$new(
    periods = indicatorsParameters[1],
    amountOfDataFromToday = indicatorsParameters[1],
    lowerBand = indicatorsParameters[2],
    upperBand = indicatorsParameters[3]
  )
  rsiValues = c()
  
  startingValue = RSIIndicator$amountOfDataFromToday+1
  amountOfDataFromToday = RSIIndicator$amountOfDataFromToday
  for(dateIndex in startingValue:nrow(financialData)){
    dataInputForStrategy = financialData[(dateIndex-amountOfDataFromToday):dateIndex-1,]
    rsiValue = RSIIndicator$calculateRSI(dataInputForStrategy)
    rsiValues= c(rsiValues, rsiValue)
  }

   plot(financialData$Date[startingValue:length(financialData$Date)],rsiValues, xlab="Date", type="l", ylim=c(1,100))
   abline(h=RSIIndicator$upperBand, col="blue")
   abline(h=RSIIndicator$lowerBand, col="blue")
}