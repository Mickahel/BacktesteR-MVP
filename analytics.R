plotDrawdown= function(portf, initialPortfolioValue){
  values = lapply(portf$Value, function(x){
    return(ifelse(x>=initialPortfolioValue,1,x/initialPortfolioValue))
  }) 
  
  plot(portf$Date, values,type="l", xlab="Date", ylab="Drawdown %", main="Drawdown")
}


plotRollingSharpe = function(data,initialPortfolioValue){
  values = c()
  for (dateIndex in 2:length(data$Value)){
    d = data$Value[2:dateIndex]
    d2 = data$Value[dateIndex]
    r = (d2-initialPortfolioValue)/initialPortfolioValue
    sd =  sd(d)   
    values = c(values, r/sd)
  }
  plot(data$Date[2:length(data$Date)], values,type="l", xlab="Date", ylab="Sharpe value", main="Rolling Sharpe")
  abline(h=0, col="blue")
}
