createOrder = function(openPrice, openDate, type){
  riskRewardRatio = (0.01)/(0.005)
  amount = portfolio$value*percentageOfPortfolioForEachInvestment #3% of portfolio invested
  status = "OPEN"
  if (type =="BUY"){
    takeProfit = openPrice + openPrice*0.006
    stopLoss = openPrice - openPrice*0.003
  }
  else if (type=="SELL"){
    takeProfit = openPrice - openPrice*0.006
    stopLoss = openPrice + openPrice*0.003
  }

  return (
    list(
    "openPrice"=openPrice,
    "openDate"=openDate,
    "closePrice"=NA,
    "closeDate"=NA,
    "type"=type,
    "riskRewardRatio"=riskRewardRatio,
    "amount"=amount,
    "status"="OPEN",
    "takeProfit"=takeProfit,
    "stopLoss"=stopLoss,
    "profitLoss"=NA
    )
  )
}



closeOrderByOrderObject = function (order, closePrice, closeDate){
  order$closePrice = closePrice
  order$status = "CLOSED"
  order$closeDate = closeDate
  if (order$type == "BUY"){
    order$profitLoss= order$amount * ((closePrice-order$openPrice)/order$openPrice)
  }
  else if (order$type == "SELL"){
    order$profitLoss= order$amount * ((order$openPrice-closePrice)/order$openPrice)
  }

 return(order)
}

