backtestStrategy = function (portfolio, strategy, financialData){
  # Get the max start of each indicator
  startingValue = strategy$startingPoint()+1
  amountOfDataFromToday = strategy$amountOfDataFromToday()
  portfolio$strategy = strategy

  for (dateIndex in startingValue:nrow(financialData)) {
    dataInputForStrategy = financialData[(dateIndex-amountOfDataFromToday):dateIndex-1,]
    todayPrice = financialData$Price[dateIndex]
    todayDate = financialData$Date[dateIndex]
    # For Each Day Check if you can open or close an order
    #'[0) check if you can close orders that go in take profit or stop loss]

    portfolio$checkForOrdersToClose(todayPrice, todayDate)
    amountInPortfolio = portfolio$value

    if (amountInPortfolio <=0){
      break
    }
    #in the last day, close all orders that are open at the close price
    else if (dateIndex == length(financialData$Date)){
     portfolio$closeAllOrders(todayPrice, todayDate)
      break
    }
    else {
      #'[1) check what strategy says (buy/sell)]
      orderType = strategy$checkForSignals(dataInputForStrategy)
      
      if (orderType=="IDLE"){
        next
      }

      #'[2) check if there are orders open]
      orderResult = portfolio$checkForOpenOrders(orderType)
      ordersOpen = orderResult$ordersOpen
      orderIndex = orderResult$orderIndex
      #'[3) if no order is open, open the order]
      if (length(ordersOpen)==0){
        orderPlaced = createOrder(
          openPrice=todayPrice, 
          openDate=todayDate, 
          type=orderType
          )
        portfolio$addOrder(orderPlaced)
      }
    #'[4) check if the order is the same of the signal]
      else if (isOrderOfThisType(ordersOpen, orderType)){ # if true
    #'[5) hold the order - DO NOTHING]   
      } 
    
    #'[6) check if the order is the opposite of the signal]
     else if (!isOrderOfThisType(ordersOpen, orderType)){
      #'[7) close the opposite order]
      portfolio = portfolio$closeOrder(orderIndex,todayPrice, todayDate)
      #'[8) open new order]
      orderPlaced = createOrder(
        openPrice=todayPrice, 
        openDate=todayDate, 
        type=orderType)
      portfolio$addOrder(orderPlaced)
      }
    }
    }
      
    return (portfolio)
}



isOrderOfThisType =function(order,orderType){
    return (order$type == orderType)
  }

