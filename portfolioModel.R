portfolioClassGenerator = setRefClass("portfolioClass", 
                   fields=list(
                     value="numeric",
                     valueHistory="ANY",
                     orders="ANY",
                     analytics="ANY",
                     strategy="ANY"
                   ),
                   methods = list(
                       closeAllOrders = function(todayPrice, todayDate){
                         if (nrow(.self$orders)>0){
                           for (ordersIndex in 1: nrow(.self$orders)){
                             if (.self$orders[ordersIndex,]$status =="OPEN"){
                               #if(.self$orders[ordersIndex,]$openPrice==52.02){
                              # browser()
                               #}
                              .self$closeOrder(ordersIndex, todayPrice,todayDate)
                             }
                           }
                         }
                       },
                       
                      addOrder=function(order){
                         .self$value = .self$value - order$amount  
                        .self$orders = rbind(.self$orders, order)
                      },
                      
                      closeOrder=function(orderIndex, price, date){
                        orderToClose = .self$orders[orderIndex,]
                        orderClosed =  closeOrderByOrderObject(orderToClose, price, date)

                        .self$orders[orderIndex,]= orderClosed
                        netProfitLoss = .self$orders[orderIndex,]$profitLoss
                        porfolioValueBeforeOrderClose =.self$valueHistory$Value[length(.self$valueHistory$Value)]
                        #browser()
                        portfolioValue =porfolioValueBeforeOrderClose+netProfitLoss
                        .self$valueHistory = rbind(
                          .self$valueHistory, 
                          list("Date"=date,
                               "Value"=portfolioValue, 
                               "Change"=portfolioValue/porfolioValueBeforeOrderClose-1)
                          )
                        .self$value = portfolioValue
                      },
                      
                      checkForOrdersToClose=function(price, date){
                        if (nrow(.self$orders)>0){
                          for (ordersIndex in 1: nrow(.self$orders)){
                            #check for orders open
                            if (.self$orders[ordersIndex,]$status =="OPEN"){
                              if (.self$orders[ordersIndex,]$type == "SELL"){
                                # if the price is above the stop loss or below the takeprofit, close the order
                                if (price >= .self$orders[ordersIndex,]$stopLoss | 
                                    price <= .self$orders[ordersIndex,]$takeProfit){
                                  .self$closeOrder(ordersIndex, price, date);
                                }
                              }
                                else if (.self$orders[ordersIndex,]$type == "BUY"){
                                # if the price is below the stop loss or above the takeprofit, close the order
                                  if (price <= .self$orders[ordersIndex,]$stopLoss |
                                      price >= .self$orders[ordersIndex,]$takeProfit){
                                    .self$closeOrder(ordersIndex, price, date);
                                  }
                                }
                            }
                          }
                        }
                      },
                      
                      checkForOpenOrders=function(orderType){
                        ordersOpen= c()
                        orderIndex= c()
                        if (nrow(.self$orders)>0){
                          for (ordersIndex in 1: nrow(.self$orders)){
                            singleOrder = .self$orders[ordersIndex,];
                            if (singleOrder$type == orderType & singleOrder$status == "OPEN"){
                              ordersOpen= c(ordersOpen,singleOrder)
                              orderIndex= c(orderIndex,ordersIndex)
                            }
                          }
                        }
                        return(list("ordersOpen"=ordersOpen, "orderIndex"=orderIndex))
                      }
)
)


