RSIClassGenerator = setRefClass("RSIClass",
                                 fields=c(
                                   periods = "numeric",
                                   amountOfDataFromToday = "numeric",
                                   upperBand = "numeric",
                                   lowerBand = "numeric"
                                 ),
                                 methods = list(
                                   calculateRSI=function(data){
                                     #browser()
                                     returns = data$Change
                                     if (sum(returns>=0)==0) {
                                     upReturns = 0
                                     }
                                     else{
                                       upReturns = returns[returns>=0]
                                     }
                                     
                                     if (sum(returns<0)==0){
                                      downReturns = 0
                                     }
                                     else{
                                       downReturns = returns[returns<0];
                                     }
                                     AvgU= mean(upReturns);
                                     AvgD = mean(abs(downReturns));
                                     RS = AvgU / AvgD;
                                     
                                     RSI = 100-100/( 1 + RS );
                                     return(RSI)
                                   }
                                   )
                                 )