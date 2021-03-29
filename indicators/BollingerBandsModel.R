BollingerBandsGenerator = setRefClass("BollingerBandsClass",
                                fields=c(
                                  periods = "numeric",
                                  amountOfDataFromToday = "numeric"
                                ),
                                methods = list(
                                  calculateBollingerBands=function(data){
                                    # Calculate TP
                                    TP = (data$Price+data$High+data$Low)/3
                                    # Calculate MA
                                    MA = mean(TP)
                                    # Calculate standard deviation
                                    SD=sd(TP)
                                    # Calculate Bands
                                    BOLU = MA+2*SD
                                    BOLD = MA-2*SD
                                    
                                    return(list("BOLU"=BOLU, "BOLD"=BOLD))
                                  }
                                )
)