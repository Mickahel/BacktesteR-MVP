# system and environment setup
rm(list=ls())
graphics.off()
Sys.setlocale(locale = "English")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Input options
pathOfFinancialData = ".\\inputCSVData\\S&P 500 Historical Data Reversed with missing data.csv"
#pathOfFinancialData = ".\\inputCSVData\\KO Historical Data.csv"
initialPortfolioValue = 10000
percentageOfPortfolioForEachInvestment=0.1
indicatorsParameters= c(8, 40, 60)
# Add paths
source("dataInputFunctions.R")
source("portfolioModel.R")
source("strategies/singleRSIStrategyModel.R")
source("strategies/bollingerBandsStrategyModel.R")
source("orderFunctions.R")
source("backtest.R")
source("analytics.R")
source("./indicators/RSIModel.R")
source("./indicators/BollingerBandsModel.R")
#  strategy
strategy = singleRSIStrategy$new() 
#strategy = bollingerBandsStrategy$new()
# data input from CSV, InvestingUSA
rawData = readDataFromCSV(pathOfFinancialData)
# Data validation
dataOutput = cleanAndValidateData(rawData)
cleanData = dataOutput$data
missingValues = dataOutput$missingValues
# Creation of the Portfolio
portfolio = portfolioClassGenerator$new(
  value=initialPortfolioValue, 
  orders = data.frame(),
  valueHistory = data.frame(
    Date=c(cleanData$Date[1]),
    Value=c(initialPortfolioValue),
    Change=c(NA)
    )
  )
# backtest
strategy$addIndicators(indicatorsParameters)
# analytics
portfolio = backtestStrategy(portfolio, strategy, cleanData)
plot(cleanData$Date, cleanData$Price, type="l", xlab="Date", ylab="Prices", main="Timeseries Prices")
plot(cleanData$Date, cleanData$Change,type="h", xlab="Date", ylab="Returns", main="Timeseries Returns")
abline(h=0, col="blue")
strategy$plotIndicators(cleanData)
portfolio$value
interest = (portfolio$value/initialPortfolioValue-1)
interest
standardDeviation = sd(portfolio$valueHistory$Value)
standardDeviation
plot(portfolio$valueHistory$Date,portfolio$valueHistory$Value, type="l", xlab="Date", ylab="Value", main="Equity Line")
abline(h=initialPortfolioValue, col="red")
plotDrawdown(portfolio$valueHistory, initialPortfolioValue)
plotRollingSharpe(portfolio$valueHistory,cleanData, initialPortfolioValue)
portfolio$orders$openDate = as.Date(portfolio$orders$openDate, origin="1970-01-01")
portfolio$orders$closeDate= as.Date(portfolio$orders$closeDate, origin="1970-01-01")
portfolio$orders
amountOfOrders = nrow(portfolio$orders)
amountOfOrders
amountOfBuyOrdersPercentage = sum(portfolio$orders$type=="BUY")/amountOfOrders
amountOfBuyOrdersPercentage
amountOfSellOrdersPercetage = sum(portfolio$orders$type=="SELL")/amountOfOrders
amountOfSellOrdersPercetage
missingValues
#output
write.table(portfolio$orders, file = ".\\output\\orders.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ",", row.names=FALSE, col.names = TRUE)
write.table(portfolio$valueHistory, file = ".\\output\\historyValue.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ",", row.names=FALSE, col.names = TRUE)


