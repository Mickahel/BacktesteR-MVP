# system and environement setup
rm(list=ls())
graphics.off()
Sys.setlocale(locale = "English")

# Input options
pathOfFinancialData = ".\\inputCSVData\\S&P 500 Historical Data Reversed with missing data.csv"
#path = ".\\inputCSVData\\AAPL Historical Data.csv"
initialPortfolioValue = 10000
percentageOfPortfolioForEachInvestment=0.1

#indicatorsParameters= c(8, 40, 65)
indicatorsParameters= c(8, 40, 60)

# Add paths
source("dataInputFunctions.R")
source("portfolioModel.R")
source("singleRSIStrategyModel.R")
source("orderFunctions.R")
source("backtest.R")
source("analytics.R")
source("./indicators/RSIModel.R")

#  strategy
strategy = singleRSIStrategy$new(); 
# data input from CSV, InvestingUSA
rawData = readDataFromCSV(pathOfFinancialData)


# Data validation
dataOutput = cleanAndValidateData(rawData)
cleanData = dataOutput$data
missingValues = dataOutput$missingValues


#plot(cleanData$Date,cleanData$Price, type="l")
# import strategies


# Creation of the Portfolio
portfolio = portfolioClassGenerator$new(
  value=initialPortfolioValue, 
  orders = data.frame(),
  valueHistory = data.frame(
    Date=c(cleanData$Date[1]),
    Value=c(initialPortfolioValue),
    Change=c(NA)
    )
  );



# backtest
strategy$addIndicators(indicatorsParameters)


portfolio = backtestStrategy(portfolio, strategy, cleanData)
plot(cleanData$Date, cleanData$Price, type="l", xlab="Date", ylab="Prices", main="Timeseries Prices")
plot(cleanData$Date, cleanData$Change, type="l", xlab="Date", ylab="Returns", main="Timeseries Returns")
abline(h=0, col="blue")
plotRSI(indicatorsParameters, cleanData)
portfolio$value
interest = (portfolio$value/initialPortfolioValue-1)
standardDeviation = sd(portfolio$valueHistory$Value)
plot(portfolio$valueHistory$Date,portfolio$valueHistory$Value, type="l", xlab="Date", ylab="Value", main="Equity Line")
abline(h=initialPortfolioValue, col="red")
portfolio$orders$openDate = as.Date(a$openDate, origin="1970-01-01")
portfolio$orders$closeDate= as.Date(a$closeDate, origin="1970-01-01")
portfolio$orders
amountOfOrders = nrow(portfolio$orders)

amountOfBuyOrdersPercentage = sum(portfolio$orders$type=="BUY")/amountOfOrders
amountOfSellOrdersPercetage = sum(portfolio$orders$type=="SELL")/amountOfOrders
missingValues


#output
write.csv(portfolio$orders,".\\output\\orders.csv", row.names = TRUE)
write.csv(portfolio$valueHistory,".\\output\\historyValue.csv", row.names = TRUE)




