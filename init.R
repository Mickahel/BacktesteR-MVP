# system and environement setup
rm(list=ls())
graphics.off()
Sys.setlocale(locale = "English")

# Input options
#path = ".\\inputCSVData\\S&P 500 Historical Data Reversed with missing data.csv"
path = ".\\inputCSVData\\AAPL Historical Data.csv"
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
rawData = readDataFromCSV(path)


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
plotRSI(indicatorsParameters, cleanData)
portfolio$value
portfolio$orders
portfolio$valueHistory
missingValues
# Analysis of the data of the backtest
# TODO portfolio$analytics = analyticsModel(portfolio);


# output excel

# output PDF


