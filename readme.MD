# Objective
The objective of the project is to create a simple **backtester** that is able to test the performance of trading systems, given as an input a `CSV` downloaded from [Investing USA](https://www.investing.com/).


## The project, in Practice 
In this project, a simple backtesting engine has been created and a simple strategy based on [Relative Strength Indicator](https://www.investopedia.com/terms/r/rsi.asp) has been used as a practical example to show the effectiveness of the backtester.

Regarding the RSI Strategy, it relies on simple Technical Analysis and on the concept of over-sold and over-bought: the strategy will stay in `IDLE` when the indicator is between the upper and the lower boundaries, while it will recommend to `BUY` or `SELL` when it will be respectively below the lower level and  above the upper one.

## Notes on the Project
1. It's written in vanilla R language, external packages has not been used as I aimed to code everything from scratch instead of using pre-built functions
2. I have made extensive use of Object Oriented Programming in R Language (i.e. `Indicators`, `Portfolio` and `Strategies` are Objects, while `Orders` are rows in a `data.frame` due to motivations that will be explained in the following sections)

# Architecture Description
The Architectural design behind this project is based on Object Oriented Programming, therefore, as already said, some components of the project are Objects created by their respective Classes.

The main reason behind this choice is the need for entities from which I could be able to call attributes (variables associated to the entity) and methods.
For instance, regarding the portfolio, I wanted to have an entity that represented the portfolio itself and from which I could have the value of it, but also functions such as `addOrder` or `closeOrder`.

Object Oriented Programming is substantiated by the creation of classes, which in are are declared using the function `setRefClass()` which creates an S4 class (an updated and more complete version of the S3 classes as it provides the possibility to create custom functions and use the object itself inside the function).

Thanks to the simple reproducibility of objects due to classes (in fact, they can be considered as blueprints of objects), I have decided to adopt this data type also for `Indicators` and `Strategies` for scalability reasons and for the motivations already stated.

## Note on architectural decision on Order Entity

The `Order` Entity, which represents a buy or sell order, is not an object although it would be eligible.
The motivation is that in R Language, Objects can be stored in vectors, but they instantly lose their parameters and methods as shown below.

```r
gen <- setRefClass("myRefClass", 
                   fields=list(aa="character"),
                   methods=list(
                     hi= function(){
                       cat("hi")
                     }
                   )
)
x <- gen$new(aa="nome")
y <- gen$new(aa="bana")
a<-c(x,y)

b<-a[1]
b$aa # NULL
```

```
## NULL
```

```r
#b$hi() throws an error
a[1]$aa # NULL
```

```
## NULL
```

Nonetheless, the main idea was to store all the orders inside a vector of the portfolio.
In order to work around the problem, it has been treated as a simple row in a data.frame.


# How to Start the Project

The project is already set up and it's possible to run it without any change: run the `init.R` code line by line or as a whole.

If, instead, it is needed to change the input parameters, it is mandatory to:

1. download historical data from [Investing USA](https://www.investing.com/) in CSV, such as [Apple Stock](https://www.investing.com/equities/apple-computer-inc-historical-data)
2. change in the file `init.R` the input parameters 
  a.`pathOfFinancialData`, which is the path of the `.CSV`. Note: you can use both relative and absolute path. 
  b. `initialPortfolioValue`, which is the amount of money in the portfolio at the beginning of the simulation.
  c. `percentageOfPortfolioForEachInvestment`, which is the percentage of the portfolio which is invested in each trade.
  d. `indicatorsParameters`: the parameters chosen for the strategy, that are in this case respectively: the periods of the RSI, the lower bound and the upper 60
 
An example is provided below:


```r
path = ".\\inputCSVData\\S&P 500 Historical Data Reversed with missing data.csv"
# or using absolute path
# path = "C:\\Users\\Michelangelo\\Desktop\\Projects\\BacktesteR-MVP\\inputCSVData\\S&P 500 Historical Data Reversed with missing data.csv"
initialPortfolioValue = 10000
percentageOfPortfolioForEachInvestment=0.1

indicatorsParameters= c(8, 40, 60)
```

After having the parameters set, start the code in `init.R` line-by-line or as a whole.

## Errors in starting the code

If you are trying to run the code of `init.R` and R throws errors while reading the `source("file.r")` such as:

```r
 source("dataInputFunctions.R")
Error in file(filename, "r", encoding = encoding) : 
  non posso aprire questa connessione
Inoltre: Warning message:
In file(filename, "r", encoding = encoding) :
  cannot open file 'dataInputFunctions.R': No such file or directory
```
then try to set the working directory as the one of the project with `setwd(folder)` manually, such as:

```r
setwd("C:\\Users\\Michelangelo\\Desktop\\Projects\\BacktesteR-MVP")
```
# Backtester Description

In this section, it will be shown how the code works under the hood and will be explained the logical steps of the backtester starting from the first line of `init.R`.

## Initial Setup

A new RSI Strategy Object is created from the `singleRSIStrategy` Class after setting the environment and the input paramenters.

It is important to not that the English language using `Sys.setlocale(locale = "English")` in order to parse the dates present inside the CSV.


```r
rm(list=ls())
graphics.off()
Sys.setlocale(locale = "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Input options
pathOfFinancialData = ".\\inputCSVData\\S&P 500 Historical Data Reversed with missing data.csv"
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

strategy = singleRSIStrategy$new(); 
```

Then, the process of data retrieving and cleaning starts:
1. The CSV parsed and stored into a `data.frame`
2. The data is then cleaned from missing values( if in the row is present a `NA` value, the row is removed) and is sorted by date
3. Finally, the data is indexed


```r
rawData = readDataFromCSV(pathOfFinancialData)
dataOutput = cleanAndValidateData(rawData)
cleanData = dataOutput$data
missingValues = dataOutput$missingValues
missingValues
```

```
##   Date  Price   Open   High    Low Change 
##      0      3      0      0      0      0
```

Moreover, the Portfolio is created with its initial values; valueHistory is a `data.frame` where the chages of the portfolio value are stored.


```r
portfolio = portfolioClassGenerator$new(
  value=initialPortfolioValue, 
  orders = data.frame(),
  valueHistory = data.frame(
    Date=c(cleanData$Date[1]),
    Value=c(initialPortfolioValue),
    Change=c(NA)
    )
  );
```

The last step before starting the backtest consists in adding the parameters of the RSI to the strategy, in fact, the objective of the function `addIndicators(values)` is to create a RSI object (with input parameters the ones that has been chosen before) as a variable inside the strategy and use it to produce signals.


```r
strategy$addIndicators(indicatorsParameters)
```

## Backtest
The backtest starts and the portfolio is returned.
In this phase, orders and signals are computed as it will be explained in the following sections.

```r
portfolio = backtestStrategy(portfolio, strategy, cleanData)
```

## Output and Analytics

In this section, a series of output, metrics and analytics is provided

Plot of price and returns of the timeseries analyzed:

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

RSI Indicator:

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Portfolio value at the end of the backtest, interest and standard deviation:


```r
portfolio$value
```

```
## [1] 10009.79
```

```r
interest = (portfolio$value/initialPortfolioValue-1)
interest
```

```
## [1] 0.0009791586
```

```r
standardDeviation = sd(portfolio$valueHistory$Value)
standardDeviation
```

```
## [1] 15.10555
```

Changes in portfolio value:


```r
portfolio$valueHistory
```

```
##          Date     Value        Change
## 1  2020-07-30 10000.000            NA
## 2  2020-08-21  9995.514 -0.0004485525
## 3  2020-08-26  9985.323 -0.0010195666
## 4  2020-08-28  9976.919 -0.0008416865
## 5  2020-09-02  9961.588 -0.0015365857
## 6  2020-09-03  9996.579  0.0035125836
## 7  2020-09-04 10004.710  0.0008132999
## 8  2020-09-08  9976.940 -0.0027756379
## 9  2020-09-09  9997.039  0.0020145025
## 10 2020-09-10  9979.459 -0.0017584791
## 11 2020-09-14  9992.714  0.0013281664
## 12 2020-09-17  9984.878 -0.0007840900
## 13 2020-09-18  9973.713 -0.0011182570
## 14 2020-09-25  9989.647  0.0015976763
## 15 2020-09-28 10005.741  0.0016110549
## 16 2020-10-19 10021.948  0.0016197098
## 17 2020-10-30 10009.792 -0.0012129506
```

Equity Line:

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

Drawdown:

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

Rolling sharpe Ratio:

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

calculated in a simpler way by assuming the risk free rate equals to 0

$$
RollingSharpe_t = \frac{Interest_t}{\sigma_t}
$$

Portfolio Orders:


```r
portfolio$orders$openDate = as.Date(portfolio$orders$openDate, origin="1970-01-01")
portfolio$orders$closeDate= as.Date(portfolio$orders$closeDate, origin="1970-01-01")
portfolio$orders
```

```
##    openPrice   openDate closePrice  closeDate type
## 1    3381.99 2020-08-17    3397.16 2020-08-21 SELL
## 2    3443.62 2020-08-25    3478.73 2020-08-26 SELL
## 3    3478.73 2020-08-26    3508.01 2020-08-28 SELL
## 4    3526.65 2020-09-01    3580.84 2020-09-02 SELL
## 5    3580.84 2020-09-02    3455.06 2020-09-03 SELL
## 6    3455.06 2020-09-03    3426.96 2020-09-04 SELL
## 7    3426.96 2020-09-04    3331.84 2020-09-08  BUY
## 8    3331.84 2020-09-08    3398.96 2020-09-09  BUY
## 9    3398.96 2020-09-09    3339.19 2020-09-10  BUY
## 10   3339.19 2020-09-10    3383.54 2020-09-14  BUY
## 11   3383.54 2020-09-14    3357.01 2020-09-17  BUY
## 12   3357.01 2020-09-17    3319.47 2020-09-18  BUY
## 13   3246.59 2020-09-24    3298.46 2020-09-25  BUY
## 14   3298.46 2020-09-25    3351.60 2020-09-28  BUY
## 15   3483.34 2020-10-15    3426.92 2020-10-19 SELL
## 16   3310.11 2020-10-29    3269.96 2020-10-30  BUY
##    riskRewardRatio    amount status takeProfit stopLoss
## 1                2 1000.0000 CLOSED   3361.698 3392.136
## 2                2  999.5514 CLOSED   3422.958 3453.951
## 3                2  998.5323 CLOSED   3457.858 3489.166
## 4                2  997.6919 CLOSED   3505.490 3537.230
## 5                2  996.1588 CLOSED   3559.355 3591.583
## 6                2  999.6579 CLOSED   3434.330 3465.425
## 7                2 1000.4710 CLOSED   3447.522 3416.679
## 8                2  997.6940 CLOSED   3351.831 3321.844
## 9                2  999.7039 CLOSED   3419.354 3388.763
## 10               2  997.9459 CLOSED   3359.225 3329.172
## 11               2  999.2714 CLOSED   3403.841 3373.389
## 12               2  998.4878 CLOSED   3377.152 3346.939
## 13               2  997.3713 CLOSED   3266.070 3236.850
## 14               2  998.9647 CLOSED   3318.251 3288.565
## 15               2 1000.5741 CLOSED   3462.440 3493.790
## 16               2 1002.1948 CLOSED   3329.971 3300.180
##    profitLoss
## 1   -4.485525
## 2  -10.191093
## 3   -8.404512
## 4  -15.330391
## 5   34.990913
## 6    8.130217
## 7  -27.769451
## 8   20.098571
## 9  -17.579583
## 10  13.254382
## 11  -7.835187
## 12 -11.165660
## 13  15.934765
## 14  16.093870
## 15  16.206397
## 16 -12.156128
```

Amount of Orders:


```r
amountOfOrders = nrow(portfolio$orders)
amountOfOrders
```

```
## [1] 16
```
Amount Of Buy Orders Percentage:

```r
amountOfBuyOrdersPercentage = sum(portfolio$orders$type=="BUY")/amountOfOrders
amountOfBuyOrdersPercentage
```

```
## [1] 0.5625
```
Amount Of Sell Orders Percentage:

```r
amountOfSellOrdersPercetage = sum(portfolio$orders$type=="SELL")/amountOfOrders
amountOfSellOrdersPercetage
```

```
## [1] 0.4375
```
Missing Values in Data

```r
missingValues
```

```
##   Date  Price   Open   High    Low Change 
##      0      3      0      0      0      0
```

The output in CSV are skipped.

## Code Explainations

### A note on Strategy Entity and RSI Entity
It is important to understand the structure and the usefulness of the Strategy entity.
This entity has the purpose, as in normal Trading systems, of checking for signals and inform about the amount of data(starting the count from the day before the day considered) needed by the indicator to compute those signals.
In fact, RSI indicator, as well as most of all Technical indicators, need a certain amount of data to create a meaningful result and not errors(for instance, an indicator that expects 14 parameters, if provided with just 6, would throw an error or an unfaithful result).


$$
RSI Value = 100-\frac{100}{1+\frac{Average Gain}{Average Loss}}
$$
Regarding RSI Entity, it's important,when computing its formula, to take into account the event by which negative or positive returns are not available for the data (maybe due to a strong uptrend or downtrend).
In order to deal with this problem, the value 0 will be assigned to the potential void variable.

```r
#[...Code...]
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
#[...Code...]
```

### Backtester Logic

![Chart of Backtester Logic Figure](figure/chart-of-backtester.png)

The logic of the backtester is shown in the chart above.
It is, on a basic level, a FOR cycle that runs from the `startingPoint` of the strategy.
The `backtestStrategy(portfolio, strategy, financialData)` function is shown below.

```r
startingValue = strategy$startingPoint()+1
  amountOfDataFromToday = strategy$amountOfDataFromToday()
  portfolio$strategy = strategy

  for (dateIndex in startingValue:nrow(financialData)) {
    dataInputForStrategy = financialData[(dateIndex-amountOfDataFromToday):dateIndex-1,]
    todayPrice = financialData$Price[dateIndex]
    todayDate = financialData$Date[dateIndex]
```

The backster then will check for orders to close and check whether the portfolio has still some money inside and if not, it will stop. If it is the last day of the trading strategy, the OPEN orders will be closed and the code will stop:

```r
    portfolio$checkForOrdersToClose(todayPrice, todayDate)
    amountInPortfolio = portfolio$value

    if (amountInPortfolio <=0){
      break
    }
    else if (dateIndex == length(financialData$Date)){
     portfolio$closeAllOrders(todayPrice, todayDate)
    }
```

Moreover, if it is not the last day of trading and the portfolio has still a positive value, the Backtester will check for signals. If the signal is IDLE, which means that the strategy suggests to not trade, the backtester will move to the next trading day.
On the contrary, if there is a BUY or SELL order, the Backtester will check whether there are open orders.
If there are not open orders, it will create an order and add it to the orders of the portfolio.
If the strategy gives a signal that is the same of the open order, then the position will be kept in hold (for instance, the strategy tells to BUY and there is a BUY open order).


```r
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
```
Lastly, if the order has a type contrary to the one suggested by the signals, the order will be closed and a new order coherent with the signal will be opened

```r
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
```


# Scaling with New Strategies and Indicators
The project is made to scale: if there is the need to add new Strategies or Indicators, it will be easy as the above mentioned entities are `Objects of Classes`.

In order to create new indicators, it will be needed to create a class with the following mandatory parameters and methods: `amountOfDataFromToday`

In order to create new Strategies, it will be needed to create a class with the following mandatory parameters and methods:
`addIndicators()`, `checkForSignals()`, `startingPoint()`, `amountOfDataFromToday()`,
`plotIndicators()`

## An Example With Bollinger Bands


A practical example consists in the creation of a new strategy: the classical Bollinger Bands strategy witht the same Historical Data.
The bollinger bands have as input parameter 3 periods.



Plot of price and returns of the timeseries analyzed:

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-2.png)

Bollinger Bands Indicator:

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

Portfolio value at the end of the backtest, interest and standard deviation:


```r
portfolio$value
```

```
## [1] 10030.09
```

```r
interest = (portfolio$value/initialPortfolioValue-1)
interest
```

```
## [1] 0.00300914
```

```r
standardDeviation = sd(portfolio$valueHistory$Value)
standardDeviation
```

```
## [1] 10.88415
```

Equity Line:

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

Drawdown:

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

Rolling sharpe Ratio:

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

