readDataFromCSV = function(path){
  data = read.table(
    file = path, 
    sep=",", 
    dec=".", 
    header = T, 
    na.strings= c("","-"),
    col.names = c( "Date",      "Price",  "Open",   "High",   "Low", "Vol", "Change"), 
    #colClasses = c("character", "double", "character", "character", "character","character", "character"),
    #stringsAsFactors = T
    )
  
  data  = data[c("Date", "Price",   "Open",   "High",   "Low","Change")]
  return(data)
}


cleanAndValidateData = function(data){
  # index the data frame with rownames
  missingValues = sapply(
    rawData, 
    FUN = function(x)sum(is.na(x))
    )
  data = na.omit(data)

  data$Date = as.Date(data$Date, format = "%b %d, %Y")
  data$Price = as.double(gsub(",","",data$Price))
  data$High = as.double(gsub(",","",data$High))
  data$Low = as.double(gsub(",","",data$Low))
  data$High = as.double(gsub(",","",data$High))
  data$Change = as.double(gsub("%","",data$Change))
  data = data[order(data$Date),]
  rownames(data) = 1:nrow(data)
  #data$Price = as.numeric(data$Price)
 # data$Change = as.numeric(data$Change)
  return (list("data" = data, "missingValues"=missingValues))
}