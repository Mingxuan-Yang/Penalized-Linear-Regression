library(jsonlite)
library(readr)
library(datasets)

url <- "https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY&symbol=MSFT&apikey=3E3W64XJ589OAWJ7"
stock_list <- read_json(url)[[2]]
data <- do.call("rbind", stock_list)
stock_data <- data.frame(matrix(unlist(data), ncol = 5))
colnames(stock_data) <- c("open", "high", "low", "close", "volume")

# change variable type
stock_data[,1:5] <- apply(stock_data[,1:5], 2, function(x){
  return(as.double(as.character(x)))
})

# save result
write_csv(stock_data,'Data/monthly_stock.csv')
write_csv(iris, 'Data/iris.csv')
write_csv(swiss, 'Data/swiss.csv')
