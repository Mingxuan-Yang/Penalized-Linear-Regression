library(highcharter)
library(dplyr)
library(stringr)

# helper function to add line
# data is the objective data set, name is "Ridge" or "Lasso"
# lambda is the last column of data set
hc_plot_returns <- function(data, name){
  hc_plot <- highchart(type = "chart") %>% 
    hc_xAxis(categories = data$lambda,
             title = list(text = "lambda")) %>%
    hc_yAxis(title = list(text = "coefficient")) %>%
    hc_title(
      text = str_c("Coefficients of <span style=\"color:#e5b13a\"> ", name, "</span> regression"),
      style = list(fontWeight = "bold", useHTML = TRUE),
      align = "center") %>% 
    hc_tooltip(borderWidth = 1, 
               table = TRUE, 
               sort = TRUE,
               valueDecimals = 4,
               crosshairs = TRUE
               )
  
  for(i in 1:(dim(data)[2] - 1)){
    hc_plot <- hc_plot %>% 
      hc_add_series(name = colnames(data)[i], data = data[,i])
  }
  
  return(hc_plot)
}

hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)

# example: hc_plot_returns_mem(data, "Lasso")
hc_plot_returns_mem(coef, "Lasso")
