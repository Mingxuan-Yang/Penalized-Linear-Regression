library(highcharter)
library(dplyr)
library(stringr)
library(glmnet)

#### to do list:
# 1. should be able to extend the range of lambda
# 2. add interactions checks: nrow(x) > ncol(x)
# 3. should be able to adjust the range of lambda
# 4. needs to improve xlab

mutate.inter <- function(xx, interactions)
{
  ## got this idea from https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables
  if(interactions == 1)
  {x = xx}
  else if(interactions == 2)
    x = cbind(xx, do.call(cbind, combn(colnames(xx), 2, FUN= function(x)
      list(setNames(data.frame(xx[,x[1]]*xx[,x[2]]),
                    paste(x, collapse=":")) ))))
  else if(interactions == 3)
  {x = cbind(xx, do.call(cbind, combn(colnames(xx), 2, FUN= function(x)
    list(setNames(data.frame(xx[,x[1]]*xx[,x[2]]),
                  paste(x, collapse=":")) ))), 
    do.call(cbind, combn(colnames(xx), 3, FUN= function(x)
                    list(setNames(data.frame(xx[,x[1]]*xx[,x[2]]*xx[,x[3]]),
                                  paste(x, collapse=":")) ))))}
  else if(interactions == 4)
  {x = cbind(xx, do.call(cbind, combn(colnames(xx), 2, FUN= function(x)
    list(setNames(data.frame(xx[,x[1]]*xx[,x[2]]),
                  paste(x, collapse=":")) ))), 
    do.call(cbind, combn(colnames(xx), 3, FUN= function(x)
                    list(setNames(data.frame(xx[,x[1]]*xx[,x[2]]*xx[,x[3]]),
                                  paste(x, collapse=":")) ))), 
    do.call(cbind, combn(colnames(xx), 4, FUN= function(x)
                                    list(setNames(data.frame(xx[,x[1]]*xx[,x[2]]*xx[,x[3]]*xx[,x[4]]),
                                                  paste(x, collapse=":")) ))))}
  else
  {
    warning("We only capture 4-way interactions")
    x = mutate.inter(xx, interactions = 4)
  }
  return(x)
}

reg <- function(df, response = 1, model, interactions = 1, lambda0 = exp(seq(-5, 20, by = 1)))
{
  ## df:  a data frame contains covariates and response
  ## response: which col is the response
  ## model: "Ridge" or "Lasso"
  ## interactions: default 0 for no interactions; 2 for all 2-way interactions;
  ###              3 for all 3-way interactions; 4 for all 4-way interactions
  ## lambda0: a sequence of shrinkage parameters to be considered
  ncol <- dim(df)[2]
  coef <- data.frame()
  y <- df[, response]
  y <- scale(y)
  xx <- df[, -response]
  xx <- scale(xx)
  x <- mutate.inter(xx, interactions)
  for(i in 1:length(lambda0)){
    coef[i, 1:ncol] <- c(coef(glmnet(as.matrix(x), y, lambda = lambda0[i],
                                     alpha = as.numeric(model == "Lasso")))[-1],
                         lambda0[i])
  }
  colnames(coef) <- c(colnames(x), "lambda")
  return(coef)
}


###### function to create an interface
hc_plot_returns <- function(coef, name){
  ## coef is the objective data set
  ## name is "Ridge" or "Lasso"
  ## lambda is a sequence of assigned shrinkage parameters, should be positive
  
  hc_plot <- highchart(type = "chart") %>%
    hc_xAxis(categories = coef$lambda,
             title = list(text = "lambda")) %>%
    hc_yAxis(title = list(text = "coefficient")) %>%
    hc_title(
      text = str_c("Coefficients of <span style=\"color:#e5b13a\"> ", name, "</span> regression"),
      style = list(fontWeight = "bold", useHTML = TRUE),
      align = "center") %>%
    hc_tooltip(borderWidth = 1, table = TRUE, sort = TRUE, 
               valueDecimals = 4, crosshairs = T)
  
  for(i in 1:(dim(coef)[2] - 1)){
    hc_plot <- hc_plot %>%
      hc_add_series(name = colnames(coef)[i], data = coef[,i])
  }
  return(hc_plot)
}

hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)

### test
swiss <- datasets::swiss
hc_plot_returns_mem(reg(swiss, model = "Ridge"), "Ridge")
