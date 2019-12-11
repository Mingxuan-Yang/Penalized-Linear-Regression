library(highcharter)
library(dplyr)
library(purrr)
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

reg <- function(df, formula = NULL, response = 1, predictors = -1, interactions = 0,
                model = c("Ridge", "Lasso"),  lambda0 = exp(seq(-5, 20, by = 1)))
{
  ## df:  a data frame with covariates and response
  ## response: the number or the name of response
  ## predictor: the numbers or the names of predictors
  ## interactions: if `formula` is not defined, a non-negative integer n indicating the level
  ###              of multiway interactions should be included
  ## model: "Ridge" or "Lasso"
  ## lambda0: a sequence of shrinkage parameters to be considered
  
  get_formula <- function(name_y, name_x, power){
    x_str <-paste(name_x, collapse = " + ")
    if (power > 0)
      x_str <- paste0("(", x_str, ") ^ ", power)
    formula_str <- paste(name_y, "~" ,x_str, "- 1")
    return (as.formula(formula_str))
  }
  
  # test validity of parameters
  
  
  # generate formula by interactions
  if (is.null(formula)){
    formula <- get_formula(colnames(df)[response],
                           colnames(df)[predictors],
                           interactions)
  }
  print(formula)
  X <- model.matrix(formula, df)
  if ("(Intercept)" %in% colnames(X))
    X <- X %>% select(-`(Intercept)`)
  Y <- df[, response]
  X_scaled <- scale(X)
  Y_scaled <- scale(Y)
  
  model <- match.arg(model)
  
  ## Suppose model %in% c("Lasso", "Ridge")
  coef <- do.call(bind_rows,
    map(lambda0, 
        ~ glmnet(X_scaled, Y_scaled, lambda = .x, alpha = ifelse(model == "Lasso", 1, 0)) %>%
          coef() %>% .[-1] %>% set_names(colnames(X_scaled)))
  )
  print(coef)
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
