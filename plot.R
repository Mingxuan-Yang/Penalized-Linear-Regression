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

mutatePower <- function(df, powers) {
  full_power <- sapply(colnames(df), function(x) powers[x])
  transformpower <- function(x, power) {
    if (is.na(power)) return (x)
    if (power == 0) return(log(x))
    else return (x^power)
  }
  transformname <- function(name, power){
    if (is.na(power) || power == 1) return (name)
    if (power == 0) return(paste0("log_", name))
    else return (paste0(name, "_pwr", power))
  }
  res <- bind_cols(map2(df, full_power, transformpower))
  colnames(res) <- map2(colnames(df), full_power, transformname)
  return (res)
}

reg <- function(df, formula = NULL, response = 1, predictors = -1, interactions = 1,
                model = c("Ridge", "Lasso"), lambda0 = exp(seq(-5, 20, by = 1)),
                powerTransform = numeric(0))
{
  ## df:  a data frame with covariates and response
  ## response: the number or the name of response
  ## predictor: the numbers or the names of predictors
  ## interactions: if `formula` is not defined, a positive integer n indicating the level
  ###              of multiway interactions should be included
  ## model: "Ridge" or "Lasso"
  ## lambda0: a sequence of shrinkage parameters to be considered
  
  get_formula <- function(name_y, name_x, power){
    x_str <-paste(name_x, collapse = " + ")
    if (power > 1)
      x_str <- paste0("(", x_str, ") ^ ", power)
    formula_str <- paste(name_y, "~" ,x_str, "- 1")
    return (as.formula(formula_str))
  }
  
  # test validity of parameters
  
  
  df <- df %>% mutatePower(powerTransform)
  # generate formula by interactions
  if (is.null(formula)){
    formula <- get_formula(colnames(df)[response],
                           colnames(df)[predictors],
                           interactions)
  }
  X <- model.matrix(formula, df)
  if ("(Intercept)" %in% colnames(X))
    X <- X[, -1]
  Y <- df[, response]
  X_scaled <- scale(X)
  Y_scaled <- scale(Y)
  
  model <- match.arg(model)
  
  ## Suppose model %in% c("Lasso", "Ridge")
  coef <- do.call(bind_rows,
    map(lambda0, 
        ~ c(glmnet(X_scaled, Y_scaled, lambda = .x, alpha = ifelse(model == "Lasso", 1, 0)) %>%
            coef() %>% .[-1] %>% set_names(colnames(X_scaled)), "lambda" = .x))
  )
  return(as.data.frame(coef))
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
hc_plot_returns_mem(reg(swiss, model = "Lasso"), "Lasso")
hc_plot_returns_mem(reg(swiss, interactions = 2, model = "Ridge"), "Ridge")
hc_plot_returns_mem(reg(swiss, formula = Agriculture ~ Education + log(Catholic), model = "Lasso"), "Lasso")
hc_plot_returns_mem(reg(swiss, model = "Lasso",
                        powerTransform = c(Education = 2, Catholic = 0)), "Lasso")
