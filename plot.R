library(highcharter)
library(dplyr)
library(purrr)
library(stringr)
library(glmnet)

#### to do list:
# 3. should be able to adjust the range of lambda ::(What's the difference between 1 and 3?)
## a scroll bar, so our app will show the plots for lambda within this range

##### --- Bo added:
# 1. needs to plot the ellipse and restriction area with a certain lambda value
# 2. needs cross validation to help determine the best lambda value
# 3. should add a function to help make predictions (interval and point estimate)
# 4. I've made it possible to make power transformations. Maybe some suggestion can be given to users.

transformpower <- function(x, power) {
  if (is.na(power)) return (x)
  if (power == 0) return(log(x))
  if (power == Inf) return (exp(x))
  else return (x^power)
}

mutatePower <- function(df, powers) {
  full_power <- sapply(colnames(df), function(x) powers[x])
  transformname <- function(name, power){
    if (is.na(power) || power == 1) return (name)
    if (power == 0) return(paste0("log_", name))
    if (power == Inf) return (paste0("exp_", name))
    else return (paste0(name, "_pwr", power))
  }
  res <- bind_cols(map2(df, full_power, transformpower))
  colnames(res) <- map2(colnames(df), full_power, transformname)
  return (res)
}

reg <- function(df, formula = NULL, response = 1, predictors = -1, interactions = 1,
                model = c("Ridge", "Lasso"), lambda0 = exp(seq(-10, 10, length.out = 300)),
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
  
  org_Y <- df[, response]
  power_response <- 1 / powerTransform[colnames(df)[response]]
  if (is.na(power_response))
    power_response <- 1
  if (is.character(response))
    response <- which(colnames(df) == response)
  if (is.character(predictors))
    predictors <- which(colnames(df) == predictors)
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
  coef <- do.call(rbind,
    map(lambda0, 
        ~ c(glmnet(X_scaled, Y_scaled, lambda = .x, alpha = ifelse(model == "Lasso", 1, 0)) %>%
            coef() %>% .[-1] %>% set_names(colnames(X_scaled))))
  )
  
  func_predict <- function(new_x, lambda, ...){
    tmp <- predict(glmnet(X_scaled, Y_scaled, lambda = lambda, alpha = ifelse(model == "Lasso", 1, 0)),
                   newx = new_x, type = "response", ...)
    return (transformpower(
      tmp * attr(Y_scaled, "scaled:scale") + attr(Y_scaled, "scaled:center"),
      power_response
    ))
  }
  
  penalty_func_list <- list(
    "Lasso" = function(x) sum(abs(x)),
    "Ridge" = function(x) sqrt(sum(x^2))
  )
  penalty_func <- penalty_func_list[[model]]
  
  ols <- lm(Y_scaled ~ X_scaled - 1)
  t <- apply(coef, 1, penalty_func)
  t_ols <- penalty_func(ols$coefficients)
  fitted <- transformpower(
    X_scaled %*% t(coef) * attr(Y_scaled, "scaled:scale") + attr(Y_scaled, "scaled:center"),
    power_response
  )
  fitted_ols <- transformpower(
    ols$fitted.values * attr(Y_scaled, "scaled:scale") + attr(Y_scaled, "scaled:center"),
    power_response
  )
  
  RSS <- apply(fitted, 2, function(x) sum((org_Y - x)^2))
  RSS_ols <- sum((org_Y - fitted_ols) ^ 2)
  res <- list(
    formula = formula,
    response = colnames(df)[response],
    predictors = colnames(df)[predictors],
    model = model,
    coef = as.data.frame(coef),
    ols = ols,
    lambda = lambda0, t = t, t_ols = t_ols,
    X = X, Y = Y,
    X.scale = X_scaled, Y.scale = Y_scaled,
    fun.predict = func_predict,
    fitted = fitted, fitted_ols = fitted_ols,
    RSS = RSS, RSS_ols = RSS_ols
  )
  class(res) <- c("reg", "list")
  return (res)
}

print.reg <- function(reg_result, nShow = 5){
  cat("Model Fitted Using", reg_result$model, "\n\n")
  cat("Formula:", paste(reg_result$formula[2], reg_result$formula[3], sep=' ~ '), "\n\n")
  nRow <- nrow(reg_result$coef)
  nSep <- max(floor(nRow / (nShow - 1)) - 1, 1)
  shown <-(0:(nRow - 1)) %% nSep == 0
  cat("OLS regression and", sum(shown), "regression results are\n")
  print(bind_cols(lambda = c(0, reg_result$lambda),
                  rbind(reg_result$ols$coefficients,
                        reg_result$coef))[c(T, shown), ])
}

summary.reg <- function(reg_result, nShow = Inf){
  nRow <- nrow(reg_result$coef)
  nSep <- max(floor(nRow / (nShow - 1)) - 1, 1)
  shown <- (0:(nRow - 1)) %% nSep == 0
  cat("OLS regression and", sum(shown), "regression results are\n")
  bind_cols(lambda = c(0,reg_result$lambda),
            param_prop = c(1,reg_result$t / reg_result$t_ols),
            rbind(reg_result$ols$coefficients,
                  reg_result$coef),
            RSS = c(reg_result$RSS_ols, reg_result$RSS))[c(T, shown), ]
}

plot.reg <- function(reg_result, which = 1, x_axis = c("log-lambda", "prop")){
  x_axis <- match.arg(x_axis)
  
  ###### function to create an interface
  hc_plot_returns <- function(coef, lambda, prop, name){
    ## coef is the objective data set
    ## name is "Ridge" or "Lasso"
    ## lambda is a sequence of assigned shrinkage parameters, should be positive
    if (x_axis == "log-lambda")
      x_var <- round(log(lambda), 4)
    else if (x_axis == "prop")
      x_var <- round(prop, 4)
    hc_plot <- highchart(type = "chart") %>%
      hc_xAxis(categories = x_var,
               title = list(text = x_axis)) %>%
      hc_yAxis(title = list(text = "coefficient")) %>%
      hc_title(
        text = str_c("Coefficients of <span style=\"color:#e5b13a\"> ", name, "</span> regression"),
        style = list(fontWeight = "bold", useHTML = TRUE),
        align = "center") %>%
      hc_tooltip(borderWidth = 1, table = TRUE, sort = TRUE, 
                 valueDecimals = 4, crosshairs = T)
    
    for(i in 1:ncol(coef)){
      hc_plot <- hc_plot %>%
        hc_add_series(name = colnames(coef)[i], data = coef[,i])
    }
    return(hc_plot)
  }
  hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)
  hc_plot_returns_mem(reg_result$coef, reg_result$lambda, reg_result$t/reg_result$t_ols, reg_result$model)
}

predict.reg <- function(reg_result, new_x, lambda, ...){
  reg_result$fun.predict(new_x, lambda, ...)
}

### test
swiss <- datasets::swiss 
plot(reg(swiss, model = "Lasso"), x_axis = "log-lambda")
plot(reg(swiss, interactions = 2, model = "Ridge"), x_axis = "p")
plot(reg(swiss, formula = Agriculture ~ Education + log(Catholic), model = "Lasso"), x_axis = "p")
plot(reg(swiss, model = "Lasso", powerTransform = c(Education = 2, Catholic = 0)), x_axis =  "l")

reg(swiss, model = "Lasso") -> tmp
print(tmp)
summary(tmp)
