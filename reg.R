library(highcharter)
library(dplyr)
library(purrr)
library(stringr)
library(glmnet)
library(ggplot2)

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
  if (is.null(formula) || formula == ""){
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
  
  func_predict <- function(newx, lambda, ...){
    tmp <- predict(glmnet(X_scaled, Y_scaled, lambda = lambda, alpha = ifelse(model == "Lasso", 1, 0)),
                   newx = newx, type = "response", ...)
    return (transformpower(
      tmp * attr(Y_scaled, "scaled:scale") + attr(Y_scaled, "scaled:center"),
      power_response
    ))
  }
  
  cv <- cv.glmnet(X_scaled, Y_scaled)
  cv_predict <- function(newx, ...){
    predict(cv, newx, ...)
  }
  
  penalty_func_list <- list(
    "Lasso" = function(x) sum(abs(x), na.rm = T),
    "Ridge" = function(x) sqrt(sum(x^2, na.rm = T))
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
  
  info <- function(i, j){
    Xs <- as.matrix(X_scaled)[, c(i,j), drop = F]
    Xo <- as.matrix(X_scaled)[, -c(i,j), drop = F]
    function(lambda){
      coefs <- glmnet(X_scaled, Y_scaled, lambda = lambda, alpha = ifelse(model == "Lasso", 1, 0)) %>%
        coef() %>% .[-1] %>% set_names(colnames(X_scaled))
      bs <- coefs[c(i,j)]
      bo <- coefs[-c(i,j)]
      bc <- solve(t(Xs) %*% (Xs)) %*% t(Xs) %*% (as.matrix(Y_scaled) - Xo %*% bo)
      RSS <- sum((Y_scaled - Xo %*% bo - Xs %*% bs)^2)
      k <- RSS - sum((as.matrix(Y_scaled) - Xo %*% bo - Xs %*% bc)^2)
      eigens <- eigen(t(Xs) %*% Xs, symmetric = T)
      ellipse.res <- list(
        xc = bc[1],
        yc = bc[2],
        a = sqrt(k/eigens$values[2]),
        b = sqrt(k/eigens$values[1]),
        phi = acos(eigens$vectors[1,2] * sign(eigens$vectors[2,2]) / sqrt(sum(eigens$vectors[,2]^2)))
      )
      class(ellipse.res) <- "ellipse"
      t <- penalty_func(bs)
      res <- list(ellipse = ellipse.res, tangent_point = bs, t = t, model = model)
      class(res) <- "regvarinfo"
      return (res)
    }
  }
  
  res <- list(
    formula = formula,
    response = colnames(df)[response],
    predictors = colnames(df)[predictors],
    model = model,
    coef = as.data.frame(coef),
    cv = cv,
    cv.coef = coef(cv),
    ols = ols,
    lambda = lambda0, t = t, t_ols = t_ols,
    X = X, Y = Y,
    X.scale = X_scaled, Y.scale = Y_scaled,
    fun.predict = func_predict,
    cv.predict = cv_predict,
    fitted = fitted, fitted_ols = fitted_ols,
    RSS = RSS, RSS_ols = RSS_ols,
    info = info
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
  cat("\nCoefficients using cross validation\n")
  cat("lambda: ", reg_result$cv$lambda.1se, "\n")
  res <- reg_result$cv.coef[-1]
  names(res) <- colnames(reg_result$coef)
  print(res)
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

plot.reg <- function(reg_result, which = 1, x_axis = c("log-lambda", "prop"), plot = T){
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
  if (plot){
    hc_plot_returns_mem <- memoise::memoise(hc_plot_returns)
    hc_plot_returns_mem(reg_result$coef, reg_result$lambda, reg_result$t/reg_result$t_ols, reg_result$model)
  }
  return (hc_plot_returns(reg_result$coef, reg_result$lambda, reg_result$t/reg_result$t_ols, reg_result$model))
}

predict.reg <- function(reg_result, newx, lambda = NULL, log = F, ...){
  if (is.null(lambda)){
    warning("No lambda specified, using cross validation.")
    return (reg_result$cv.predict(newx))
  }
  reg_result$fun.predict(newx, ifelse(log, exp(lambda), lambda), ...)
}

plot.ellipse <- function(ellipse, n = 1, plot = T, ...){
  t <- seq(0, 2*pi, 0.01)
  f <- function(k){
    x <- ellipse$xc + ellipse$a*k/n*cos(t)*cos(ellipse$phi) - ellipse$b*k/n*sin(t)*sin(ellipse$phi)
    y <- ellipse$yc + ellipse$a*k/n*cos(t)*sin(ellipse$phi) + ellipse$b*k/n*sin(t)*cos(ellipse$phi)
    data.frame(k = k, x = x, y = y)
  }
  data <- bind_rows(lapply(1:n, f))
  if (plot){
    print(
      ggplot(data) + 
        geom_point(aes(x = x, y = y, col = factor(k)), show.legend = FALSE, ...) +
        geom_point(data = data.frame(x = ellipse$xc, y = ellipse$yc), aes(x, y))
    )
  }
  return (data)
}

getCircle <- function(radius, npoints = 1000){
  tt <- seq(0, 2*pi, length.out = npoints)
  xx <- radius * cos(tt)
  yy <- radius * sin(tt)
  data <- data.frame(x = xx, y = yy)
  return(data)
}

getSquare <- function(side, npoints = 1000){
  z <- seq(-side, side, length = npoints)
  w <- abs(side - abs(z))
  z <- c(z, z)
  w <- c(w, -w)
  data <- data.frame(x = z, y = w)
  return(data)
}

plot.regvarinfo <- function(info, xlabel = "input$x", ylabel = "input$y"){
  data.ellipse <- plot(info$ellipse, n = 3, plot = F)
  tmp <- info$tangent_point
  data.tp <- data.frame(x = tmp[1], y = tmp[2])
  if (info$model == "Lasso")
    data.restriction <- getSquare(info$t)
  else
    data.restriction <- getCircle(info$t)
  gp <- ggplot() +
    geom_polygon(data = data.restriction, aes(x, y), fill = "#C0C0C0", alpha = 0.5) +
    geom_path(data = data.ellipse, aes(x, y, col = as.factor(k)), show.legend = F) +
    geom_point(aes(x = info$ellipse$xc, y = info$ellipse$yc), size = 2) +
    #geom_text(aes(x = info$ellipse$xc * 0.85, y = info$ellipse$yc * 0.85, label = "beta_OLS")) +
    geom_point(data = data.tp, aes(x,y), size = 2) +
    geom_point(aes(x = 0, y = 0), size = 2, col = "purple") +
    labs(x = xlabel, y = ylabel) +
    coord_fixed() +
    theme_bw(base_size = 15)
  print(gp)
}
