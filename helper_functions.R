
# some convenience functions ------------

{
  
  # model_l = arima_models[type == "arima" & lookback == 9]$model
  # arima_models[type == "arima" & lookback == 1]
  # arima_models[type == "arima" & lookback == 1 & transform == "log"]
  # model_l = model_stats[1, ]$model  
  # retrieve the p-value from a list containing one arima model
  model_l = models[64, ]$model[[1]]
  model_l = models[54, ]$model[[1]]
  
  get_p = function(model_l)
  {
    # fit = models[transform == "log" & type == "arima"][lb]$model[[1]]
    fit = model_l
    tab = coeftest(fit)
    p_col = which(grepl(pattern = "Pr\\(", colnames(tab)))
    p_row = which(grepl(pattern = "xreg", rownames(tab)))
    p_row = which(grepl("xreg", rownames(tab)))
    if(length(p_row) == 0)
      p_row = which(grepl("survival", rownames(tab)))
    return(tab[p_row, p_col])
  }
  
  # retrieve the regression coefficient from a list containing one arima model
  get_c = function(model_l)
  {
    fit = model_l
    coef_1 = coef(fit)
    if ("xreg" %in% names(coef_1)) 
      return(as.numeric(coef_1[which(grepl("xreg", names(coef_1)))]))
    return(as.numeric(coef_1[which(grepl("survival", names(coef_1)))]))
  }
  get_int = function(model_l)
  {
    fit = model_l
    coef_1 = coef(fit)
    if ("intercept" %in% names(coef_1)) 
      return (as.numeric(coef_1[which(grepl("intercept", names(coef_1)))]))
    return(NA)
  }
  
  # retrieve the sum of squared residuals from a list containing one arima model
  get_ssr = function(model_l)
  {
    fit = model_l[[1]]
    return(sum(residuals(fit) ^ 2))
  }
}
