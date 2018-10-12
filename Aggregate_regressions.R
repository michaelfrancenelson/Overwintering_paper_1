# packages, data ------------
{
  source("Globals.R")
  source("helper_functions.R")
  require(lmtest)
  require(data.table)
  require(grid)
  require(gridExtra)
  require(gtable)
  require(ggplot2)
  kill_survival_agg = fread(paste0(local_dat_dir, "pine_range_national_forest_survival_kill_aggregated.csv"))
  
}



# Total study area ----
# intersection of lodgepole and ponderosa species ranges with nationa forests
study_area = readOGR(paste0(spatial_data_output_dir, "pine_range_polygons_national_forests"),
layer = "pine_range_polygons_national_forests")

area_sq_m = rgeos::gArea(study_area)
area_sq_m / (1000 * 1000)
506823
# Temporal autocorrelation of means: significance tests ------------------------------------------------

# Alternative hypotheses are stationarity, i.e. not autocorrelated.
# High p-values indicate autocorrelation
# We're only really interested in type 1 (we don't have enough data points to accurately determine the other types...)
aTSA::adf.test(kill_survival_agg[lookback == 1, kill])
aTSA::adf.test(kill_survival_agg[lookback == 16, log(kill)])
# Original, and log-transformed have autocorrelation.

# Data table of model fits: lm and lm with ARIMA errors ------------------------------------------------------------
kill_survival_agg[, survival100 := 100 * survival]
# Create models (doesn't take long):
models = rbind(
  kill_survival_agg[ , .(model = list(forecast::Arima(kill, order = c(0, 0, 0), xreg = survival100)), type = "lm", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::Arima(log(kill), order = c(0, 0, 0), xreg = survival100)), type = "lm", transform = "log"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::auto.arima(kill, xreg = survival100)), type = "auto", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::auto.arima(log(kill), xreg = survival100)), type = "auto", transform = "log"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::Arima(kill, order = c(1, 0, 0), xreg = survival100)), type = "ar1", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::Arima(log(kill), order = c(1, 0, 0), xreg = survival100)), type = "ar1", transform = "log"), by = "lookback"]
)
models$AIC = sapply(models$model, function(x) AIC(x))
models$RMSE = sapply(models$model, function(x) forecast::accuracy(x)[1, "RMSE"])
# Make sure lookback is a numeric variable
models[, lookback := as.numeric(lookback)]
kill_survival_agg[, lookback:= as.numeric(lookback)]

# Data table of residuals, in melted format ----------
model_residuals = models[, .(fitted_vals = fitted.values(model[[1]]), residuals = residuals(model[[1]]), year = 1997:2010), by = list(lookback, type, transform)]
model_residuals = merge(kill_survival_agg, model_residuals)

kill_survival_agg
model_residuals
models


# Table of model coeffs and goodness-of-fit stats  -------------------
# Keep just the regressions with AR(1) errors
# arima_models = models[type == "arima"]

# Keep the auto fitted models;
# arima_models = models[type %in% c("lm", "ar1")]

# Keep all the models
model_stats = models

# model_stats = models[transform == "log" & type == "arima"] 
# model_stats = models[transform == "log" & type == "auto"] 


sapply(models$model, function(x) get_p(x))
sapply(models$model, function(x) get_c(x))
sapply(models$model, function(x) get_int(x))
model_stats[, p := sapply(models$model, function(x) get_p(x))]
model_stats[, c := sapply(models$model, function(x) get_c(x))]
model_stats[, intercept := sapply(models$model, function(x) get_int(x))]
model_stats[, SSR := get_ssr(model), by = list(lookback, transform, type)]

# Significance levels
model_stats[, sig := "N.S."]
model_stats[p < 0.01, sig := "0.01"]
model_stats[p >= 0.01 & p < 0.05, sig := "0.05"]

model_stats

# A melted data table for ggplotting: --------------------
models_stats_melted = melt(model_stats, 
                           measure.vars = c("AIC", "p", "c", "intercept", "RMSE", "SSR"), 
                           id.vars = c("type", "lookback", "transform"), variable.name = "statistic")
models_stats_melted[
  , 
  panel := 
    gsub("RMSE", "Root mean square error",
         gsub("unit_1", "1% MPB survival increase",
              gsub("unit_5", "5% MPB survival increase",
                   gsub("unit_10", "10% MPB survival increase",
                        gsub("c", "MPB survival slope coefficient", 
                             gsub("aic", "AIC", 
                                  gsub("p", "P-val.", 
                                       models_stats_melted$statistic)))))))]

models_stats_melted = merge(models_stats_melted, 
                            model_stats[, .(lookback, transform, type, sig)], 
                            by =  c("lookback", "transform", "type"))
models_stats_melted
names(models_stats_melted)
names(model_stats)
model_stats

models_log_interpretation =
  do.call(rbind, lapply(1:10, function(i)
    models_stats_melted[statistic == "c" & transform == "log" , .(lookback, type, transform, coeff = value, sig)]
    [, .(lookback, type, coeff, transform, percent_increase_kill =   
           100 * (exp(i * coeff) - 1), percent_survival_increase = i)]))


models_stats_melted[statistic == "c" & transform == "log" , .(lookback, type, transform, coeff = value, sig)]

models_log_interpretation

# Save tables ---------
save(models,
     model_stats, 
     model_residuals, 
     models_stats_melted, 
     models_log_interpretation,
     file = "data/models.Rd")



# Model diagnostics ----
lb = 9
mm = models[type == "lm"][transform == "log"][lookback == lb]$model[[1]]
mm = models[type == "ar1"][transform == "log"][lookback == lb]$model[[1]]
mm = models[type == "auto"][transform == "log"][lookback == lb]$model[[1]]

# qq plot
ggplot(data.frame(y = c(mm$residuals)), aes(sample = y)) + stat_qq() + stat_qq_line()

# resids/fitted
ggplot(data.frame(x = c(mm$fitted), y = c(mm$residuals)), aes(x, y)) + geom_point() + geom_hline(yintercept = 0)

# data with regression line
ggplot(kill_survival_agg[lookback == lb], 
       aes(x = 100 * survival, y = log(kill))) +
  geom_point() +
  geom_abline(intercept = get_int(mm), slope = get_c(mm))


aTSA::adf.test(mm$residuals)
mm



mm$coef

get_c(mm)
get_int(mm)
model_residuals[type == "arima" & transform == "log" & lookback == 8, cor(fitted_vals, log(kill))]
model_residuals[type == "arima" & transform == "none" & lookback == 8, cor(fitted_vals, kill)]
model_residuals[type == "arima" & transform == "log" & lookback == 1, cor(fitted_vals, log(kill))]
model_residuals[type == "arima" & transform == "none" & lookback == 1, cor(fitted_vals, log(kill))]


model_residuals[type == "arima" & transform == "log", cor(fitted_vals, log(kill)), by = lookback]
