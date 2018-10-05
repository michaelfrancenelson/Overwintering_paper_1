source("Globals.R")
require(lmtest)
require(data.table)
require(grid)
require(gridExtra)
require(gtable)
require(ggplot2)

# some convenience functions ------------

{
  # retrieve the p-value from a list containing one arima model
  get_p = function(model_l, lb)
  {
    # fit = models[transform == "log" & type == "arima"][lb]$model[[1]]
    fit = model_l[[1]]
    tab = coeftest(fit)
    
    if( "fit" %in% class(fit))
    {
      p_col = which(grepl(pattern = "Pr\\(", colnames(tab)))
      p_row = which(grepl(pattern = "surviva", rownames(tab)))
    }

    else if ("Arima" %in% class(fit))
    {
      p_col = which(grepl(pattern = "Pr\\(", colnames(tab)))
      p_row = which(grepl(pattern = "xreg", rownames(tab)))
    }
    return(tab[p_row, p_col])
  }
  
# retrieve the regression coefficient from a list containing one arima model
  get_c = function(model_l, lb)
  {
    # fit = models[transform == "log" & type == "arima"][lb]$model[[1]]
    fit = model_l[[1]]
    # tab = get_coef_table(fit, lb)
    # tab = get_cc(fit, lb)
    coef_1 = coef(fit)
    
    if(is.null(names(coef_1))) return(coef_1[2])
    return(coef_1[["xreg"]])
    # 
    # c_col = which(grepl(pattern = "Estimate", colnames(tab)))
    # c_row = which(grepl(pattern = "MPB", rownames(tab)))
    # return(tab[c_row, c_col])
  }
  
  # retrieve the sum of squared residuals from a list containing one arima model
  get_ssr = function(model_l)
  {
    fit = model_l[[1]]
    return(sum(residuals(fit) ^ 2))
  }
}

kill_survival_agg = fread(paste0(local_dat_dir, "pine_range_national_forest_survival_kill_aggregated.csv"))

# Temporal autocorrelation of means: significance tests ------------------------------------------------

# Alternative hypotheses are stationarity, i.e. not autocorrelated.
# High p-values indicate autocorrelation
# We're only really interested in type 1 (we don't have enough data points to accurately determine the other types...)
aTSA::adf.test(kill_survival_agg[lookback == 1, kill])
aTSA::adf.test(kill_survival_agg[lookback == 1, log(kill)])
# Original, and log-transformed have autocorrelation.

# Model fits: lm and lm with ARIMA errors ------------------------------------------------------------
# Big data tables to hold the model objects and the residuals

kill_survival_agg[, survival100 := 100 * survival]
models = rbind(
  kill_survival_agg[ , .(model = list(lm(kill ~ survival100, data = .SD)), type = "lm", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(lm(log(kill) ~ survival100, data = .SD)), type = "lm", transform = "log"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::auto.arima(kill, xreg = survival100)), type = "auto", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::auto.arima(log(kill), xreg = survival100)), type = "auto", transform = "log"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::Arima(kill, order = c(1, 0, 0), xreg = survival100)), type = "arima", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::Arima(log(kill), order = c(1, 0, 0), xreg = survival100)), type = "arima", transform = "log"), by = "lookback"]
)
models$aic = sapply(models$model, function(x) AIC(x))
models$RMSE = sapply(models$model, function(x) forecast::accuracy(x)[1, "RMSE"])
# Make sure lookback is a numeric variable
models[, lookback := as.numeric(lookback)]
kill_survival_agg[, lookback:= as.numeric(lookback)]

# table of residuals, in melted format
model_residuals = models[, .(fitted_vals = fitted.values(model[[1]]), residuals = residuals(model[[1]]), year = 1997:2010), by = list(lookback, type, transform)]
model_residuals = merge(kill_survival_agg, model_residuals)

kill_survival_agg
model_residuals

mm = models[type == "arima" & transform == "log"][9, model][[1]]
str(mm, 1)
forecast::accuracy(models[type == "arima" & transform == "log"][1, model][[1]])$RSME
aa = forecast::accuracy(models[type == "auto" & transform == "log"][1, model][[1]])
forecast::accuracy(models[type == "lm" & transform == "log"][1, model][[1]])

str(aa)
aa[1, "RMSE"]


residuals(models[type == "arima" & transform == "log"][9, model][[1]])
fitted.values(models[type == "arima" & transform == "log"][9, model][[1]])
residuals(models[type == "auto" & transform == "log"][9, model][[1]])
models[type == "auto" & transform == "log"][9, model][[1]]
fitted.values(models[type == "auto" & transform == "log"][9, model][[1]])

# Table of model coeffs and info for different lookbacks  -------------------
arimas_dt = models[transform == "log" & type == "arima"] 
arimas_dt = models[transform == "log" & type == "auto"] 
arimas_dt[, p := get_p(model, lookback), by = lookback]
arimas_dt[, c := get_c(model, lookback), by = lookback]
# arimas_dt[, me := get_me(model), by = lookback]
arimas_dt[, ssr := get_ssr(model), by = lookback]

# Significance levels
arimas_dt[, sig := "N.S."]
arimas_dt[p < 0.01, sig := "0.01"]
arimas_dt[p >= 0.01 & p < 0.05, sig := "0.05"]




arimas_dt

# A melted data table for a faceted plot: --------------------
arimas_melted = melt(arimas_dt, measure.vars = c("aic", "p", "c"), id.vars = "lookback", variable.name = "statistic")
arimas_melted[, panel := gsub("c", "Slope coef.", gsub("aic", "AIC", gsub("p", "P-val.", arimas_melted$statistic)))]

arimas_melted = merge(arimas_melted, arimas_dt[, .(lookback, sig)], by = "lookback")


# table and plot of model coefficients ---------------------------------
aes_aic = aes(y = aic)
aes_p = aes(y = p)
aes_me = aes(y = me)
aes_c = aes(y = c)
aes_ssr = aes(y = ssr)
aes_rmse = aes(y = RMSE)
t1 = theme(axis.title.x = element_blank(), axis.text.x = element_blank())

gg_arimas = ggplot(arimas_dt, aes(x = lookback))

gg_arimas + aes_aic

gg_arimas + aes_rmse + geom_line()

gg_arimas + aes_aic + geom_line() + geom_point()

gg_aic = ggplotGrob(gg_arimas + aes_aic + geom_line() + geom_point() + ylab("AIC"))
gg_ssr = ggplotGrob(gg_arimas + aes_ssr + geom_line() + geom_point() + ylab("SSqResid") + t1)

gg_arimas + aes_aic + geom_line() + geom_point(mapping = aes(shape = sig)) + ylab("AIC")

grid.draw(gg_aic)
gg_arimas




gg_line = geom_line() + geom_point()

gg_me = ggplotGrob(gg_arimas + aes_me +  + ylab("Mean Error") + t1 + scale_y_logg_arimas0())
gg_rmse = ggplotGrob(gg_arimas + aes_rmse + gg_line + ylab("RMSE") + t1 + scale_y_logg_arimas0())
gg_p   = ggplotGrob(gg_arimas + aes_p + gg_line + scale_y_logg_arimas0() + t1 + ylab("p") + geom_hline(aes(yintercept = 0.05), linetype = 3))
gg_c   = ggplotGrob(gg_arimas + aes_c + gg_line + t1 + ylab("coef"))

gg_me = ggplotGrob(gg_arimas + aes_me +  + ylab("Mean Error") + t1)
gg_rmse = ggplotGrob(gg_arimas + aes_rmse + gg_line + ylab("RMSE") + t1 + scale_y_logg_arimas0())
gg_p   = ggplotGrob(gg_arimas + aes_p + gg_line + scale_y_logg_arimas0() + t1 + ylab("p") + geom_hline(aes(yintercept = 0.05), linetype = 3))
gg_c   = ggplotGrob(gg_arimas + aes_c + gg_line + t1 + ylab("coef"))






gg_all = rbind(gg_c, gg_p, gg_ssr, gg_aic, size = "first")
gg_all = rbind(gg_c, gg_p, gg_aic, size = "first")

pdf(paste0(figures_dir, "model_coef_lookbacks_plot.pdf"), width = 4)
grid.newpage()
grid.table(arimas_dt[, .(lookback, coef = c, p = p, AIC = round(aic, digits = 1))], rows = NULL)
grid.newpage()
grid.draw(gg_all)
dev.off()
grid.arrange(gg_aic, gg_p, gg_c, nrow = 3)


# Model Coefficient panel plot ----------------
arimas_melted = arimas_melted[order(arimas_melted$statistic, arimas_melted$lookback), ]

ggplot(arimas_melted, aes(x = lookback, y = value, color = panel)) + 
  geom_point(mapping = aes(shape = sig), show.legend = T, size = 3) +
  geom_line(show.legend = F, aes(x = lookback, y = value)) +
  facet_wrap(~panel, scales = "free", nrow = 3) + guides(color = F) +
  scale_shape_manual(values = c(8, 4, 20)) +
  theme(axis.title.y = element_blank()) +
  xlab("mean of n MPB survival years")

