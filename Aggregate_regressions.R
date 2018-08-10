source("Globals.R")
require(lmtest)
require(grid)
require(gridExtra)
require(gtable)

# some convenience functions:

# retrieve the p-value from a list containing one arima model
get_p = function(model_l, lb)
{
  # fit = models[transform == "log" & type == "arima"][lb]$model[[1]]
  fit = model_l[[1]]
  tab = get_coef_table(fit, lb)
  p_col = which(grepl(pattern = "Pr\\(", colnames(tab)))
  p_row = which(grepl(pattern = "MPB", rownames(tab)))
  return(tab[p_row, p_col])
}

# retrieve the regression coefficient from a list containing one arima model
get_c = function(model_l, lb)
{
  # fit = models[transform == "log" & type == "arima"][lb]$model[[1]]
  fit = model_l[[1]]
  tab = get_coef_table(fit, lb)
  c_col = which(grepl(pattern = "Estimate", colnames(tab)))
  c_row = which(grepl(pattern = "MPB", rownames(tab)))
  return(tab[c_row, c_col])
}

# retrieve the sum of squared residuals from a list containing one arima model
get_ssr = function(model_l)
{
  fit = model_l[[1]]
  return(sum(residuals(fit) ^ 2))
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
models = rbind(
  kill_survival_agg[ , .(model = list(lm(kill ~ survival, data = .SD)), type = "lm", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(lm(log(kill) ~ survival, data = .SD)), type = "lm", transform = "log"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::auto.arima(kill, xreg = survival)), type = "arima", transform = "none"), by = "lookback"],
  kill_survival_agg[ , .(model = list(forecast::auto.arima(log(kill), xreg = survival)), type = "arima", transform = "log"), by = "lookback"]
)
models$aic = sapply(models$model, function(x) AIC(x))

# Make sure lookback is a numeric variable
models[, lookback := as.numeric(lookback)]
kill_survival_agg[, lookback:= as.numeric(lookback)]

# table of residuals, in melted format
model_residuals = models[, .(fitted_vals = fitted.values(model[[1]]), residuals = model[[1]]$residuals, year = 1997:2010), by = list(lookback, type, transform)]
model_residuals = merge(kill_survival_agg, model_residuals)

kill_survival_agg
model_residuals

# Table of model coeffs and info for different lookbacks  -------------------
arimas_dt = models[transform == "log" & type == "arima"] 
arimas_dt[, p := get_p(model, lookback), by = lookback]
arimas_dt[, c := get_c(model, lookback), by = lookback]
arimas_dt[, me := get_me(model), by = lookback]
arimas_dt[, ssr := get_ssr(model), by = lookback]

# table and plot of model coefficients ---------------------------------
a_aic = aes(y = aic)
a_p = aes(y = p)
a_me = aes(y = me)
a_c = aes(y = c)
a_ssr = aes(y = ssr)
pty = geom_line()
t1 = theme(axis.title.x = element_blank(), axis.text.x = element_blank())
g_aic = ggplotGrob(g1 + a_aic + pty + ylab("AIC"))
g_ssr = ggplotGrob(g1 + a_ssr + pty + ylab("SSqResid") + t1)

g_me = ggplotGrob(g1 + a_me + pty + ylab("Mean Error") + t1 + scale_y_log10())
g_p   = ggplotGrob(g1 + a_p + pty + scale_y_log10() + t1 + ylab("p") + geom_hline(aes(yintercept = 0.05), linetype = 3))
g_c   = ggplotGrob(g1 + a_c + pty + t1 + ylab("coef"))

gg_all = rbind(g_c, g_p, g_ssr, g_aic, size = "first")
gg_all = rbind(g_c, g_p, g_aic, size = "first")

pdf(paste0(figures_dir, "model_coef_lookbacks_plot.pdf"), width = 4)
grid.newpage()
grid.table(arimas_dt[, .(lookback, coef = c, p = p, AIC = round(aic, digits = 1))], rows = NULL)
grid.newpage()
grid.draw(gg_all)
dev.off()
grid.arrange(g_aic, g_p, g_c, nrow = 3)
