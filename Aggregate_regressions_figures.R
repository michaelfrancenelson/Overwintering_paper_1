# packages, data ----
{
  
  source("Globals.R")
  # source("helper_functions.R")
  require(lmtest)
  require(data.table)
  require(grid)
  require(gridExtra)
  require(gtable)
  require(ggplot2)
  
  load("data/models.Rd")
  
  arimas_melted = models_stats_melted[transform == "log"]
  arimas_melted[, sig := replace(replace(
    sig,
    sig == "0.01", "< 0.01"),
    sig == "0.05", "< 0.05")]
  
  arimas_melted[, panel_code := replace(replace(replace(replace(replace(
    as.character(statistic), 
    statistic == "c", "a"), 
    statistic == "p", "b"),
    statistic == "RMSE", "c"),
    statistic == "AIC", "d"),
    statistic == "SSR", "e")]
  arimas_melted = arimas_melted[order(arimas_melted$statistic, arimas_melted$lookback), ]
  
  arimas_melted
}


# plotting parameters ----
line_size = 1.1

gl = geom_line(show.legend = F, aes(x = lookback, y = value), size = line_size, color = "red2")
gp1 = geom_point(mapping = aes(shape = sig), show.legend = F, size = 3, color = 1)
gp2 = geom_point(mapping = aes(shape = sig), show.legend = T, size = 3, color = 1)
gsh = scale_shape_manual(values = c(8, 10, 20))
gsz = scale_size_manual(values = c(2, 2, 1)) 
t1 = theme(axis.title.x = element_blank())
type_1 = "ar1"


# Model Coefficient panel plot with facet_wrap() ----------------
# ggplot(arimas_melted[statistic %in% c("RMSE", "p", "c", "AIC")], aes(x = lookback, y = value, color = panel)) + 
ggplot(arimas_melted[statistic %in% c("RMSE", "p", "c") & type == type_1],
       aes(x = lookback, y = value, color = panel)) + 
  gl + gp2 + gsh + gsz + t1 +
  facet_wrap(~panel_code, scales = "free", nrow = 4) + guides(color = F) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "bottom") +
  xlab("n years")


# panel plot with gtable (better) ----

g_c = ggplotGrob(
  ggplot(arimas_melted[statistic %in% c("c") & type == type_1], 
         aes(x = lookback, y = value)) + 
    gl + gp1 + gsh + gsz + t1 +
    ylab("Coeff.")
)

g_p = ggplotGrob(
  ggplot(arimas_melted[statistic %in% c("p") & type == type_1], 
         aes(x = lookback, y = value)) + 
    gl + gp1 + gsh + gsz + t1 +
    scale_y_log10() +
    ylab("p")
)

g_rmse = ggplotGrob(
  ggplot(arimas_melted[statistic %in% c("RMSE") & type == type_1],
         aes(x = lookback, y = value)) + 
    gl + gp2 + gsh + gsz +
    theme(legend.position = "bottom") +
    xlab("n years") +
    ylab("RMSE")
)

g_all = rbind(g_c, g_p, g_rmse, size = "first"); g_all$widths = unit.pmax(g_c$widths, g_p$widths, g_rmse$widths)
grid.newpage(); grid.draw(g_all)





# Model coefficient interpretation plot ----
lookbacks = c(1, 4, 9, 16)
lookbacks = 1:16
pcts = c(1, 2, 3, 4, 5)


dt = models_log_interpretation[
  lookback %in% lookbacks &
    percent_survival_increase %in% pcts &
    type == type_1][, pct := factor(percent_survival_increase)]
dt = models_log_interpretation[lookback %in% lookbacks & percent_survival_increase %in% pcts][, .(lookback, percent_increase_kill, percent_survival_increase)]
dt = models_log_interpretation[percent_survival_increase %in% pcts][, .(lookback, percent_increase_kill, percent_survival_increase)]
dt2 = dcast(dt, percent_survival_increase ~ lookback , value.var = "percent_increase_kill")
grid.table(round(dt2))
cat(print(round(dt2)))
writeClipboard(as.character(round(dt2)))

ggplot(dt, aes(x = lookback, y = percent_increase_kill, color = pct)) + 
  geom_line(size = line_size) + 
  scale_y_log10(breaks = c(1, 6, 12, 25, 50, 100, 250, 500, 1000, 2000, 2500)) +
  guides(color = guide_legend(title="% increase in MPB survival")) +
  xlab("n years") +
  ylab("% increase in killed trees") +
  theme_bw() +
  theme(legend.position = "bottom") 


models_log_interpretation[type == type_1 & lookback > 8, summary(coeff)]
models_log_interpretation[type == type_1 & lookback == 12, ]

