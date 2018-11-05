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
  arimas_melted[, sig_legend := replace(replace(
    sig,
    sig == "0.01", "< 0.01"),
    sig == "0.05", "< 0.05")]
  arimas_melted[, sig_name := replace(replace(replace(
    sig,
    sig == "N.S.", "ns"),
    sig == "0.01", "s_01"),
    sig == "0.05", "s_05")]
  
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
{
  point_col = "darkcyan"
  point_col = "navyblue"
  point_size = 3
  line_size = 1.1
  line_col = gray(0.5, alpha = 0.5)
  sig_point_shapes = c(8, 16) # different shapes by significance level
  sig_point_shape = 1 # For single shape plot
  sig_point_sizes  = c(s_01 = 2, s_05 = 4, ns = 1)
  sig_point_stroke = c(s_01 = 2, s_05 = 2, ns = 1)
  type_1 = "ar1"  # the model type to use for plotting
  
  stroke_width = 1.2
  
  arimas_melted$sig_stroke = 1
  arimas_melted[sig_name != "ns", sig_stroke := stroke_width]
  
  lab_coef = "model coef."
  lab_pval = "p"
  lab_rmse = "RMSE"
}

# plotting style objects ----
{
  gl = geom_line(show.legend = F, aes(x = lookback, y = value), 
                 size = line_size, color = line_col)
  gp_no_leg = geom_point(
    mapping = aes(shape = sig, stroke = sig_stroke), 
    show.legend = F, size = point_size, color = point_col)
  gp_leg = geom_point(
    mapping = aes(shape = sig, stroke = sig_stroke), 
    show.legend = T, size = point_size, color = point_col)
  g_shapes = scale_shape_manual(values = sig_point_shapes)#, stroke = sig_point_stroke)
  g_shapes_str = scale_shape_manual(values = sig_point_stroke, aesthetics = "stroke")#, stroke = sig_point_stroke)
  g_1_shape = scale_shape_manual(values = sig_point_shape)
  
  sc_y = scale_y_continuous(expand = c(0.1, 0))
  
  gsz = scale_size_manual(values = sig_point_sizes) 
  t1 = theme(axis.title.x = element_blank(),
             panel.background = element_rect(fill = gray(0.95), color = 1))
}

# plot objects: p-values by point shape ----
{
  dat_1 = arimas_melted[type == type_1, .(lookback, statistic, value, sig_stroke, sig_name, sig = sig_legend)]
  g_coef_1 = ggplotGrob(
    ggplot(dat_1[statistic %in% c("c")],
           # arimas_melted[statistic %in% c("c") & type == type_1], 
           aes(x = lookback, y = value)) + 
      gl + gp_no_leg + g_shapes + gsz + t1 + sc_y +
      ylab(lab_coef)
  )
  
  g_p_1 = ggplotGrob(
    ggplot(dat_1[statistic %in% c("p")], aes(x = lookback, y = value)) + 
      gl + gp_no_leg + g_shapes + gsz + t1 +
      scale_y_log10(expand = c(0.1, 0)) +
      ylab(lab_pval)
  )
  
  
  g_rmse_1 = ggplotGrob(
  ggplot(dat_1[statistic %in% c("RMSE")],
         aes(x = lookback, y = value)) + 
    gl + gp_leg + g_shapes + gsz + t1 + sc_y + 
    theme(legend.position = "bottom") +
    xlab("n years") +
    ylab(lab_rmse) +
    guides(
      shape = guide_legend(override.aes = list(
        stroke = stroke_width, 
        size = point_size))
    )
  )
}

pdf(file = "figures/fig_02_regressions_coeff_summary.pdf")
g_all_1 = rbind(g_coef_1, g_p_1, g_rmse_1, size = "first"); g_all_1$widths = unit.pmax(g_coef_1$widths, g_p_1$widths, g_rmse_1$widths)
grid.newpage(); grid.draw(g_all_1)
dev.off()
# 4 panels with gtable: p-val in own panel




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

