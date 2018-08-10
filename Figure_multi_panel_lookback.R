source("Globals.R")

require(data.table)
require(ggplot2)
require(ggrepel)

kill_survival_agg = fread(paste0(local_dat_dir, "pine_range_national_forest_survival_kill_aggregated.csv"))


kill_survival_agg
length(kill_years) * 16


# Six-paneled figure.  Survival vs mortality.  lookbacks at 1, 2, 4, 6, 8, 10
lookbacks = c(1, seq(2, 10, 2))
lookback_labeller = function(vari, val) return(paste0("lookback = ", vari))

r1 = geom_text_repel()
f1 = facet_wrap(~lookback, scales = "free_x", labeller = label_both, nrow = 3)
x1 = xlab("MPB mean estimated survival")
y1 = ylab("pines killed per 1km cell")

facet_names = paste0("lookback: ", lookbacks)

g1 = ggplot(kill_survival_agg[lookback %in% lookbacks], aes(x = survival, y = kill, label = year))
g2 = ggplot(kill_survival_agg[lookback %in% lookbacks], aes(x = survival, y = log(kill)))

pdf(file = paste0(figures_dir, "lookback_panels.pdf"), height = 9, width = 9)
g1 + geom_point() + f1 + geom_text_repel() + x1 + y1
g1 + geom_point() + f1 + scale_y_log10() + geom_text_repel()  + x1 + y1
dev.off()
