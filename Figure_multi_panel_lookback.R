source("Globals.R")

require(data.table)
require(ggplot2)
require(ggrepel)

kill_survival_agg = fread(paste0(local_dat_dir, "pine_range_national_forest_survival_kill_aggregated.csv"))

# kill_survival_agg[, lookback := factor(lookback)]
length(kill_years) * 16

# Six-paneled figure.  Survival vs mortality.  lookbacks at 1, 2, 4, 6, 8, 10
lookbacks = c(1, seq(2, 10, 2))
lookback_labeller = function(vari, val) return(paste0("lookback = ", vari))
lookback_labeller = function(vari) {
  if(vari == 1 ) return(paste0("previous year"))
  else return(paste0("mean of previous ", vari, " years"))}

labels_lookback =  sapply(lookbacks, lookback_labeller)

# The labeller only works if the character vector entries are named
names(labels_lookback) = lookbacks


r1 = geom_text_repel()
# f1 = facet_wrap(~lookback, scales = "free_x", labeller = label_both, nrow = 3)
f1 = facet_wrap(~lookback, scales = "free_x", labeller = labeller(lookback = labels_lookback), nrow = 3)
x1 = xlab("MPB mean estimated survival")
y1 = ylab("pines killed per 1km cell")

g1 = ggplot(kill_survival_agg[lookback %in% lookbacks], aes(x = survival, y = kill, label = year))
g2 = ggplot(kill_survival_agg[lookback %in% lookbacks], aes(x = survival, y = log(kill)))

pdf(file = paste0(figures_dir, "lookback_panels.pdf"), height = 9, width = 9)
g1 + geom_point() + f1 + geom_text_repel() + x1 + y1
g1 + geom_point() + f1 + scale_y_log10() + geom_text_repel()  + x1 + y1
dev.off()


# Square panels, two rows ------------------------
t1 = theme(aspect.ratio = 1)
f2 = facet_wrap(~lookback, scales = "free_x", labeller = labeller(lookback = labels_lookback), nrow = 2)

pdf(file = paste0(figures_dir, "lookback_panels.pdf"), height = 9, width = 9)
g1 + geom_point() + f2 + geom_text_repel() + x1 + y1 + t1
g1 + geom_point() + f2 + scale_y_log10() + geom_text_repel()  + x1 + y1 + t1
dev.off()

# Year as color -------------------
kill_years
kill_breaks = c(1997, 2000, 2003, 2006, 2010)

color_scale_years = scale_color_continuous(breaks = kill_breaks, labels = kill_breaks)
color_scale_years = scale_color_discrete(breaks = kill_breaks, labels = kill_breaks)

g1_c = ggplot(kill_survival_agg[lookback %in% lookbacks], aes(x = survival, y = kill, color = year, label = year))
g2_c = ggplot(kill_survival_agg[lookback %in% lookbacks], aes(x = survival, y = log(kill), color = year))

pdf(file = paste0(figures_dir, "lookback_panels.pdf"), height = 9, width = 9)
g1 + geom_point() + f2 + geom_text_repel() + x1 + y1 + t1
g1 + geom_point() + f2 + scale_y_log10() + geom_text_repel()  + x1 + y1 + t1
g1_c + geom_point(size = 3) + f2 + x1 + y1 + t1 + color_scale_years
g1_c + geom_point(size = 3) + f2 + scale_y_log10()  + x1 + y1 + t1  + color_scale_years
dev.off()

