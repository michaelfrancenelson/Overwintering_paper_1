{
source("Globals.R")

require(raster)
require(rasterVis)
require(ggplot2)
require(rgdal)
require(ggpolypath)
require(data.table)
}

# Load data ----------------
{
national_forests_poly = readOGR(dsn = paste0(spatial_data_output_dir, "national_forests_in_mpb_states"), layer = "national_forests_in_mpb_states")
states_poly = readOGR(dsn = paste0(spatial_data_output_dir, "mpb_states"), layer = "mpb_states")

# pre-make ggplot objects for the polygons
g_nf = geom_polypath(data = national_forests_poly, aes(long, lat, group = group), color = 1, fill = rgb(0, 0, 0, 0))
g_st = geom_polygon(data = states_poly, aes(long, lat, group = group), color = 1, fill = rgb(0, 0, 0, 0))

# summary stat rasters:
pine_sum = raster(paste0(local_dat_dir, "pine_sum.nc"),crs = proj4_master)

# we want NA's in cells with zero pine kill:
pine_sum[pine_sum[] == 0] = NA

mean_winter_tmin = raster(paste0(local_dat_dir, "mean_winter_tmin.nc"), crs = proj4_master)
sd_winter_tmin = raster(paste0(local_dat_dir, "sd_winter_tmin.nc"), crs = proj4_master)

mean_surv = raster(paste0(local_dat_dir, "mean_surv.nc"), crs = proj4_master)
sd_surv = raster(paste0(local_dat_dir, "sd_surv.nc"), crs = proj4_master)
}


# Raster brick summary stats ----------------
# winter_tmin = brick(paste0(spatial_data_output_dir, "winter_tmin_brick.nc"), crs = proj4_master)
# annual_tmin = brick(paste0(spatial_data_output_dir, "annual_tmin_brick.nc"), crs = proj4_master)
# survival_brick = brick(paste0(spatial_data_output_dir, "mpb_overwinter_survival_brick.nc"), crs = proj4_master)
# # Calculate the average and sd 
# mean_winter_tmin = raster::calc(winter_tmin, fun = mean, na.rm = T)
# sd_winter_tmin   = raster::calc(winter_tmin, fun = sd, na.rm = T)
# mean_surv = raster::calc(survival_brick, fun = mean, na.rm = T)
# sd_surv = raster::calc(survival_brick, fun = sd, na.rm = T)

# proj4string(mean_winter_tmin) = proj4_master
# proj4string(sd_winter_tmin) = proj4_master
# proj4string(mean_surv) = proj4_master
# proj4string(sd_surv) = proj4_master

# pine_brick = brick(paste0(spatial_data_output_dir, "pine_kill_western_US_brick.nc"), crs = proj4_master)
# pine_sum = raster::calc(pine_brick, fun = sum, na.rm = T)

# # those were slow, so save copies
# writeRaster(mean_winter_tmin, paste0(local_dat_dir, "mean_winter_tmin.nc"), format = "CDF", overwrite = T)
# writeRaster(sd_winter_tmin, paste0(local_dat_dir, "sd_winter_tmin.nc"), format = "CDF", overwrite = T)
# writeRaster(mean_surv, paste0(local_dat_dir, "mean_surv.nc"), format = "CDF", overwrite = T)
# writeRaster(sd_surv, paste0(local_dat_dir, "sd_surv.nc"), format = "CDF", overwrite = T)
# writeRaster(pine_sum, paste0(local_dat_dir, "pine_sum.nc"), format = "CDF", overwrite = T)

# Plot mean/sd winter min temps ------------------
lab_sd = guides(fill = guide_legend(title = "SD temp."))
lab_mean = guides(fill = guide_legend(title = "mean temp."))



gg_p2 + geom_point(size = 0.1) + sc_col_p_2 + g_st + t1_pine + gd_p + coord_fixed()




sc_col = scale_fill_gradientn(colours = terrain.colors(10), na.value = rgb(0, 0, 0, 0))

g_rast = geom_raster(aes(fill = value))
t1 = theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
gw_mean = gplot(mean_winter_tmin)
gw_sd = gplot(sd_winter_tmin)


map_scale = 4
t1_surv = theme(
  axis.title = element_blank(), 
  axis.text = element_blank(), 
  axis.ticks = element_blank(), 
  panel.background = element_blank(),
  # legend.key.size = unit(1 * map_scale, "npc"),
  # legend.key = element_rect(size = 4 * map_scale),
  legend.text = element_text(size = 9 * map_scale),
  legend.title = element_text(size = 11 * map_scale))
gd_mean = guides(fill = guide_legend(override.aes = list(size = 4 * map_scale), title = "mean"))
gd_sd = guides(fill = guide_legend(override.aes = list(size = 4 * map_scale), title = "s.d."))


width_png = height_png = 400

png(file = "figures/winter_tmin_sd.png", width = width_png * map_scale, height = height_png * map_scale)
gw_sd + g_rast + sc_col + g_st + t1_surv + gd_sd + coord_fixed()
dev.off()


png(file = "figures/winter_tmin_mean.png", width = width_png * map_scale, height = height_png * map_scale)
gw_mean + g_rast + sc_col + t1_surv + g_st + gd_mean + coord_fixed()
dev.off()

gw_mean + g_rast + sc_col + t1 + g_st + lab_mean
gw_sd + g_rast + sc_col + g_st + lab_sd



# Plot pine kill sums ---------------

min(pine_sum[], na.rm = T)
summary(pine_sum[])
boxplot(pine_sum[])

# remove those with values of < 1, since this causes issues with

pine_gg_sum = pine_sum + 1
# Truncate above a cutoff
p_cutoff = 2e4
sum(pine_gg_sum[] >= p_cutoff, na.rm = T)
sum(pine_gg_sum[] > 0, na.rm = T)

pine_gg_sum[pine_gg_sum >= p_cutoff] = p_cutoff

lab_p =  guides(fill = guide_legend(title = "Pines\nkilled\nper\ncell"))
lab_p2 =  guides(colour = guide_legend(title = "Pines\nkilled\nper\ncell"))
lab_p =  guides(fill = guide_legend(title = "sum"))
lab_p2 =  guides(colour = guide_legend(title = "sum"))

# Some nice levels for the legend:


max(pine_sum[], na.rm = T)
scale_upper = plyr::round_any(max(pine_gg_sum[], na.rm = T), 1e4)

p_breaks = plyr::round_any(exp(seq(from = log(10), to = log(scale_upper), len = 4)), 100)
p_breaks[1] = 10
p_labels = p_breaks
p_labels[length(p_labels)] = paste0(tail(p_breaks, 1), "+")


sc_col_p = scale_fill_gradientn(colours = heat.colors(10), na.value = rgb(0, 0, 0, 0), trans = "log", breaks = p_breaks, labels = p_labels)

sc_col_p = scale_fill_gradientn(colours = heat.colors(10), na.value = rgb(0, 0, 0, 0), trans = "log", breaks = p_breaks)

gp_sum = gplot(pine_gg_sum)



t1 = theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())


map_scale = 1
t1_pine = theme(
  axis.title = element_blank(), 
  axis.text = element_blank(), 
  axis.ticks = element_blank(), 
  panel.background = element_blank(),
  # legend.key.size = unit(0.04 * map_scale, "npc"),
  legend.key = element_rect(size = map_scale),
  legend.text = element_text(size = 9 * map_scale),
  legend.title = element_text(size = 11 * map_scale))
gp_sum + sc_col_p + g_st + t1 + g_rast + lab_p



# The raster plot doesn't look very good. Try maing into a data table and plot with round points
pine_dt = data.table(rasterToPoints(pine_gg_sum))
pine_dt

sc_col_p_2 = scale_color_gradientn(colours = heat.colors(10), na.value = rgb(0, 0, 0, 0), trans = "log", breaks = p_breaks, labels = p_labels)

gg_p2 = ggplot(pine_dt, aes(x, y, colour = layer))
gg_p2 = ggplot(pine_dt[sample(1:nrow(pine_dt), 100)], aes(x, y, colour = layer))

png(width = 1500, height = 1500, filename = "map1.png")


map_scale = 4
t1_pine = theme(
  axis.title = element_blank(), 
  axis.text = element_blank(), 
  axis.ticks = element_blank(), 
  panel.background = element_blank(),
  # legend.key.size = unit(1 * map_scale, "npc"),
  # legend.key = element_rect(size = 4 * map_scale),
  legend.text = element_text(size = 9 * map_scale),
  legend.title = element_text(size = 11 * map_scale))
gd_p = guides(colour = guide_legend(override.aes = list(size = 4 * map_scale), title = "sum"))
gg_p2 + geom_point(size = 0.1) + sc_col_p_2 + g_st + t1_pine + gd_p + coord_fixed()



width_pdf = height_pdf = 7
width_png = height_png = 400
pdf(file = "maps.pdf", map_scale * width_pdf, map_scale * height_pdf)
gg_p2 + geom_point(size = 0.1) + sc_col_p_2 + g_st + lab_p2 + t1_pine + gd_p + coord_fixed()
gg_p2 + geom_point(size = 0.1) + sc_col_p_2 + g_st + lab_p2 + t1_pine + gd_p
dev.off()

png(file = "figures/pine_map.png", width = width_png * map_scale, height = height_png * map_scale)
gg_p2 + geom_point(size = 0.1 * map_scale) + sc_col_p_2 + g_st + lab_p2 + t1_pine + gd_p + coord_fixed()
dev.off()




