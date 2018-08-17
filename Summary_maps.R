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

# Convenience functions: ---------------
{
  
  title_scale = function(map_scale, leg_text_size = 9, leg_title_size = 11, subtitle_size = 13, title_size = 15)
  {
    return(
      theme(
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank(),
        legend.text = element_text(size = leg_text_size * map_scale),
        legend.title = element_text(size = leg_title_size * map_scale),
        plot.subtitle = element_text(size = subtitle_size * map_scale),
        plot.title = element_text(size = title_size * map_scale))
    )
  }
}

# Plot parameters ---------------
{
  width_pdf = height_pdf = 7
  width_png = height_png = 400
  map_scale = 1
  
  g_rast = geom_raster(aes(fill = value))
  maxpixels = 6e6
  
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






# Mean winter low ------------------
lab_mean = guides(fill = guide_legend(title = "mean temp."))

gp_mean = gplot(mean_winter_tmin, maxpixels = maxpixels)

# plot the state outlines in translucent black
g_states_mean = geom_polygon(data = states_poly, aes(long, lat, group = group), color = gray(0, alpha = 0.15), fill = rgb(0, 0, 0, 0))

# The few values at the high and low ends distort the color scale
# Legend labels and breaks:
range(mean_winter_tmin[], na.rm = T)
breaks_mean = c( -35, -15, 0, 4)

# Legend color bar options:
gd_mean =  guides(fill = guide_colourbar(title = "Mean"))
t1 = theme(legend.key.height = unit(0.1, "npc"), legend.key.width = unit(0.05, "npc"))

# mean pine kill blue to red:
# Custom red and blue color ramps
cf_red = colorRampPalette(c(rgb(1, 0.75, 0.75), rgb(1, 0.5, 0.5), rgb(1, 0, 0), rgb(0.65, 0, 0), rgb(0.2, 0, 0)))
cf_blue = colorRampPalette(c(rgb(0, 0, 0.2), rgb(0, 0, 1), rgb(0.5, 0.5, 1), rgb(0.75, 0.75, 1)))
cols = c(cf_blue(10), cf_red(15))

sc_mean_cf = scale_fill_gradientn(colours = cols, na.value = rgb(0, 0, 0, 0), breaks = breaks_mean, labels = breaks_mean)

map_scale = 1
gp_mean + gd_mean + g_rast + sc_mean_cf + g_states_mean + title_scale(map_scale) + coord_fixed() + t1 + ggtitle("Mean winter lowest temperature", subtitle = "1980 - 2016")

map_scale = 3
width_png = 0.95 * height_png
png(file = "figures/winter_tmin_mean.png", width = width_png * map_scale, height = height_png * map_scale)
gp_mean + gd_mean + g_rast + sc_mean_cf + g_states_mean + title_scale(map_scale) + coord_fixed() + t1 + ggtitle("Mean winter lowest temperature", subtitle = "1980 - 2016")
dev.off()



# Standard deviation plot -------------------
# The raster plot objects:
gp_sd = gplot(sd_winter_tmin, maxpixels = maxpixels)

# plot the state outlines in translucent black - like mean map
g_states_sd = geom_polygon(data = states_poly, aes(long, lat, group = group), color = gray(0, alpha = 0.15), fill = rgb(0, 0, 0, 0))

# The few values at the high and low ends distort the color scale
# Legend labels and breaks:
range(sd_winter_tmin[], na.rm = T)
boxplot(sd_winter_tmin[])
# It's very right skewed
breaks_sd = c( 1.5, 6, 10, 13)

# Legend color bar options:
gd_sd =  guides(fill = guide_colourbar(title = "S.D."))
t1_sd = theme(legend.key.height = unit(0.1, "npc"), legend.key.width = unit(0.05, "npc"))

# Custom red and blue color ramps
cf_red = colorRampPalette(c(rgb(1, 0.75, 0.75), rgb(1, 0.5, 0.5), rgb(1, 0, 0), rgb(0.65, 0, 0), rgb(0.2, 0, 0)))
cf_blue = colorRampPalette(c(rgb(0, 0, 0.2), rgb(0, 0, 1), rgb(0.5, 0.5, 1), rgb(0.75, 0.75, 1)))
cols = c(cf_blue(8), cf_red(25))
sc_sd_cf = scale_fill_gradientn(colours = cols, na.value = rgb(0, 0, 0, 0), breaks = breaks_sd, labels = breaks_sd)

map_scale = 1
gp_sd + gd_sd + g_rast + sc_sd_cf + g_states_sd + title_scale(map_scale) + coord_fixed() + t1 + ggtitle("Standard deviation winter lowest temperature", subtitle = "1980 - 2016")

map_scale = 3
width_png = 0.95 * height_png
png(file = "figures/winter_tmin_sd_red_blue.png", width = width_png * map_scale, height = height_png * map_scale)
gp_sd + gd_sd + g_rast + sc_sd_cf + g_states_sd + title_scale(map_scale) + coord_fixed() + t1 + ggtitle("Standard deviation winter lowest temperature", subtitle = "1980 - 2016")
dev.off()





# Plot pine kill sums ---------------
min(pine_sum[], na.rm = T)
summary(pine_sum[])
boxplot(pine_sum[]) # Very right skewed, we'll need a log transform

# remove those with values of < 1, since this causes issues with log scaling
pine_gg_sum = pine_sum + 1

# Truncate above a cutoff
p_cutoff = 2e4
sum(pine_gg_sum[] >= p_cutoff, na.rm = T)
sum(pine_gg_sum[] > 0, na.rm = T)
pine_gg_sum[pine_gg_sum >= p_cutoff] = p_cutoff


gp_sum = gplot(pine_gg_sum, maxpixels = 6e6)


# Some nice levels for the legend:
max(pine_sum[], na.rm = T)
scale_upper = plyr::round_any(max(pine_gg_sum[], na.rm = T), 1e4)

p_breaks = plyr::round_any(exp(seq(from = log(10), to = log(scale_upper), len = 4)), 100)
p_breaks[1] = 10
p_labels = p_breaks
p_labels[length(p_labels)] = paste0(tail(p_breaks, 1), "+")

# Legend color bar options:
gd_pine =  guides(fill = guide_colourbar(title = "Sum"))
t1_pine = theme(legend.key.height = unit(0.1, "npc"), legend.key.width = unit(0.05, "npc"))

# The state borders should be more prominent than in the other plots
g_states_pine = geom_polygon(data = states_poly, aes(long, lat, group = group), color = gray(0, alpha = 0.25), fill = rgb(0, 0, 0, 0))


# Custom red and blue color ramps
cf_red_pine = colorRampPalette(c(
  rgb(1, 0.75, 0.75),
  rgb(1, 0.35, 0.35),
  rgb(1, 0.05, 0.05),
  rgb(1, 0, 0), 
  rgb(0.35, 0, 0)))

cf_red_pine_2 = colorRampPalette(c(
  # rgb(1, 0.75, 0.75),
  # rgb(1, 0.35, 0.35),
  # rgb(1, 0.05, 0.05),
  rgb(1, 0, 0), 
  rgb(0.35, 0, 0)))
cf_yellow_pine = colorRampPalette(c(
  rgb(1, 0.3, 0),
  rgb(1, 0.15, 0))
  )


plot(1, col = rgb(1, 0.75, 0), pch = 16, cex = 4)
cols_pine = c(cf_red_pine(25))
cols_pine_2 = c(cf_yellow_pine(5), cf_red_pine_2(12))

sc_pine_cf = scale_fill_gradientn(
  colours = cols_pine, na.value = rgb(0, 0, 0, 0), 
                                  breaks = p_breaks, labels = p_labels, trans = "log")

sc_pine_cf_2 = scale_fill_gradientn(
  colours = cols_pine_2, na.value = rgb(0, 0, 0, 0), 
  breaks = p_breaks, labels = p_labels, trans = "log")

map_scale = 1
gp_sum + gd_pine + g_rast + sc_pine_cf + g_states_pine + title_scale(map_scale) + coord_fixed() + t1_pine + ggtitle("Cumulative pines killed", subtitle = "1980 - 2016")
gp_sum + gd_pine + g_rast + sc_pine_cf_2 + g_states_pine + title_scale(map_scale) + coord_fixed() + t1_pine + ggtitle("Cumulative pines killed", subtitle = "1980 - 2016")


map_scale = 3
width_png = 0.95 * height_png
png(file = "figures/cumulative_pine_kill_red.png", width = width_png * map_scale, height = height_png * map_scale)
gp_sum + gd_pine + g_rast + sc_pine_cf + g_states_pine + title_scale(map_scale) + coord_fixed() + t1_pine + ggtitle("Cumulative pines killed", subtitle = "1980 - 2016")
dev.off()

png(file = "figures/cumulative_pine_kill_red_orange.png", width = width_png * map_scale, height = height_png * map_scale)
gp_sum + gd_pine + g_rast + sc_pine_cf_2 + g_states_pine + title_scale(map_scale) + coord_fixed() + t1_pine + ggtitle("Cumulative pines killed", subtitle = "1980 - 2016")
dev.off()

