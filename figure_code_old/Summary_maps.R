# Packages  -------
{
  
  require(raster)
  require(rasterVis)
  require(ggplot2)
  require(rgdal)
  require(ggpolypath)
  require(data.table)
  require(grid)
  require(gridExtra)
}

# Load data ----------------
{
  source("Globals.R")
  
  national_forests_poly = readOGR(dsn = paste0(local_dat_dir, "national_forests_in_mpb_states"), layer = "national_forests_in_mpb_states")
  
  states_poly = readOGR(dsn = paste0(local_dat_dir, "mpb_states"), layer = "mpb_states")
  
  
  # pre-make ggplot objects for the polygons
  g_nf = geom_polypath(data = national_forests_poly, aes(long, lat, group = group), color = 1, fill = rgb(0, 0, 0, 0))
  g_st = geom_polygon(data = states_poly, aes(long, lat, group = group), color = 1, fill = rgb(0, 0, 0, 0))
  
  # Individual years' survival and pine kill:
  survival_brick = brick(paste0(local_dat_dir, "mpb_overwinter_survival_brick.nc"), crs = proj4_master)
  pine_brick = brick(paste0(local_dat_dir, "pine_kill_western_US_brick.nc"), crs = proj4_master)
  names(survival_brick) = daymet_years[-1]
  names(pine_brick) = kill_years
  
  # summary stat rasters:
  pine_sum = raster(paste0(local_dat_dir, "pine_sum.nc"),crs = proj4_master)
  
  # we want NA's in cells with zero pine kill:
  pine_sum[pine_sum[] == 0] = NA
  
  mean_winter_tmin = raster(paste0(local_dat_dir, "mean_winter_tmin.nc"), crs = proj4_master)
  sd_winter_tmin = raster(paste0(local_dat_dir, "sd_winter_tmin.nc"), crs = proj4_master)
  mean_winter_tmin_kill_years = raster(paste0(local_dat_dir, "mean_winter_tmin_kill_years.nc"), crs = proj4_master)
  sd_winter_tmin_kill_years = raster(paste0(local_dat_dir, "sd_winter_tmin_kill_years.nc"), crs = proj4_master)
  
  mean_surv = raster(paste0(local_dat_dir, "mean_surv.nc"), crs = proj4_master)
  sd_surv = raster(paste0(local_dat_dir, "sd_surv.nc"), crs = proj4_master)
}

# Convenience functions: ---------------
{
  
  scaled_theme = function(
    map_scale, marg = 25, leg_text_size = 9, leg_title_size = 11, subtitle_size = 13, title_size = 15)
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
        plot.title = element_text(size = title_size * map_scale),
        legend.margin = margin(0, marg, 0, marg),
        legend.box.margin = legend.box.margin,
        legend.position = "bottom"))
  }
}


# shared plot elements ---------------
{
  width_pdf = height_pdf = 7
  width_png = height_png = 400
  map_scale = 1
  
  g_rast = geom_raster(aes(fill = value))
  maxpixels = 1e7
  
  legend.margin = margin(0, 15, 0, 15)
  legend.box.margin = margin(-25, 40, 0, 40)
  
  legend_bar_base_width = 18
  legend_bar_base_height = 1.5
  
  # plot the state outlines in translucent black
  states_poly_gray_1 = geom_polygon(
    data = states_poly, 
    aes(long, lat, group = group), 
    color = gray(0, alpha = 0.15), 
    fill = rgb(0, 0, 0, 0))
  
}

# Raster brick summary stats ----------------
# winter_tmin = brick(paste0(spatial_data_output_dir, "winter_tmin_brick.nc"), crs = proj4_master)
# annual_tmin = brick(paste0(spatial_data_output_dir, "annual_tmin_brick.nc"), crs = proj4_master)
# names(winter_tmin) = paste0("winter_ending_", daymet_years[-1])
# winter_tmin
# 
# winter_tmin_kill_years = subset(winter_tmin, which(daymet_years %in% kill_years) - 2)

# # Calculate the average and sd 
# mean_winter_tmin = raster::calc(winter_tmin, fun = mean, na.rm = T)
# sd_winter_tmin   = raster::calc(winter_tmin, fun = sd, na.rm = T)
# mean_winter_tmin_kill_years = raster::calc(winter_tmin_kill_years, fun = mean, na.rm = T)
# sd_winter_tmin_kill_years   = raster::calc(winter_tmin_kill_years, fun = sd, na.rm = T)


# mean_surv = raster::calc(survival_brick, fun = mean, na.rm = T)
# sd_surv = raster::calc(survival_brick, fun = sd, na.rm = T)

# proj4string(mean_winter_tmin) = proj4_master
# proj4string(sd_winter_tmin) = proj4_master
# proj4string(mean_surv) = proj4_master
# proj4string(sd_surv) = proj4_master

# pine_sum = raster::calc(pine_brick, fun = sum, na.rm = T)

# # those were slow, so save copies
# writeRaster(mean_winter_tmin, paste0(local_dat_dir, "mean_winter_tmin.nc"), format = "CDF", overwrite = T)
# writeRaster(sd_winter_tmin, paste0(local_dat_dir, "sd_winter_tmin.nc"), format = "CDF", overwrite = T)
# writeRaster(mean_winter_tmin_kill_years, paste0(local_dat_dir, "mean_winter_tmin_kill_years.nc"), format = "CDF", overwrite = T)
# writeRaster(sd_winter_tmin_kill_years, paste0(local_dat_dir, "sd_winter_tmin_kill_years.nc"), format = "CDF", overwrite = T)
# writeRaster(mean_surv, paste0(local_dat_dir, "mean_surv.nc"), format = "CDF", overwrite = T)
# writeRaster(sd_surv, paste0(local_dat_dir, "sd_surv.nc"), format = "CDF", overwrite = T)
# writeRaster(pine_sum, paste0(local_dat_dir, "pine_sum.nc"), format = "CDF", overwrite = T)

# Mean winter low ------------------
{
  gplot_mean = gplot(mean_winter_tmin, maxpixels = maxpixels)
  gplot_mean_kill_years = gplot(mean_winter_tmin_kill_years, maxpixels = maxpixels)
  
  # The few values at the high and low ends distort the color scale
  # Legend labels and breaks:
  range(mean_winter_tmin[], na.rm = T)
  range(mean_winter_tmin_kill_years[], na.rm = T)
  breaks_mean = c( -35, -15, 0, 4)
  
  # mean pine kill blue to red:
  # Custom red and blue color ramps
  ramp_red_mean = colorRampPalette(c(
    rgb(1, 1, 1), 
    # rgb(1, 0.85, 0.85), 
    rgb(1, 0.75, 0.75), 
    rgb(1, 0.5, 0.5), 
    rgb(1, 0.25, 0.25), 
    rgb(1, 0, 0), 
    rgb(0.85, 0, 0), 
    rgb(0.5, 0, 0), 
    rgb(0.2, 0, 0), 
    rgb(0.15, 0, 0), 
    rgb(0.1, 0, 0)))
  ramp_blue_mean = colorRampPalette(c(
    rgb(0, 0, 0.2), 
    rgb(0, 0, 1), 
    rgb(0.25, 0.25, 1), 
    rgb(0.5, 0.5, 1), 
    rgb(0.75, 0.75, 1)))#,
    # rgb(0.85, 0.85, 1),
    # rgb(0.95, 0.95, 1)))
  cols_mean = c(ramp_blue_mean(12), ramp_red_mean(12))
  scale_color_mean = scale_fill_gradientn(
    colours = cols_mean, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_mean, 
    labels = breaks_mean)
  
  # We won't use this for the submitted figure
  tit_mean = ggtitle("Mean winter lowest temperature", subtitle = "1980 - 2016")
  
  # Legend titles
  leg_tit_mean = paste0("mean winter minimum temp (°C)")
  
  # guide_colorbar_below_mean = guides(
  #   fill = guide_colourbar(
  #     title = leg_tit_mean,
  #     title.position = "bottom", 
  #     title.hjust = 0.5,
  #     barheight = 1.5,
  #     barwidth = 15,
  #     label.position = "top"))
  # 
  ggplot_mean = function(map_scale, bar_scale = 3, legend_bar_base_height = 1.5, legend_bar_base_width = 18) 
  {
    gplot_mean + coord_fixed() + scaled_theme(map_scale) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_mean,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      g_rast + states_poly_gray_1 + scale_color_mean 
  }
  
  ggplot_mean(1, 1)
  
  ggplot_mean_kill_years  = function(map_scale, bar_scale = 3, legend_bar_base_height = 1.5, legend_bar_base_width = 18) 
  {
    gplot_mean_kill_years + coord_fixed() + scaled_theme(map_scale) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_mean,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      g_rast + states_poly_gray_1 + scale_color_mean 
  }
  
  
}


# Standard deviation of winter low ------------------
{
  gplot_sd = gplot(sd_winter_tmin, maxpixels = maxpixels)
  gplot_sd_kill_years = gplot(sd_winter_tmin_kill_years, maxpixels = maxpixels)
  
  sd_winter_tmin_truncated = sd_winter_tmin
  sd_winter_tmin_truncated[sd_winter_tmin_truncated[] > 6] = 6
  gplot_sd_truncated = gplot(sd_winter_tmin_truncated, maxpixels = maxpixels)
  
  # The few values at the high and low ends distort the color scale
  # Legend labels and breaks:
  range(sd_winter_tmin[], na.rm = T)
  range(sd_winter_tmin_kill_years[], na.rm = T)
  boxplot(sd_winter_tmin[])
  
  # It's very right skewed
  breaks_sd = c( 1.5, 6, 10, 13)
  breaks_sd_kill_years = c( 1, 4, 7, 10)
  
  breaks_sd_truncated = 0:6
  labels_sd_truncated = c(0:5, ">6")
  
  # We won't use this for the submitted figure
  
  # Custom red and blue color ramps
  ramp_red_sd = colorRampPalette(c(
    rgb(1, 0.85, 0.85),
    rgb(1, 0.75, 0.75),
    rgb(1, 0.5, 0.5), 
    rgb(1, 0, 0), 
    rgb(0.65, 0, 0), 
    rgb(0.2, 0, 0)))
  ramp_blue_sd = colorRampPalette(c(
    # rgb(0, 0, 0.2), 
    rgb(0, 0, 0.5), 
    rgb(0, 0, 1), 
    rgb(0.25, 0.25, 1),
    rgb(0.5, 0.5, 1),
    rgb(0.75, 0.75, 1),
    rgb(0.85, 0.85, 1)))
  cols_sd = c(ramp_blue_sd(12), ramp_red_sd(12))
  scale_color_sd = scale_fill_gradientn(
    colours = cols_sd, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_sd, 
    labels = breaks_sd)
  
  scale_color_sd_truncated = scale_fill_gradientn(
    colours = cols_sd, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_sd_truncated, 
    labels = labels_sd_truncated)
  
  scale_color_sd_kill_years = scale_fill_gradientn(
    colours = cols_sd, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_sd_kill_years, 
    labels = breaks_sd_kill_years)
  
  # We won't use this for the submitted figure
  tit_sd = ggtitle("Standard deviation of winter minimum temperature", subtitle = "1980 - 2016")
  
  # Legend title:
  leg_tit_sd = paste0("standard deviation of winter minimum temp (°C)")
  
  guide_colorbar_below_sd = guides(
    fill = guide_colourbar(
      title = leg_tit_sd,
      title.position = "bottom", 
      title.hjust = 0.5,
      label.position = "top"))
  
  map_scale = 1
  
  ggplot_sd_truncated = function(map_scale, bar_scale = 3, legend_bar_base_height = 1.5, legend_bar_base_width = 18)
  {
    gplot_sd_truncated + coord_fixed() + scaled_theme(map_scale) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_sd,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      g_rast + states_poly_gray_1 + scale_color_sd_truncated
  }
  
  ggplot_sd_truncated(1, 1)
  
  
  ggplot_sd = function(map_scale, bar_scale = 3, legend_bar_base_height = 1.5, legend_bar_base_width = 18)
  {
    gplot_sd + coord_fixed() + scaled_theme(map_scale) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_sd,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      g_rast + states_poly_gray_1 + scale_color_sd 
  }
  
  ggplot_sd_kill_years = function(map_scale, bar_scale = 3, legend_bar_base_height = 1.5, legend_bar_base_width = 18)
  {
    gplot_sd_kill_years + coord_fixed() + scaled_theme(map_scale) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_sd,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      g_rast + states_poly_gray_1 + scale_color_sd_kill_years
  }
}


# Plot pine kill sums ---------------
{
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
  
  gplot_pine_sum = gplot(pine_gg_sum, maxpixels = maxpixels)
  
  # Some nice levels for the legend:
  max(pine_sum[], na.rm = T)
  scale_upper = plyr::round_any(max(pine_gg_sum[], na.rm = T), 1e4)
  
  breaks_pine_sum = plyr::round_any(exp(seq(from = log(10), to = log(scale_upper), len = 4)), 100)
  breaks_pine_sum[1] = 10
  labels_pine_sum = breaks_pine_sum
  labels_pine_sum[length(labels_pine_sum)] = paste0(tail(breaks_pine_sum, 1), "+")
  
  # We won't use this for the submitted figure
  tit_pine_sum = ggtitle("Cumulative MPB pine mortality", subtitle = "1980 - 2016")
  
  # Legend title:
  leg_tit_pine_sum = paste0("cumulative MPB pine mortality")
  
  guide_colorbar_below_pine_sum = guides(
    fill = guide_colourbar(
      title = leg_tit_pine_sum,
      title.position = "bottom", 
      title.hjust = 0.5,
      label.position = "top"
      
    ))
  
  # The state borders should be more prominent than in the other plots
  states_poly_pine_sum = geom_polygon(data = states_poly, aes(long, lat, group = group), 
                                      color = gray(0, alpha = 0.75), fill = rgb(0, 0, 0, 0))

  # Custom red and blue color ramps
  # ramp_red_pine_sum = colorRampPalette(c(
  #   rgb(1, 0.75, 0.75),
  #   rgb(1, 0.35, 0.35),
  #   rgb(1, 0.05, 0.05),
  #   rgb(1, 0, 0), 
  #   rgb(0.35, 0, 0)))
  ramp_red_pine_sum = colorRampPalette(c(
    rgb(1, 0.75, 0),
    rgb(1, 0.35, 0),
    rgb(1, 0.05, 0),
    rgb(1, 0, 0),
    rgb(0.35, 0, 0)))
  
  
  plot(0, 0, cex = 20, pch = 16, col = rgb(1, 0.05, 0.0))
  
  ramp_red_pine_sum_2 = colorRampPalette(c(
    # rgb(1, 0.75, 0.75),
    # rgb(1, 0.35, 0.35),
    # rgb(1, 0.05, 0.05),
    rgb(1, 0, 0), 
    rgb(0.35, 0, 0)))
  
  ramp_yellow_pine_sum = colorRampPalette(c(
    rgb(1, 0.3, 0),
    rgb(1, 0.15, 0))
  )
  cols_pine = c(ramp_red_pine_sum(25))
  cols_pine_2 = c(ramp_yellow_pine_sum(5), ramp_red_pine_sum_2(12))
  
  scale_color_pine_sum = scale_fill_gradientn(
    colours = cols_pine, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum, trans = "log")
  
  scale_color_pine_sum_2 = scale_fill_gradientn(
    colours = cols_pine_2, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum, trans = "log")
  
  ggplot_pine_sum = function(map_scale, bar_scale = 3, legend_bar_base_height = 1.5, legend_bar_base_width = 18)
  {
    gplot_pine_sum + coord_fixed() + scaled_theme(map_scale) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_pine_sum,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      # g_rast + states_poly_pine_sum + scale_color_pine_sum_2 
      g_rast + states_poly_pine_sum + scale_color_pine_sum
  }
  ggplot_pine_sum(1, 1)
}




# Save figures -------------------------
{
  map_scale = 3
  bar_scale = 1
  bar_scale_2k = 2.2
  grob_mean = ggplot_mean(map_scale, bar_scale)
  grob_sd = ggplot_sd(map_scale, bar_scale)
  grob_sd_truncated = ggplot_sd_truncated(map_scale, bar_scale)
  grob_mean_kill_years = ggplot_mean_kill_years(map_scale, bar_scale)
  grob_sd_kill_years = ggplot_sd_kill_years(map_scale, bar_scale)
  grob_pine_sum = ggplot_pine_sum(map_scale, bar_scale)
  grob_2k = ggplot_pine_surv(map_scale, bar_scale_2k, marg = 15 * map_scale,  point_size = 0.005, point_alpha = 0.1)
  
  png(file = "figures/winter_tmin_mean_red_blue.png", width = width_png * map_scale, height = height_png * map_scale)
  grob_mean
  dev.off()
  
  png(file = "figures/winter_tmin_mean_kill_years_red_blue.png", width = width_png * map_scale, height = height_png * map_scale)
  grob_mean_kill_years
  dev.off()
  
  
  png(file = "figures/winter_tmin_sd_red_blue.png", width = width_png * map_scale, height = height_png * map_scale)
  grob_sd
  dev.off()
  
  png(file = "figures/winter_tmin_sd_kill_years_red_blue.png", width = width_png * map_scale, height = height_png * map_scale)
  grob_sd_kill_years
  dev.off()
  
  png(file = "figures/cumulative_pine_kill_orange_red.png", width = width_png * map_scale, height = height_png * map_scale)
  print(grob_pine_sum)
  dev.off()
  
  
  png("figures/summary_maps_panels.png", width = map_scale * 2.5 * width_png, height = map_scale * height_png)
  grid.arrange(grob_mean, grob_sd_truncated, grob_pine_sum, nrow = 1)
  dev.off()
  
  png("figures/summary_maps_panels_kill_years.png", width = map_scale * 2.5 * width_png, height = map_scale * height_png)
  grid.arrange(grob_mean_kill_years, grob_sd_kill_years, grob_pine_sum, nrow = 1)
  dev.off()
  
  png("figures/kill_survival_2000.png", width = map_scale * width_png, height = map_scale * height_png)
  grob_2k
  dev.off()
  
  # Bigger files:
  
  map_scale = 5
  bar_scale = 1
  png(file = "figures/winter_tmin_mean_red_blue_big.png", width = width_png * map_scale, height = height_png * map_scale)
  ggplot_mean(map_scale, bar_scale)
  dev.off()
  
  png(file = "figures/winter_tmin_sd_red_blue_big.png", width = width_png * map_scale, height = height_png * map_scale)
  ggplot_sd(map_scale, bar_scale)
  dev.off()
  
  png(file = "figures/cumulative_pine_kill_orange_red_big.png", width = width_png * map_scale, height = height_png * map_scale)
  ggplot_pine_sum(map_scale, bar_scale)
  dev.off()
  
  
  png("figures/summary_maps_panels_big.png", width = map_scale * 2.5 * width_png, height = map_scale * height_png)
  grid.arrange(
    ggplot_mean(map_scale, bar_scale), 
    ggplot_sd(map_scale, bar_scale),
    ggplot_pine_sum(map_scale, bar_scale),
    nrow = 1)
  dev.off()

  png("figures/summary_maps_panels_kill_years_big.png", width = map_scale * 2.5 * width_png, height = map_scale * height_png)
  grid.arrange(
    ggplot_mean_kill_years(map_scale, bar_scale), 
    ggplot_sd_kill_years(map_scale, bar_scale),
    ggplot_pine_sum(map_scale, bar_scale),
    nrow = 1)
  dev.off()
  
  png("figures/kill_survival_2000_big.png", width = map_scale * width_png, height = map_scale * height_png)
  ggplot_pine_surv(map_scale, bar_scale_2k, marg = 15 * map_scale, point_size = 0.005, point_alpha = 0.1)
  dev.off()
  
}

# Figures with gtable ----

ggg_mean = ggplotGrob(ggplot_mean(map_scale, bar_scale))
ggg_sd_truncated = ggplotGrob(ggplot_sd_truncated(map_scale, bar_scale))
ggg_pine_kill = ggplotGrob(ggplot_pine_sum(map_scale, bar_scale))

g_all = cbind(ggg_mean, ggg_sd_truncated, ggg_pine_kill, size = "first")

png("figures/summary_maps_panels_big.png", width = map_scale * 2.5 * width_png, height = map_scale * height_png)
grid.newpage(); grid.draw(g_all)
dev.off()


