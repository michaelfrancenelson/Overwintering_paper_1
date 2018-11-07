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
  
  lat_lon = spTransform(readOGR(dsn = paste0(local_dat_dir, "lat_long_us"), layer = "lat_long_us"), CRS(proj4_master))
  lon_lat_lines = subset(lat_lon, DEGREE5 == "Y")
  
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

# Convenience functions: ----
{
  plot_map = function(gg)
  {
    gg + gg_lat_lon + g_rast + states_poly_gray_1 + t_map + coords_map_panel
  } 
  # create a scaled legend for the survival
  scaled_legend = function(leg_gg, label_size = 40, tick_size = 20)
  {
    t_leg = t_leg_1(label_size, tick_size)
    leg_gg + t_leg
    # ggplot(leg_band_data, aes(x = surv, y = y, color = surv)) +
    #   line_leg + t_leg + sc_y_leg +
    #   coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$surv) - min(leg_band_data$surv))) +
    #   scale_col_surv +
    #   scale_x_continuous(breaks = breaks_surv, labels = 100 * breaks_surv) +
    #   xlab("MPB Survival Percent") 
  }
}


# plot params ---------------
{
  # Shared map plot parameters:
  source("maps_plot_params.R")
  
  # Save filename info:
  fig_num = "XX"
  fig_name = "Summary_stats_maps"
  fig_dir = "PNAS_figures"
  fig_filename = paste0(fig_dir, "/Figure_", fig_num, "_", fig_name)
  
  # Legend labels and breaks:
  breaks_mean = c( -35, -15, 0, 4)
  labels_mean = c( -35, -15, 0, 4)
  
  breaks_sd = 0:6
  labels_sd = c(0:5, ">6")
  
  # Some nice levels for the pine kill legend:
  scale_upper = plyr::round_any(max(pine_sum[] + 1, na.rm = T), 1e4)
  breaks_pine_sum = plyr::round_any(exp(seq(from = log(10), to = log(scale_upper), len = 4)), 100)
  breaks_pine_sum[1] = 10
  labels_pine_sum = breaks_pine_sum
  labels_pine_sum[length(labels_pine_sum)] = paste0(tail(breaks_pine_sum, 1), "+")
  
  # color scale for mean winter low ----
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
  scale_fill_mean =  scale_fill_gradientn(
    colours = cols_mean, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_mean, labels = breaks_mean)
  scale_color_mean = scale_color_gradientn(
    colours = cols_mean, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_mean, labels = breaks_mean)
  
  # color scale for winter low sd ----
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
  scale_fill_sd = scale_fill_gradientn(
    colours = cols_sd, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_sd, labels = breaks_sd)
  scale_color_sd = scale_color_gradientn(
    colours = cols_sd, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_sd, labels = breaks_sd)
  
  # color scale for pine kill ----
  ramp_red_pine_sum = colorRampPalette(c(
    rgb(1, 0.75, 0),
    rgb(1, 0.35, 0),
    rgb(1, 0.05, 0),
    rgb(1, 0, 0),
    rgb(0.35, 0, 0)))
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
  
  scale_fill_kill_log = scale_fill_gradientn(
    colours = cols_pine, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum, trans = "log")
  scale_color_kill_log = scale_color_gradientn(
    colours = cols_pine, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum, trans = "log")
  
  scale_fill_kill = scale_fill_gradientn(
    colours = cols_pine, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum)
  scale_color_kill = scale_color_gradientn(
    colours = cols_pine, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum)
  
  
  scale_color_kill_2 = scale_fill_gradientn(
    colours = cols_pine_2, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_sum, labels = labels_pine_sum, trans = "log")
}

# lat/long, states ----
{
  gg_lat_lon = geom_polypath(data = lon_lat_lines, aes(long, lat, group = group), color = lon_lat_col, fill = rgb(0,0,0,0))
  
  # plot the state outlines in translucent black
  states_poly_gray_1 = geom_polygon(
    data = states_poly,
    aes(long, lat, group = group),
    color = state_boundary_col,
    fill = rgb(0, 0, 0, 0))
}

# Mean winter low ----
{
  mean_gg = gplot(mean_winter_tmin, maxpixels = maxpixels)
  mean_grob = ggplotGrob(plot_map(mean_gg) + scale_fill_mean)
}

# SD winter low ----
{
  sd_gg = gplot(sd_winter_tmin, maxpixels = maxpixels)
  sd_grob = ggplotGrob(plot_map(sd_gg) + scale_fill_sd)
}

# Pine kill sums ----
{
  sum_gg = gplot(pine_gg_sum, maxpixels = maxpixels)
  sum_grob = ggplotGrob(plot_map(sum_gg) + scale_fill_kill)
}


# legends ----
{
  # Legend titles
  leg_tit_mean = paste0("mean winter minimum temp (°C)")
  leg_tit_sd = paste0("standard deviation of winter minimum temp (°C)")
  leg_tit_pine_sum = paste0("cumulative MPB pine mortality")
  
  n_leg_pts = 1000
  
  leg_band_data = data.table(
    y = 1, 
    mean = seq(breaks_mean[1], tail(breaks_mean, 1), len = n_leg_pts),
    sd   = seq(breaks_sd[1], tail(breaks_sd, 1), len = n_leg_pts),
    kill = seq(breaks_pine_sum[1], tail(breaks_pine_sum, 1), len = n_leg_pts),
    kill_log = log(seq(breaks_pine_sum[1], tail(breaks_pine_sum, 1), len = n_leg_pts))
  )
  leg_band_data
  breaks_mean
  
  coord_leg = coord_equal(ratio = legend_aspect_ratio)
  
  line_leg = geom_line(size = legend_band_width, show.legend = F)
  sc_y_leg = scale_y_continuous(expand = c(0, 0), limits = c(0.9, 1.1))
  
  leg_mean = ggplot(leg_band_data, aes(x = mean, y = y, color = mean)) +
    line_leg +
    # t_leg + 
    sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$mean) - min(leg_band_data$mean))) +
    scale_color_mean +
    scale_x_continuous(breaks = breaks_mean, labels = labels_mean, 
                       sec.axis = sec_axis(~., breaks = breaks_mean, labels = labels_mean)) +
    xlab(leg_tit_mean) 
  
  leg_sd = ggplot(leg_band_data, aes(x = sd, y = y, color = sd)) +
    line_leg +
    # t_leg + 
    sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$sd) - min(leg_band_data$sd))) +
    scale_color_sd +
    scale_x_continuous(breaks = breaks_sd, labels = labels_sd, sec.axis = sec_axis(~.)) +
    xlab(leg_tit_sd) 
  
  leg_kill = ggplot(leg_band_data, aes(x = kill, y = y, color = kill)) +
    line_leg +
    # t_leg + 
    sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$kill) - min(leg_band_data$kill))) +
    scale_color_kill +
    scale_x_continuous(breaks = breaks_pine_sum, labels = labels_pine_sum, 
                       sec.axis = sec_axis(~., breaks = breaks_pine_sum, labels = labels_pine_sum)) +
    xlab(leg_tit_pine_sum) 
  
  log(max(leg_band_data$kill) - min(leg_band_data$kill))
  
  leg_kill_log = ggplot(leg_band_data, aes(x = kill, y = y, color = kill)) +
    line_leg +
    # t_leg + 
    sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$kill_log) - min(leg_band_data$kill_log))) +
    # coord_equal(ratio = (log(legend_aspect_ratio) * log(max(leg_band_data$kill_log) - min(leg_band_data$kill_log)))) +
    # coord_equal(ratio = (log(legend_aspect_ratio) * log(max(leg_band_data$kill) - min(leg_band_data$kill)))) +
    scale_color_kill +
    scale_x_continuous(breaks = breaks_pine_sum, labels = labels_pine_sum, trans = "log", 
                       sec.axis = sec_axis(~., labels = labels_pine_sum, breaks = breaks_pine_sum)) +
    # scale_x_continuous(breaks = breaks_pine_sum, labels = labels_pine_sum) +
    xlab(leg_tit_pine_sum) 
  
  scaled_legend(leg_kill_log, 10, 10)
}

# build figure ----
{
  
  png_height = 2000
  png_aspect = 2.2
  
  dpi_adj = png_height / 72
  
  map_height = 4
  legend_height = 1.2
  
  
  
  
  layout_mx = matrix(rbind(
    c(1, 2, 3), # the maps
    c(4, 5, 6) # the legends
  ), nrow = 2)
  
  label_adj = 2.2 * dpi_adj
  tick_adj = 2 * dpi_adj
  
  gp_all = list(
    mean_grob,
    sd_grob,
    sum_grob,
    ggplotGrob(scaled_legend(leg_mean, label_adj, tick_adj)),
    ggplotGrob(scaled_legend(leg_sd, label_adj, tick_adj)),
    ggplotGrob(scaled_legend(leg_kill_log, label_adj, tick_adj))
  )
  
  gt_all = arrangeGrob(grobs = gp_all, layout_matrix = layout_mx)
  
  map_height = 5
  legend_height = 1
  
  gt_all$heights = gt_all$heights = 
    c(
      unit(map_height, "npc"),
      unit(legend_height, "npc")
    )
  
  png_height = 2000
  png_aspect = 2.7
  {
    
    
    # png("PNAS_Figures/figure_XX_test.png", width = png_aspect * png_height, height = png_height)
    # grid.newpage()
    # plot(gt_all)
    # dev.off()
    
    png(paste0(fig_filename, ".png"), width = png_aspect * png_height, height = png_height)
    grid.newpage()
    grid.draw(gt_all)
    dev.off()
  }
  
}
