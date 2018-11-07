# Packages  -------
{
  source("Globals.R")
  
  require(raster)
  require(rasterVis)
  require(ggplot2)
  require(rgdal)
  require(ggpolypath)
  require(data.table)
  require(grid)
  require(gridExtra)
  require(gtable)
}

# Load data (uses only local data directory) ----------------
{
  # national_forests_poly = readOGR(dsn = paste0(spatial_data_output_dir, "national_forests_in_mpb_states"), layer = "national_forests_in_mpb_states")
  
  states_poly = readOGR(dsn = paste0(local_dat_dir, "mpb_states"), layer = "mpb_states")
  
  
  # pre-make ggplot objects for the polygons
  # g_nf = geom_polypath(data = national_forests_poly, aes(long, lat, group = group), color = 1, fill = rgb(0, 0, 0, 0))
  # g_st = geom_polygon(data = states_poly, aes(long, lat, group = group), color = 1, fill = rgb(0, 0, 0, 0))
  
  # Individual years' survival and pine kill:
  survival_brick = brick(paste0(local_dat_dir, "mpb_overwinter_survival_brick.nc"), crs = proj4_master)
  pine_brick = brick(paste0(local_dat_dir, "pine_kill_western_US_brick.nc"), crs = proj4_master)
  names(survival_brick) = daymet_years[-1]
  names(pine_brick) = kill_years
  
  # summary stat rasters:
  pine_sum = raster(paste0(local_dat_dir, "pine_sum.nc"),crs = proj4_master)
  
  # we want NA's in cells with zero pine kill:
  pine_sum[pine_sum[] == 0] = NA
  
  
  lat_lon = spTransform(readOGR(dsn = paste0(local_dat_dir, "lat_long_us"), layer = "lat_long_us"), CRS(proj4_master))
  lon_lat_lines = subset(lat_lon, DEGREE5 == "Y")
  
  
  # mean_winter_tmin = raster(paste0(local_dat_dir, "mean_winter_tmin.nc"), crs = proj4_master)
  # sd_winter_tmin = raster(paste0(local_dat_dir, "sd_winter_tmin.nc"), crs = proj4_master)
  # mean_winter_tmin_kill_years = raster(paste0(local_dat_dir, "mean_winter_tmin_kill_years.nc"), crs = proj4_master)
  # sd_winter_tmin_kill_years = raster(paste0(local_dat_dir, "sd_winter_tmin_kill_years.nc"), crs = proj4_master)
}



# Plot params -----
{
  lon_lat_col = gray(0.8, 0.4)
  
  maxpixels = 1e6
  g_rast_legend = geom_raster(aes(fill = value), show.legend = T)
  g_rast_no_legend = geom_raster(aes(fill = value), show.legend = F)
  
  legend.margin = margin(0, 15, 0, 15)
  legend.box.margin = margin(-25, 40, 0, 40)
  
  legend_bar_base_width = 18
  legend_bar_base_height = 1.5
  
  t1 = theme(axis.text = element_blank(), axis.ticks = element_blank(), 
             panel.grid = element_blank(), panel.background = element_blank(),
             panel.border = element_rect(fill = NA),
             axis.title = element_blank())
  
  coords_square_panel = coord_equal(xlim = c(-2e6, 1e5), ylim = c(-1.1e6, 0.85e6), ratio = 1)
  
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
  cols_pine_2 = c(ramp_yellow_pine_sum(5), ramp_red_pine_sum_2(12))
  # Colors
  
  guide_colorbar_below_pine_sum = guides(
    fill = guide_colourbar(
      title.position = "bottom", 
      title.hjust = 0.5,
      label.position = "top"
    ))
  
  guide_colorbar_below_surv = guides(
    fill = guide_colourbar(
      title.position = "bottom",
      title.hjust = 0.5,
      label.position = "top"))
  
  breaks_pine_2k = c(2, 10, 100, 1000, 16000)
  scale_color_pine = scale_color_gradientn(
    colours = cols_pine_2, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_2k, labels = breaks_pine_2k, trans = "log")
  
  breaks_surv = seq(0, 1, len = 6)
  labels_surv = paste0(100 * breaks_surv, "")
  
  cols_surv = terrain.colors(10)[-c(5:7)]
  
  scale_fill_surv = scale_fill_gradientn(
    colours = cols_surv, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_surv, 
    labels = labels_surv)
  
  scale_color_pine_1 = scale_color_gradientn(
    colours = cols_pine_2,
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  scale_color_pine_2 = scale_color_gradientn(
    colours = heat.colors(10),
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  scale_color_pine_3 = scale_color_gradientn(
    colours = rainbow(10)[-10],
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  # plot the state outlines in translucent black
  states_poly_gray_1 = geom_polygon(
    data = states_poly, 
    aes(long, lat, group = group), 
    color = gray(0, alpha = 0.15), 
    fill = rgb(0, 0, 0, 0))
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


# lat/long ----
gg_lat_lon = geom_polypath(data = lon_lat_lines, aes(long, lat, group = group), color = lon_lat_col, fill = rgb(0,0,0,0))


# Kill and survival in separate panels -----
{
  
  # Retrieve points for specific pine kill years
  get_pine_gg = function(kill_year, sz = 0.01, alph = 0.5, legend = F)
  {
    dtt = data.table(as.data.frame(subset(pine_brick, which(grepl((kill_year + 1), names(pine_brick)))), xy = T, na.rm = T))
    names(dtt)[3] = "kill"
    dtt = dtt[kill > 0, ]
    dtt = dtt[kill > 0.5, ]
    dtt[kill < 1, ]
    dtt[, kill1 := sapply(kill, function(x) max(1, x))]
    dtt[, kill1 := sapply(kill, function(x) min(x, 16000))]
    dtt[, kill_log := log(kill1)]; #dtt[, summary(kill_log)]
    return (  geom_point(data = dtt, aes(x, y, color = kill_log), size = sz, alpha = alph, show.legend = legend))
  }
  
  
  # gplot objects for various survival years
  surv_layers = 1998:2009
  
  # Grobs of survival ----
  surv_grobs = lapply(
    which(daymet_years %in% surv_layers), 
    function(x) gplot(subset(survival_brick, x), maxpixels = maxpixels) + 
      gg_lat_lon + g_rast_no_legend + states_poly_gray_1 +
      t1 + coords_square_panel + scale_fill_surv)
  names(surv_grobs) = as.character(surv_layers)
  surv_grobs_legend = lapply(
    which(daymet_years %in% surv_layers), 
    function(x) gplot(subset(survival_brick, x), maxpixels = maxpixels) + 
      gg_lat_lon + g_rast_legend + states_poly_gray_1 +
      t1 + coords_square_panel + scale_fill_surv)
  names(surv_grobs_legend) = as.character(surv_layers)
  
  # Grobs of kill plots ----
  get_kill_grob = function(kill_year) 
  {
    ggplot() + gg_lat_lon + get_pine_gg(kill_year) + states_poly_gray_1 +
      t1 + coords_square_panel + 
      guide_colorbar_below_pine_sum + 
      scale_color_pine_3
    # scale_color_pine_2
  }
  
  get_kill_grob_2 = function(kill_year, colors = heat.colors(10)[-10], legend = F) 
  {
    ggplot() + gg_lat_lon + get_pine_gg(kill_year, legend = legend) + states_poly_gray_1 +
      t1 + coords_square_panel + 
      guide_colorbar_below_pine_sum + 
      scale_color_gradientn(
        colours = colors,
        na.value = rgb(0, 0, 0, 0),
        breaks = log(breaks_pine_2k),
        labels = breaks_pine_2k)
    
    # scale_color_pine_2
  }
  kill_grobs = lapply(kill_years - 1, get_kill_grob)
  names(kill_grobs) = as.character(kill_years)
  
  plot_year = 2001
  get_kill_grob_2(1996, colors = rainbow(10)[-c(1:2)], legend = T) + theme(legend.position = "bottom")
  
  get_kill_grob(2000)
  
  kill_grobs[[as.character(plot_year)]]
  kill_grobs[[as.character(plot_year + 1)]]
  
  ggplot() + gg_lat_lon + get_pine_gg(2000) + coords_square_panel
  ggplot() + gg_lat_lon + get_pine_gg(2001) + coords_square_panel
  
  kill_row = cbind(
    ggplotGrob(get_kill_grob_2(plot_year, colors = rainbow(10)[-c(1:2)], legend = T) + theme(legend.position = "bottom") + ggtitle(plot_year)),
    ggplotGrob(get_kill_grob_2(plot_year + 1, colors = rainbow(10)[-c(1:2)], legend = T) + theme(legend.position = "bottom") + ggtitle(plot_year + 1)),
    ggplotGrob(get_kill_grob_2(plot_year + 2, colors = rainbow(10)[-c(1:2)], legend = T) + theme(legend.position = "bottom") + ggtitle(plot_year + 2)),
    # ggplotGrob(kill_grobs[[as.character(plot_year)]] + ggtitle(plot_year)),
    # ggplotGrob(kill_grobs[[as.character(plot_year + 1)]] + ggtitle(plot_year + 1)),
    # ggplotGrob(kill_grobs[[as.character(plot_year + 2)]] + ggtitle(plot_year + 2)),
    size = "first"
  )
  surv_row = cbind(
    ggplotGrob(surv_grobs[[as.character(plot_year)]]),
    ggplotGrob(surv_grobs[[as.character(plot_year + 1)]]),
    ggplotGrob(surv_grobs[[as.character(plot_year + 2)]]),
    size = "first"
  )
  surv_row = cbind(
    ggplotGrob(surv_grobs_legend[[as.character(plot_year)]] + theme(legend.position = "bottom") ),
    ggplotGrob(surv_grobs_legend[[as.character(plot_year + 1)]] + theme(legend.position = "bottom") ),
    ggplotGrob(surv_grobs_legend[[as.character(plot_year + 2)]] + theme(legend.position = "bottom") ),
    size = "first"
  )
  
  
  grid.draw(kill_row)
  grid.draw(surv_row)
  
  grid.draw(rbind(kill_row, surv_row, size = "first"))
  
  png(width = 1800, height = 1300, file = "figures/kill_surv_rows.png")
  grid.draw(rbind(kill_row, surv_row, size = "first"))
  dev.off()
  
  
}




# Year 2000 maps -------------------------------
{
  pine_red_stage_year = 2001; 
  pine_kill_year = pine_red_stage_year - 1
  
  # red-stage in 2000, kill from previous summer.
  pine_layer = which(kill_years == pine_red_stage_year)
  surv_layer = which(daymet_years == pine_kill_year)  
  
  plot(subset(pine_brick, pine_layer))
  plot(subset(survival_brick, surv_layer))
  
  pine_2k = subset(pine_brick, pine_layer)
  pine_2k[pine_2k[] == 0] = NA
  
  # This already has NA values outside the mask
  surv_2k = subset(survival_brick, surv_layer)
  
  
  
  
  
  
  gp_surv[[as.character(surv_year)]] + g_rast
  
  gp_surv
  
  surv_year = 2006
  
  which(daymet_years %in% surv_year)
  
  surv_2k = subset(survival_brick, surv_layer)
  gplot_surv_2k = gplot(surv_2k, maxpixels = maxpixels)
  
  # I can'f figure out how to overplot one raster on top of another with a different color scale,
  # so use a hack of converting the pine to a data frame and plot with a square-shaped point
  pine_2k_df = data.table(raster::as.data.frame(pine_2k, xy = T))
  names(pine_2k_df)[3] = "kill"
  
  pine_kill_year
  
  dtt = data.table(as.data.frame(subset(pine_brick, which(grepl((pine_kill_year + 1), names(pine_brick)))), xy = T, na.rm = T))
  names(dtt)[3] = "dat"
  dtt = dtt[dat > 0, ]
  
  data.table(as.data.frame((pine_kill_year + 1) %in% names(pine_brick), xy = T))
  pine_brick
  data.table(as.data.frame(pine_brick, xy = T))
  
  
  
  
  
  # eliminate the NAs and the zeroes (they cause problems with the log scaling below)
  pine_2k_df = pine_2k_df[complete.cases(pine_2k_df),,]
  pine_2k_df = pine_2k_df[kill > 1]
  
  # new scale and breaks needed
  boxplot(pine_2k_df$kill)
  summary(pine_2k_df$kill)
  breaks_pine_2k = c(2, 10, 100, 1000, 16000)
  
  gplot_surv_2k = gplot(surv_2k, maxpixels = maxpixels)
  ggplot_pine_2k = ggplot(pine_2k_df, aes(x, y, color = kill))
  
  # Legend color bar options:
  leg_tit_surv = "% MPB overwinter survival"
  leg_tit_pine_2k = "MPB pine mortality"
  
  
  scale_color_pine = scale_color_gradientn(
    colours = cols_pine_2, na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_pine_2k, labels = breaks_pine_2k, trans = "log")
  
  scale_color_pine = scale_color_gradientn(
    colours = cols_pine_2, na.value = rgb(0, 0, 0, 0), trans = "log")
  
  breaks_pine_2k = c(1, 10, 100, 1000, 16000)
  log(breaks_pine_2k)
  
  pine_kill_max = 1.6e4
  
  year = 2000
  dtt = data.table(as.data.frame(subset(pine_brick, which(grepl((year + 1), names(pine_brick)))), xy = T, na.rm = T))
  names(dtt)[3] = "kill"
  dtt = dtt[kill > 0, ]; dtt[, summary(kill)]
  dtt = dtt[kill > 0.5, ]; dtt[, summary(kill)]
  dtt[kill < 1, ]
  dtt[, kill1 := sapply(kill, function(x) max(1, x))]
  dtt[, kill_log := log(kill1)]; dtt[, summary(kill_log)]
  
  ggplot() + 
    states_poly_gray_1 +
    geom_point(data = dtt, aes(x, y, color = kill_log), size = 0.01, alpha = 0.5) +
    scale_color_gradientn(
      colours = cols_pine_2, na.value = rgb(0, 0, 0, 0),
      breaks = log(breaks_pine_2k), labels = breaks_pine_2k)
  
  
  map_scale = 1
  
  gd_1 =  guides(
    color = guide_colourbar(
      title = leg_tit_pine_2k,
      title.position = "bottom", 
      title.hjust = 0.5,
      barheight = 1,
      barwidth = 5))
  
  
  
  # ,
  # label.position = "top"))
  
  
  
  ggplot() + t1 + coord_equal(xlim = c(-2e6, 0.2e6), ylim = c(-1.2e6, 1e6)) + 
    guide_colorbar_below_pine_sum + scg_1 +
    get_pine_gg(2001) + states_poly_gray_1 + gg_lat_lon
  
  
  ggplot_pine_surv = function(
    # surv_year = 2006,
    # pine_kill_year = 2000,
    year = 2000,
    map_scale, bar_scale = 3, 
    marg = 25,
    legend_bar_base_height = 1.5, legend_bar_base_width = 18,
    point_size = 0.01,
    point_alpha = 0.5)
  {
    dtt = data.table(as.data.frame(subset(pine_brick, which(grepl((year + 1), names(pine_brick)))), xy = T, na.rm = T))
    names(dtt)[3] = "kill"
    dtt = dtt[kill > 0, ]
    
    surv_grobs[[as.character(year)]] +
      # gplot_surv_2k + 
      g_rast_no_legend + scale_fill_surv + # coord_fixed() +
      states_poly_gray_1 + 
      # geom_point(data = pine_2k_df, aes(x, y, color = kill), size = point_size, alpha = point_alpha) + 
      geom_point(data = dtt, aes(x, y, color = kill), size = point_size, alpha = point_alpha) + 
      scale_color_pine +
      scaled_theme(map_scale, marg) +
      guides(
        fill = guide_colourbar(
          title = leg_tit_surv,
          title.position = "bottom",
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top"),
        color = guide_colourbar(
          title = leg_tit_pine_2k,
          title.position = "bottom", 
          title.hjust = 0.5,
          barheight = map_scale * legend_bar_base_height / bar_scale,
          barwidth = map_scale * legend_bar_base_width / bar_scale,
          label.position = "top")) +
      ggtitle(year)
  }
}

ggplot_pine_surv(map_scale = 1, year = 2000)


png(height = 700, width = 600, file = "figures/overplot_example.png")
ggplot_pine_surv(map_scale = 1, year = 2000)
dev.off()

