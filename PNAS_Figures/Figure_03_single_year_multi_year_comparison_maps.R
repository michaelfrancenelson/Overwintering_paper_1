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
  # data table version of pine kill data (takes a few seconds):
  # Retrieve points as a data table for specific pine kill years
  # pine_kill_table = data.table(as.data.frame(pine_brick, xy = T, na.rm = T))
  # fwrite(pine_kill_table, file = paste0(local_dat_dir, "pine_kill_table.csv"))
  pine_kill_table = fread(paste0(local_dat_dir, "pine_kill_table.csv"))
  
  # state borders
  states_poly = readOGR(dsn = paste0(local_dat_dir, "mpb_states"), layer = "mpb_states")
  
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
}


# Convenience functions ----
{
  
  # Convenience function to plot a single year of survival
  plot_surv_gg = function(year, gplots)
  {
    gplots[[as.character(year)]] + 
      gg_lat_lon + g_rast_no_legend + states_poly_gray_1 +
      t1 + coords_square_panel + scale_fill_surv
  }  
  # Convenience function to grab the pine kill data for a specific year
  get_pine_year_data = function(year, keep_threshold = 0.5, min_kill = 1, max_kill = 16000)
  { 
    # pine_kill_table[get(paste0("X", 2001)) > keep_threshold, .(x, y, kill = get(paste0("X", year)))]
    
    dtt = pine_kill_table[
      get(paste0("X", year)) > keep_threshold, 
      .(x, y, kill = get(paste0("X",year)))]
    kill1 = sapply(dtt$kill, function(x) max(min_kill, x))
    kill1 = sapply(kill1, function(x) min(x, max_kill))
    dtt[, kill := kill1]
    dtt[, kill_log := log(kill1)]; #dtt[, summary(kill_log)]
    return(dtt)
  }
  
  # Convenience function to get a ggplot object of pine kill for a specific year
  pine_log_gpoint = function(kill_year, sz = 0.01, alph = 0.5, legend = F, keep_threshold = 0.5, min_kill = 1, max_kill = 16000)
  {
    dtt = get_pine_year_data(kill_year, max_kill = max_kill)
    names(dtt)[3] = "kill"
    dtt = dtt[kill > keep_threshold, ]
    dtt[, kill1 := sapply(kill, function(x) max(min_kill, x))]
    dtt[, kill1 := sapply(kill, function(x) min(x, max_kill))]
    dtt[, kill_log := log(kill1)]; #dtt[, summary(kill_log)]
    
    return(geom_point(data = dtt[dtt[, order(kill)],], 
                      aes(x, y, color = kill_log), 
                      size = sz, alpha = alph, show.legend = legend))
  }
  
  # Create a custom theme given the input font sizes
  t_leg_1 = function(label_size, tick_size)
  {
    return (
      theme(
        panel.background = element_blank(), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = label_size),
        axis.text.x = element_text(size = tick_size))
    )
  }
  
  # create a scaled legend for the survival
  leg_surv = function(label_size = 40, tick_size = 20)
  {
    t_leg = t_leg_1(label_size, tick_size)
    ggplot(leg_band_data, aes(x = surv, y = y, color = surv)) +
      line_leg + t_leg + sc_y_leg +
      coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$surv) - min(leg_band_data$surv))) +
      scale_col_surv +
      scale_x_continuous(breaks = breaks_surv, labels = 100 * breaks_surv) +
      xlab("MPB Survival Percent") 
  }
  
  # create a scaled legend for the survival/kill
  leg_kill = function(label_size = 40, tick_size = 20, color_scale)
  {
    t_leg = t_leg_1(label_size, tick_size)
    ggplot(leg_band_data, aes(x = kill, y = y, color = kill)) +
      line_leg + t_leg + sc_y_leg +
      coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$kill) - min(leg_band_data$kill))) +
      color_scale +
      scale_x_continuous(breaks = log(breaks_pine_2k), labels = breaks_pine_2k) +
      xlab("Pines killed per hectare") 
  }
  
  
}

# Plot params -----
{
  
  # Which years to show in the individual year plots
  surv_panel_years = as.character(c(2000, 2001, 2002))
  # Which year to show in the multi-year mean plot
  mean_surv_panel_year = 2002
  
  lon_lat_col = gray(0.8, 0.4)
  pine_point_size = 1.2
  
  # sets resolution for plotting the survival, use low value for faster speed
  maxpixels = 1e7
  max_kill = 1000
  
  lookback = 10
  
  g_rast_legend = geom_raster(aes(fill = value), show.legend = T)
  g_rast_no_legend = geom_raster(aes(fill = value), show.legend = F)
  
  # legend.margin = margin(0, 15, 0, 15)
  # legend.box.margin = margin(-25, 40, 0, 40)
  
  # legend_bar_base_width = 18
  # legend_bar_base_height = 1.5
  
  t1 = theme(axis.text = element_blank(), axis.ticks = element_blank(), 
             panel.grid = element_blank(), panel.background = element_blank(),
             panel.border = element_rect(fill = NA),
             # panel.border = element_blank(),
             axis.title = element_blank())
  
  coords_square_panel = coord_equal(xlim = c(-2e6, 1e5), ylim = c(-1.1e6, 0.85e6), ratio = 1)
  
  breaks_pine_2k = c(2, 10, 100, 1000)
  breaks_surv = seq(0, 1, len = 6)
  labels_surv = paste0(100 * breaks_surv, "")
  
  n_cols = 20
  color_length = 1000
  color_pow = 0.99
  truncate_pct = 0.075
  
  # Create a color ramp that has more colors at the bottom of the range
  n_truncate = round(truncate_pct * color_length)
  x = log(1:n_cols) / log(n_cols)
  x = ((1:n_cols) ^ color_pow) / (n_cols ^ color_pow)
  indices = round(color_length * x)
  indices = rev(round(color_length * x))
  
  colours = rev(colorRampPalette(c(rgb(0.1, 0.1, 1), rgb(0.5, 0, 0.5), rgb(1, 0, 0)))(color_length)[indices])
  scale_pine_red_blue = scale_color_gradientn(
    colours = colours,
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  colours = rev(heat.colors(color_length + n_truncate))[-c(1:n_truncate)][sort(unique(indices))]
  scale_pine_heat  = scale_color_gradientn(
    colours = colours,
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  
  cols_surv = terrain.colors(10)[-c(5:7)]
  
  scale_fill_surv = scale_fill_gradientn(
    colours = cols_surv, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_surv, 
    labels = labels_surv)
  scale_col_surv = scale_color_gradientn(
    colours = cols_surv, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_surv, 
    labels = labels_surv)
  
}


# lat/long, states ----
{
  gg_lat_lon = geom_polypath(data = lon_lat_lines, aes(long, lat, group = group), color = lon_lat_col, fill = rgb(0,0,0,0))
  
  # plot the state outlines in translucent black
  states_poly_gray_1 = geom_polygon(
    data = states_poly,
    aes(long, lat, group = group),
    color = gray(0, alpha = 0.15),
    fill = rgb(0, 0, 0, 0))
}

# legends ----
{
  leg_band_data = data.table(kill = seq(0, log(max_kill), len = 1000), surv = seq(0, 1, len = 1000), y = 1)
  
  legend_band_width = 15
  legend_aspect_ratio = 0.5
  coord_leg = coord_equal(ratio = legend_aspect_ratio)
  
  t_leg = theme(
    panel.background = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )
  line_leg = geom_line(size = legend_band_width, show.legend = F)
  sc_y_leg = scale_y_continuous(expand = c(0, 0), limits = c(0.9, 1.1))
  
  leg_kill_gg_heat = ggplot(leg_band_data, aes(x = kill, y = y, color = kill)) +
    line_leg + t_leg + sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$kill) - min(leg_band_data$kill))) +
    scale_pine_heat +
    scale_x_continuous(breaks = log(breaks_pine_2k), labels = breaks_pine_2k) +
    xlab("Pines killed per hectare") 
  
  leg_kill_gg_red_blue = ggplot(leg_band_data, aes(x = kill, y = y, color = kill)) +
    line_leg + t_leg + sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$kill) - min(leg_band_data$kill))) +
    scale_pine_red_blue +
    scale_x_continuous(breaks = log(breaks_pine_2k), labels = breaks_pine_2k) +
    xlab("Pines killed per hectare") 
  
  leg_surv_gg = ggplot(leg_band_data, aes(x = surv, y = y, color = surv)) +
    line_leg + t_leg + sc_y_leg +
    coord_equal(ratio = legend_aspect_ratio * (max(leg_band_data$surv) - min(leg_band_data$surv))) +
    scale_col_surv +
    scale_x_continuous(breaks = breaks_surv, labels = 100 * breaks_surv) +
    xlab("MPB Survival Percent") 
  
  # legend_row = cbind(ggplotGrob(leg_kill_gg), ggplotGrob(leg_surv_gg), size = "first")  
}

# survival gplot objects ----
surv_layers = 1998:2009
surv_gplots = lapply(
  which(daymet_years %in% surv_layers), 
  function(x) gplot(subset(survival_brick, x), maxpixels = maxpixels))
names(surv_gplots) = as.character(surv_layers)

# multi-year mean survivals (this takes a little time)
which(daymet_years %in% surv_layers)
surv_mean_gplots = lapply(
  which(daymet_years %in% surv_layers),
  function(x){
    print(x)  
    gplot(
      raster::calc(subset(survival_brick, subset = (x - lookback + 1):x), mean),
      maxpixels = maxpixels)
  } 
)
names(surv_mean_gplots) = as.character(surv_layers)

# pine kill ggplot objects ----
pine_log_gpoint_list = list(
  pine_log_gpoint(surv_panel_years[1], sz = pine_point_size),
  pine_log_gpoint(surv_panel_years[2], sz = pine_point_size),
  pine_log_gpoint(surv_panel_years[3], sz = pine_point_size),
  pine_log_gpoint(mean_surv_panel_year, sz = pine_point_size)
)



# Survival, kill rows ----

surv_gg_list = 
  list(
    plot_surv_gg(surv_panel_years[1], surv_gplots),
    plot_surv_gg(surv_panel_years[2], surv_gplots),
    plot_surv_gg(surv_panel_years[3], surv_gplots),
    plot_surv_gg(mean_surv_panel_year, surv_mean_gplots)    
  )

surv_grob_list = lapply(1:4, function(x) ggplotGrob(surv_gg_list[[x]]))
surv_grob_list[["size"]] = "first"

surv_row = do.call(cbind, args = surv_grob_list)

# survival/kill row
pine_log_surv_gg_list_heat = lapply(1:4, function(x) surv_gg_list[[x]] + pine_log_gpoint_list[[x]] + scale_pine_heat)
pine_log_surv_grob_list_heat = lapply(1:4, function(x) ggplotGrob(pine_log_surv_gg_list_heat[[x]]))
pine_log_surv_grob_list_heat[["size"]] = "first"
pine_log_surv_row_heat = do.call(cbind, args = pine_log_surv_grob_list_heat)

pine_log_surv_gg_list_red_blue = lapply(1:4, function(x) surv_gg_list[[x]] + pine_log_gpoint_list[[x]] + scale_pine_red_blue)
pine_log_surv_grob_list_red_blue = lapply(1:4, function(x) ggplotGrob(pine_log_surv_gg_list_red_blue[[x]]))
pine_log_surv_grob_list_red_blue[["size"]] = "first"
pine_log_surv_row_red_blue = do.call(cbind, args = pine_log_surv_grob_list_red_blue)


# grid.newpage()
# grid.draw(pine_log_surv_row_panels)

# Complete figure  ----
dpi_adj = png_height / 72

png_height = 1600
png_aspect = 2

height_1 = 7
height_2 = 1.2

maps_red_blue = rbind(surv_row, pine_log_surv_row_red_blue, size = "first")
maps_heat = rbind(surv_row, pine_log_surv_row_heat, size = "first")

legends_heat = cbind(
  ggplotGrob(leg_kill(dpi_adj * 2, dpi_adj * 1.8, scale_pine_heat)),
  ggplotGrob(leg_surv(dpi_adj * 2, dpi_adj * 1.8)),
  size = "first")

legends_red_blue = cbind(
  ggplotGrob(leg_kill(dpi_adj * 2, dpi_adj * 1.8, scale_pine_red_blue)),
  ggplotGrob(leg_surv(dpi_adj * 2, dpi_adj * 1.8)),
  size = "first")

fig_heat = rbind(arrangeGrob(maps_heat), arrangeGrob(legends_heat))
fig_red_blue = rbind(arrangeGrob(maps_red_blue), arrangeGrob(legends_red_blue))

fig_heat$heights = c(unit(height_1, "npc"), unit(height_2, "npc"))
fig_red_blue$heights = c(unit(height_1, "npc"), unit(height_2, "npc"))


png("PNAS_Figures/figure_3_heat.png", width = png_aspect * png_height, height = png_height)
grid.draw(fig_heat)
dev.off()


png("PNAS_Figures/figure_3_red_blue.png", width = png_aspect * png_height, height = png_height)
grid.draw(fig_red_blue)
dev.off()


