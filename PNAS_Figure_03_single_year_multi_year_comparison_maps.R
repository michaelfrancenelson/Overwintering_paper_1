# Packages/environment -------
{
  source("Environment/Globals.R")
  source("Environment/helper_functions.R")
  
  require(raster)
  require(rasterVis)
  require(ggplot2)
  require(rgdal)
  require(ggpolypath)
  require(data.table)
  require(grid)
  require(gridExtra)
  require(gtable)
  
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
}

# Load data (uses only local data directory) ----------------
{
  
  # data table version of pine kill data (takes a few seconds):
  # Retrieve points as a data table for specific pine kill years
  # pine_kill_table = data.table(as.data.frame(pine_brick, xy = T, na.rm = T))
  # fwrite(pine_kill_table, file = paste0(local_dat_dir, "pine_kill_table.csv"))
  pine_kill_table = fread(paste0(local_dat_dir, "pine_kill_table.csv"))
  
  # small subset of pine kill data for testing, and make sure some are at the max value ----
  # pine_kill_table = pine_kill_table[sample(1:nrow(pine_kill_table), 100000)]
  # for (i in kill_years) pine_kill_table[sample(1:nrow(pine_kill_table), 20), paste0("X", i) := 99999]
  
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

# Plot params -----
{
  # Shared map plot parameters:
  source("Environment/maps_plot_params.R")
  
  # Save filename info:
  fig_num = "03"
  fig_name = "survival_kill_single_multi_maps"
  fig_dir = "PNAS_figures"
  fig_filename = paste0(fig_dir, "/Figure_", fig_num, "_", fig_name)
  
  # labels for the legends:
  leg_tit_pines = "Pines killed per hectare"
  leg_tit_surv  = "MPB Survival Percent"
  
  # Which years to show in the individual year plots
  surv_panel_years = as.character(2005 + 1:3)
  
  # Which year to show in the multi-year mean plot
  mean_surv_panel_year = surv_panel_years[3] + 3
  
  # point size for the pine kill data
  pine_point_size = 1.5
  
  # maximum pines killed per hectare for display. 
  # All values above this are all grouped into one color
  max_kill = 1000
  
  # number of years to include in the mean survival panels
  lookback = 10
  
  # labels/breaks for the legends
  breaks_pine_2k = c(2, 10, 100, 1000)
  labels_pine_2k = breaks_pine_2k
  breaks_surv = seq(0, 1, len = 6)
  labels_surv = paste0(100 * breaks_surv, "")
  
  # settings for the heat color ramp for legends and pine kill data
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
  
  # red/blue color ramp ----
  colours = rev(colorRampPalette(c(rgb(0.1, 0.1, 1), rgb(0.5, 0, 0.5), rgb(1, 0, 0)))(color_length)[indices])
  scale_pine_red_blue = scale_color_gradientn(
    colours = colours,
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  # heat color ramp ----
  colours = rev(heat.colors(color_length + n_truncate))[-c(1:n_truncate)][sort(unique(indices))]
  scale_pine_heat  = scale_color_gradientn(
    colours = colours,
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  # Color ramp for the survival maps ----
  colors_surv_terrain = terrain.colors(10)[-c(5:7)]
  colors_surv_gray = gray(seq(0.4, 0.85, len = 100))
  colors_surv = colors_surv_gray
  
  scale_fill_surv = scale_fill_gradientn(
    colours = colors_surv, 
    na.value = rgb(0, 0, 0, 0), 
    breaks = breaks_surv, 
    labels = labels_surv)
  scale_col_surv = scale_color_gradientn(
    colours = colors_surv, 
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
    color = state_boundary_col,
    fill = rgb(0, 0, 0, 0))
}

#  Panel titles ----
{
  title_aspect_ratio = 1
  panel_titles = c(surv_panel_years, paste0(mean_surv_panel_year, "\n(", lookback, " year mean survival)"))
  print(panel_titles)
  
  sc_y_title = scale_y_continuous(expand = c(0, 0), limits = c(0.5, 1.5))
  coord_title = coord_equal(ratio = legend_aspect_ratio)
  
  title_size = 12
  
  panel_titles_grob_list = function(title_size)
  {
    title_gg_list = lapply(1:4, function(i)
      ggplot(data.frame(x = 0, y = 0)) + 
        annotate(geom = "text", x = 0, y = 0,
                 label = panel_titles[i], size = title_size) +
        # sc_y_title + 
        # coord_title + 
        theme_void() 
    )# theme(text = element_text(size = 16))
    
    title_grob_list = lapply(1:4, function(x) ggplotGrob(title_gg_list[[x]]))
    # title_grob_list[["size"]] = "first"
    
    # title_row = do.call(cbind, args = title_grob_list)
    # return(title_row)
    return(title_grob_list)
  }
  
  panel_titles_row = function(title_size)
  {
    g_list = panel_titles_grob_list(title_size)
    g_list[["size"]] = "first"
  }
}

# survival gplot rasters ----
{
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
}

# pine kill geom_point objects ----
{
  pine_log_gpoint_list = list(
    pine_log_gpoint(surv_panel_years[1], sz = pine_point_size),
    pine_log_gpoint(surv_panel_years[2], sz = pine_point_size),
    pine_log_gpoint(surv_panel_years[3], sz = pine_point_size),
    pine_log_gpoint(mean_surv_panel_year, sz = pine_point_size)
  )
}

# Survival only map grobs ----
{
  surv_gg_list = 
    list(
      plot_map(surv_gplots[[as.character(surv_panel_years[1])]]) + scale_fill_surv,
      plot_map(surv_gplots[[as.character(surv_panel_years[2])]]) + scale_fill_surv,
      plot_map(surv_gplots[[as.character(surv_panel_years[3])]]) + scale_fill_surv,
      plot_map(surv_mean_gplots[[as.character(mean_surv_panel_year)]]) + scale_fill_surv
    )
  surv_grob_list = lapply(1:4, function(x) ggplotGrob(surv_gg_list[[x]]))
}

# Survival/kill map grobs ----
{
  pine_log_surv_gg_list_heat = lapply(1:4, function(x) surv_gg_list[[x]] + pine_log_gpoint_list[[x]] + scale_pine_heat)
  pine_log_surv_grob_list_heat = lapply(1:4, function(x) ggplotGrob(pine_log_surv_gg_list_heat[[x]]))
  
  pine_log_surv_gg_list_red_blue = lapply(1:4, function(x) surv_gg_list[[x]] + pine_log_gpoint_list[[x]] + scale_pine_red_blue)
  pine_log_surv_grob_list_red_blue = lapply(1:4, function(x) ggplotGrob(pine_log_surv_gg_list_red_blue[[x]]))
}

# Build figure  ----
{
  
  png_height = 2400
  png_aspect = 1.8
  
  dpi_adj = png_height / 72
  
  label_adj = 1.6 * dpi_adj
  tick_adj = 1.8 * dpi_adj
  title_adj = 0.75 * dpi_adj
  
  title_height = 1.1
  map_height = 5
  legend_height = 1.8
  
  layout_mx = matrix(rbind(
    c(1, 2, 3, 4), # the titles
    c(5, 6, 7, 8), # the survival only maps
    c(9, 10, 11, 12), # the survival/kill maps
    c(13, 13, 14, 14) # the legends
  ), nrow = 4)
  
  gt_heights = c(unit(title_height, "npc"), unit(map_height, "npc"), unit(map_height, "npc"), unit(legend_height, "npc"))
  
  gt_red_blue = arrangeGrob(grobs = c(
    panel_titles_grob_list( title_size * 0.001 * png_height),
    surv_grob_list[1:4],
    pine_log_surv_gg_list_red_blue[1:4],
    list(
      ggplotGrob(legend_default(leg_tit_surv, scale_col_surv, breaks_surv, labels_surv)),
      ggplotGrob(legend_default(leg_tit_pines, scale_pine_red_blue, breaks_pine_2k, labels_pine_2k, log = T)))),
    layout_matrix = layout_mx
  )
  
  gt_heat = arrangeGrob(
    grobs =c(
      panel_titles_grob_list( title_size * 0.001 * png_height),
      surv_grob_list[1:4],
      pine_log_surv_gg_list_heat[1:4],
      list(
        ggplotGrob(legend_default(leg_tit_surv, scale_col_surv, breaks_surv, labels_surv)),
        ggplotGrob(legend_default(leg_tit_pines, scale_pine_heat, breaks_pine_2k, labels_pine_2k, log = T)))),
    layout_matrix = layout_mx
  )

  gt_red_blue$heights = gt_heat$heights = gt_heights
  
  {
    png(paste0(fig_filename, "_red_blue_gray_2005.png"), width = png_aspect * png_height, height = png_height)
    grid.newpage()
    # plot(gt_red_blue)
    grid.draw(gt_red_blue)
    dev.off()
  }
  {
    png(paste0(fig_filename, "_heat_gray_2005.png"), width = png_aspect * png_height, height = png_height)
    grid.newpage()
    # plot(gt_heat)
    grid.draw(gt_heat)
    dev.off()
  }
}
