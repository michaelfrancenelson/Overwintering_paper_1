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
  surv_panel_years = as.character(2002 + 1:3)
  
  # Which year to show in the multi-year mean plot
  mean_surv_panel_year = 2005
  
  # color for the lon/lat lines
  lon_lat_col = gray(0.8, 0.4)
  
  # color for the state boundaries
  state_boundary_col = gray(0.3, alpha = 1)
  
  # point size for the pine kill data
  pine_point_size = 1.5
  
  # sets resolution for plotting the survival, use low value for faster speed
  # maxpixels = 1e2
  maxpixels = 1e7
  
  # maximum pines killed per hectare for display. 
  # All values above this are all grouped into one color
  max_kill = 1000
  
  # number of years to include in the mean survival panels
  lookback = 10
  
  # g_rast_legend = geom_raster(aes(fill = value), show.legend = T)
  g_rast_no_legend = geom_raster(aes(fill = value), show.legend = F)
  
  # Theme for the map panels
  t1 = theme(axis.text = element_blank(), axis.ticks = element_blank(), 
             panel.grid = element_blank(), panel.background = element_blank(),
             panel.border = element_rect(fill = NA),
             # panel.border = element_blank(),
             axis.title = element_blank())
  
  # x- and y- limits for the map plot panels
  coords_square_panel = coord_equal(xlim = c(-2e6, 1e5), ylim = c(-1.1e6, 0.85e6), ratio = 1)
  
  # labels/breaks for the legends
  breaks_pine_2k = c(2, 10, 100, 1000)
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
  
  # red/blue color ramp
  colours = rev(colorRampPalette(c(rgb(0.1, 0.1, 1), rgb(0.5, 0, 0.5), rgb(1, 0, 0)))(color_length)[indices])
  scale_pine_red_blue = scale_color_gradientn(
    colours = colours,
    na.value = rgb(0, 0, 0, 0),
    breaks = log(breaks_pine_2k),
    labels = breaks_pine_2k)
  
  # heat color ramp
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

#  Panel titles ----
{
  
  title_aspect_ratio = 1
  surv_panel_years
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
  # 
  # tit_test = rbind(panel_titles_row(12), panel_titles_row(14), size = "first")
  # tit_test$heights = c(unit(1, "npc"), unit(2, "npc"))  
  # class(tit_test)
  # tit_test
  # plot(tit_test)  
  # 
  # 
  # 
  # w <- list(unit(1,"null"), unit(1,"null"))
  # class(w) <-  c("unit.list", "unit")
  # h <- unit(1, "in")
  # gl1 <- grid.layout(1, 2, widths = w, heights = h,
  #                    respect = TRUE)
  # grid.newpage()
  # grid.show.layout(gl1) # fine
  # 
  # w2 <- w
  # w2[[1]] <- unit.pmax(unit(1,"null"), unit(1,"null"))
  # gl2 <- grid.layout(1, 2, widths = w2, heights = h,
  #                    respect = FALSE)
  # grid.newpage()
  # grid.show.layout(gl2)# fine
  # 
  # gl3 <- grid.layout(1, 2, widths = w2, heights = h,
  #                    respect = TRUE)
  # grid.newpage()
  # grid.show.layout(gl3)
  # 
  # 
  # grob_list = panel_titles_grob_list(1)
  # grob_list_2 = panel_titles_grob_list(2)  
  # 
  # lmx = layout_matrix = rbind(
  #   c(1, 1, 2, 2),
  #   c(3, 4, 5, 6),
  #   c(1, 1, 1, 1) * 7
  # )
  # 
  # grobs = grob_list
  # grob_list[[5]] = grob_list_2[[3]]
  # grob_list[[6]] = grob_list_2[[2]]
  # grob_list[[7]] = grob_list_2[[4]]
  # 
  # gt = arrangeGrob(grobs = grob_list, layout_matrix = lmx)    
  # gt$heights = c(
  #   unit(1, "npc"),
  #   unit(1, "npc"),
  #   unit(2, "npc")
  #   )
  # grid.newpage()
  # grid.draw(gt)    
  
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

# Survival only panel row ----
surv_gg_list = 
  list(
    plot_surv_gg(surv_panel_years[1], surv_gplots),
    plot_surv_gg(surv_panel_years[2], surv_gplots),
    plot_surv_gg(surv_panel_years[3], surv_gplots),
    plot_surv_gg(mean_surv_panel_year, surv_mean_gplots)    
  )

surv_grob_list = lapply(1:4, function(x) ggplotGrob(surv_gg_list[[x]]))
# surv_grob_list[["size"]] = "first"

surv_row = do.call(cbind, args = c(surv_grob_list, size = "first"))

# Survival/kill panel row ----
pine_log_surv_gg_list_heat = lapply(1:4, function(x) surv_gg_list[[x]] + pine_log_gpoint_list[[x]] + scale_pine_heat)
pine_log_surv_grob_list_heat = lapply(1:4, function(x) ggplotGrob(pine_log_surv_gg_list_heat[[x]]))
# pine_log_surv_grob_list_heat[["size"]] = "first"

pine_log_surv_gg_list_red_blue = lapply(1:4, function(x) surv_gg_list[[x]] + pine_log_gpoint_list[[x]] + scale_pine_red_blue)
pine_log_surv_grob_list_red_blue = lapply(1:4, function(x) ggplotGrob(pine_log_surv_gg_list_red_blue[[x]]))
# pine_log_surv_grob_list_red_blue[["size"]] = "first"

pine_log_surv_row_heat = do.call(cbind, args = c(pine_log_surv_grob_list_heat, size = "first"))
pine_log_surv_row_red_blue = do.call(cbind, args = c(pine_log_surv_grob_list_red_blue, size = "first"))


# grid.newpage()
# grid.draw(pine_log_surv_row_panels)

# Complete figure  ----
{
  
  png_height = 1600
  png_aspect = 2.2
  
  dpi_adj = png_height / 72
  
  height_1 = 8
  height_2 = 1.12
  
  title_height = 1
  map_height = 4
  legend_height = 1.2
  
  
  layout_mx = matrix(rbind(
    c(1, 2, 3, 4), # the titles
    c(5, 6, 7, 8), # the survival only maps
    c(9, 10, 11, 12), # the survival/kill maps
    c(13, 13, 14, 14) # the legends
  ), nrow = 4)
  
  gp_red_blue = c(
    panel_titles_grob_list( title_size * 0.001 * png_height),
    surv_grob_list[1:4],
    pine_log_surv_gg_list_red_blue[1:4],
    list(ggplotGrob(leg_kill(dpi_adj * 2, dpi_adj * 1.8, scale_pine_red_blue)),
         ggplotGrob(leg_surv(dpi_adj * 2, dpi_adj * 1.8)))
  )
  
  gp_heat = c(
    panel_titles_grob_list( title_size * 0.001 * png_height),
    surv_grob_list[1:4],
    pine_log_surv_gg_list_heat[1:4],
    list(ggplotGrob(leg_kill(dpi_adj * 2, dpi_adj * 1.8, scale_pine_heat)),
         ggplotGrob(leg_surv(dpi_adj * 2, dpi_adj * 1.8)))
  )
  
  
  gt_red_blue = arrangeGrob(grobs = gp_red_blue, layout_matrix = layout_mx)
  gt_heat = arrangeGrob(grobs = gp_heat, layout_matrix = layout_mx)
  
  title_height = 1
  map_height = 5
  legend_height = 1.2
  
  gt_red_blue$heights = gt_heat$heights = 
    c(
      unit(title_height, "npc"), 
      unit(map_height, "npc"), 
      unit(map_height, "npc"),
      unit(legend_height, "npc")
    )
  
  png_height = 2000
  png_aspect = 2
  
  {
    png("PNAS_Figures/figure_3_red_blue_gray.png", width = png_aspect * png_height, height = png_height)
    grid.newpage()
    # plot(gt_red_blue)
    grid.draw(gt_red_blue)
    dev.off()
  }
  {
    png("PNAS_Figures/figure_3_heat_gray.png", width = png_aspect * png_height, height = png_height)
    grid.newpage()
    # plot(gt_heat)
    grid.draw(gt_heat)
    dev.off()
  }  
  
  
  
}

#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#     
#   # gt = arrangeGrob(grobs = grob_list, layout_matrix = lmx)    
#   # gt$heights = c(
#   #   unit(1, "npc"),
#   #   unit(1, "npc"),
#   #   unit(2, "npc")
#   #   )
#   # grid.newpage()
#   # grid.draw(gt)
# 
#   
#   
#   
#     maps_red_blue = 
#     rbind(
#       arrangeGrob(panel_titles_row(title_size = title_size * 0.001 * png_height)), 
#       arrangeGrob(surv_row), 
#       arrangeGrob(pine_log_surv_row_red_blue), 
#       arrangeGrob(legends_red_blue),
#       size = "first")
#   
#   
#   maps_red_blue = rbind(
#     panel_titles_row(title_size = title_size * 0.001 * png_height), 
#     surv_row, 
#     pine_log_surv_row_red_blue, 
#     size = "first"
#   )
#   
#   maps_red_blue$heights = c(
#     unit(title_height, "npc"), 
#     unit(map_height, "npc"), 
#     unit(map_height, "npc")
#   )
#   
#   
#   
#   # maps_red_blue$heights = c(
#   #   unit(title_height, "npc"), 
#   #   unit(map_height, "npc"), 
#   #   unit(map_height, "npc"),
#   #   unit(legend_height, "npc"))
#   # 
#   png("PNAS_Figures/figure_3_test.png", width = png_aspect * png_height, height = png_height)
#   # grid.draw(fig_heat)
#   grid.draw(arrangeGrob(maps_red_blue))
#   dev.off()
#   
#   
#   maps_heat = rbind(surv_row, pine_log_surv_row_heat, size = "first")
#   
#   legends_heat = cbind(
#     ggplotGrob(leg_kill(dpi_adj * 2, dpi_adj * 1.8, scale_pine_heat)),
#     ggplotGrob(leg_surv(dpi_adj * 2, dpi_adj * 1.8)),
#     size = "first")
#   
#   legends_red_blue = cbind(
#     ggplotGrob(leg_kill(dpi_adj * 2, dpi_adj * 1.8, scale_pine_red_blue)),
#     ggplotGrob(leg_surv(dpi_adj * 2, dpi_adj * 1.8)),
#     size = "first")
#   
#   fig_heat = rbind(arrangeGrob(maps_heat), arrangeGrob(legends_heat))
#   fig_red_blue = rbind(arrangeGrob(maps_red_blue), arrangeGrob(legends_red_blue))
#   
#   fig_heat$heights = c(unit(height_1, "npc"), unit(height_2, "npc"))
#   fig_red_blue$heights = c(unit(height_1, "npc"), unit(height_2, "npc"))
#   
#   
#   png("PNAS_Figures/figure_3_heat_gray.png", width = png_aspect * png_height, height = png_height)
#   grid.draw(fig_heat)
#   # grid.draw(maps_red_blue)
#   dev.off()
#   
#   png("PNAS_Figures/figure_3_red_blue_gray.png", width = png_aspect * png_height, height = png_height)
#   grid.draw(fig_red_blue)
#   dev.off()
#   
# }
# 
