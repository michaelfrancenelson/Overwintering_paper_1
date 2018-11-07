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
  require(raster)
}

# Load data (uses only local data files) ----
{
  source("Globals.R")
  
  national_forests_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "national_forests_in_mpb_states"), layer = "national_forests_in_mpb_states"), CRS(proj4_master))
  pine_range_polygons   = spTransform(readOGR(dsn = paste0(local_dat_dir, "pine_range_polygons_mpb_states"), layer = "pine_range_polygons_mpb_states"), CRS(proj4_master))
  mpb_states_poly       = spTransform(readOGR(dsn = paste0(local_dat_dir, "mpb_states"), layer = "mpb_states"), CRS(proj4_master))
  
  # from arcgis.com:  
  states_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "states"), layer = "states"), CRS(proj4_master))
  canada_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "Canada"), layer = "Canada"), CRS(proj4_master))
  mexico_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "mexstates"), layer = "mexstates"), CRS(proj4_master))
  states_poly@data = data.frame(name = states_poly$STATE_NAME)
  canada_poly@data = data.frame(name = canada_poly$NAME)
  mexico_poly@data = data.frame(name = mexico_poly$ADMIN_NAME)
  n_am_poly = rbind(states_poly, canada_poly, mexico_poly)
  
  lat_lon = spTransform(readOGR(dsn = paste0(local_dat_dir, "lat_long_us"), layer = "lat_long_us"), CRS(proj4_master))
  lon_lat_lines = subset(lat_lon, DEGREE5 == "Y")
}

# plot params ----
{
  # Save filename info:
  fig_num = "01"
  fig_name = "Study_Area_Map"
  fig_dir = "PNAS_figures"
  fig_filename = paste0(fig_dir, "/Figure_", fig_num, "_", fig_name)
  
  # color for the lon/lat lines
  lon_lat_col = gray(0.8, 0.4)
  
  # color for the state boundaries
  state_boundary_col = gray(0.3, alpha = 1)
  
  # map theme with border and no panel background  
  theme_map = 
    theme(
      axis.text = element_blank(), axis.ticks = element_blank(), 
      panel.grid = element_blank(), panel.background = element_blank(),
      panel.border = element_rect(fill = NA),
      axis.title = element_blank())
  
  # legend theme with border and white panel background
  theme_legend = 
    theme(
      axis.text = element_blank(), axis.ticks = element_blank(), 
      panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
      panel.border = element_rect(fill = NA),
      axis.title = element_blank())
    
    # line width for pine and nf polys
    range_line_size = 0.1
  
  # pine color is a translucent green  
  fill_pine = rgb(0.0, .7, 0.0, 0.5)
  
  # pine line color is null
  line_pine = rgb(0, 0, 0, 0)
  
  # national forest color is a translucent blue
  fill_nf = rgb(0.0, 0, 1, 0.3)
  
  # national forest border color is aless translucent blue
  line_nf = rgb(0, 0, 1, 0.15)
  
  # legend window plot limits:
  leg_ylims = ylim(-0.6, 0.6)
  leg_xlims = xlim(0.0, 4.1)
  
  # legend label vertical offset
  leg_y_horiz = -0.31
  
  # legend label size
  leg_label_size = 11
  
}



# Legend polygons  ----
{
  # a square for plotting the legend colors
  # square = matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), ncol = 2)
  square = matrix(c(0, 0, 1, 1, 0, 0, 0.6, 0.6, 0, 0), ncol = 2)
  (add_x = matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), ncol = 2))
  (add_y = matrix(c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1), ncol = 2))
  
  squares_horiz_left  = Polygons(list(
    Polygon(square), 
    Polygon(square + 1.5 * add_x)), ID = 1)
  squares_horiz_right = Polygons(list(
    Polygon(square + 1.5 * add_x),
    Polygon(square + 3.0 * add_x)), ID = 1)
  squares_horiz_right2 = Polygons(list(Polygon(square + 1.5 * add_x), Polygon(square + 2.5 * add_x)), ID = 2)
  
  legend_horiz_left = SpatialPolygonsDataFrame(
    SpatialPolygons(list(squares_horiz_left)),
    data = data.frame(name = "nf"))
  
  legend_horiz_right = SpatialPolygonsDataFrame(
    SpatialPolygons(list(squares_horiz_right)),
    data = data.frame(name = "pine"))
  
  gp_legend_left = geom_polypath(
    data = legend_horiz_left, 
    aes(long, lat, group = group), 
    fill = fill_nf, color = line_nf)
  
  gp_legend_right = geom_polypath(
    data = legend_horiz_right, 
    aes(long, lat, group = group), 
    fill = fill_pine)
  # ggplot() + gp_legend_right + gp_legend_left + coord_equal() 
}


# ggplot polygons ----
{
  # national forest polys
  gg_nf = geom_polypath(data = national_forests_poly, 
                        aes(long, lat, group = group), 
                        color = line_nf, fill = fill_nf, size = range_line_size)
  # pine native range polys:
  gg_pine_range = geom_polypath(data = pine_range_polygons, 
                                aes(long, lat, group = group), 
                                fill = fill_pine, color = line_pine)
  
  # lon/lat lines:
  gg_lat_lon = geom_polypath(data = lon_lat_lines, aes(long, lat, group = group), 
                             color = lon_lat_col, fill = rgb(0,0,0,0))
  
  #north america state/province boundaries polygons:
  gg_na = geom_polypath(data = n_am_poly, aes(long, lat, group = group), 
                        color = state_boundary_col, fill = rgb(0, 0, 0, 0))
  
  # Build a legend ggplot object
  ggp_leg_horiz = function(leg_label_size, ylims, xlims, leg_y_horiz)
  {
    ggplot() + coord_equal(ratio = 1) +
      theme(plot.margin = NULL) + 
      theme_legend +
      ylims + xlims +
      gp_legend_right + gp_legend_left +
      annotate("text", x = 0.5, y = leg_y_horiz, label = "National\nForests", size = leg_label_size) +
      annotate("text", x = 2, y = leg_y_horiz, label = "Overlap\n ", size = leg_label_size) +
      annotate("text", x = 3.5, y = leg_y_horiz, label = "Pine\nNative Range", size = leg_label_size)
  }
}


# Create figure ----
{
  
  ggp_map = ggplot() + coord_equal(xlim = c(-2e6, 0.2e6), ylim = c(-1.2e6, 1e6)) + theme_map +
    gg_lat_lon + gg_na + 
    gg_pine_range + gg_nf
  
  # Where to overplot the legend:
  legend_vp = viewport(width = unit(0.32, "npc"), height = unit(0.5, "npc"), x = 0.71, y = 0.91)
  {
    png(width = 2000, height = 2000, filename = paste0(fig_filename, "_blue_2k.png"))
    print(ggp_map)
    print(ggp_leg_horiz(
      leg_label_size = leg_label_size, leg_y_horiz = leg_y_horiz, 
      ylims = leg_ylims, xlims = leg_xlims), vp = legend_vp)
    dev.off()  
  }
}

