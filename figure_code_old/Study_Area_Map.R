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

# Load data ----------------
{
  source("Globals.R")
  
  national_forests_poly = spTransform(readOGR(dsn = paste0(spatial_data_output_dir, "national_forests_in_mpb_states"), layer = "national_forests_in_mpb_states"), CRS(proj4_master))
  pine_range_polygons   = spTransform(readOGR(dsn = paste0(spatial_data_output_dir, "pine_range_polygons_mpb_states"), layer = "pine_range_polygons_mpb_states"), CRS(proj4_master))
  mpb_states_poly = spTransform(readOGR(dsn = paste0(spatial_data_output_dir, "mpb_states"), layer = "mpb_states"), CRS(proj4_master))
  
  # from arcgis.com:  
  states_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "states"), layer = "states"), CRS(proj4_master))
  canada_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "Canada"), layer = "Canada"), CRS(proj4_master))
  mexico_poly = spTransform(readOGR(dsn = paste0(local_dat_dir, "mexstates"), layer = "mexstates"), CRS(proj4_master))
  states_poly@data = data.frame(name = states_poly$STATE_NAME)
  canada_poly@data = data.frame(name = canada_poly$NAME)
  mexico_poly@data = data.frame(name = mexico_poly$ADMIN_NAME)
  n_am_poly = rbind(states_poly, canada_poly, mexico_poly)
  
  lat_lon = spTransform(readOGR(dsn = paste0(spatial_data_dir, "lat_long_us"), layer = "lat_long_us"), CRS(proj4_master))
  lon_lat_lines = subset(lat_lon, DEGREE5 == "Y")
  # This is a huge file and takes forever to load: 
  plot(n_am_poly)
  plot(lon_lat_lines, add = T)
}

# plot params ----
{
  state_border_color = gray(0.1, 0.5)
  t1 = theme(axis.text = element_blank(), axis.ticks = element_blank(), 
             panel.grid = element_blank(), panel.background = element_blank(),
             panel.border = element_rect(fill = NA),
             axis.title = element_blank())
  
  t2 = theme(axis.text = element_blank(), axis.ticks = element_blank(), 
             panel.grid = element_blank(), panel.background = element_blank(),
             panel.border = element_blank(),
             axis.title = element_blank())
  
  lon_lat_col = gray(0.8, 0.4)
  
  fill_pine = rgb(0.0, .7, 0.0, 0.5)
  fill_nf = rgb(0.0, 0, 1, 0.3)
  # fill_nf = rgb(0.0, 0, 1, 0.5)
  line_nf = rgb(0, 0, 1, 0.15)
}



# Legend polygons  ----
{
  square = matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), ncol = 2)
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
  
  # legend_horiz = SpatialPolygonsDataFrame(
  #   SpatialPolygons(list(squares_horiz_left, squares_horiz_right)),
  #   data = data.frame(name = c("nf", "pine")))
  # 
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
  ggplot() + gp_legend_right + gp_legend_left + coord_equal()
}


# ggplot polygons ----
{
  # gg_states = geom_polypath(data = states_poly, aes(long, lat, group = group), color = state_border_color, fill = rgb(0, 0, 0, 0))
  
  gg_nf = geom_polypath(data = national_forests_poly, 
                        aes(long, lat, group = group), 
                        color = line_nf, fill = fill_nf, size = 0.1)
  gg_pine_range = geom_polypath(data = pine_range_polygons, 
                                aes(long, lat, group = group), 
                                fill = fill_pine, color = rgb(0, 0, 0, 0))
  
  gg_lat_lon = geom_polypath(data = lon_lat_lines, aes(long, lat, group = group), color = lon_lat_col, fill = rgb(0,0,0,0))
  gg_na = geom_polypath(data = n_am_poly, aes(long, lat, group = group), color = state_border_color, fill = rgb(0, 0, 0, 0))
  
  leg_y_horiz = -0.28
  leg_ann_size = 6
  ggp_leg_horiz = function(leg_ann_size = 6, ylims = ylim(-0.6, 0.6), xlims = xlim(0.0, 4.1), leg_y_horiz = -0.29)
  {
    ggplot() + coord_equal(ratio = 1) +
      theme(plot.margin = NULL) + 
      t2 +
      ylims +
      xlims +
      gp_legend_right + gp_legend_left +
      annotate("text", x = 0.5, y = leg_y_horiz, label = "National\nForests", size = leg_ann_size) +
      annotate("text", x = 2, y = leg_y_horiz, label = "Overlap\n ", size = leg_ann_size) +
      annotate("text", x = 3.5, y = leg_y_horiz, label = "Pine\nNative Range", size = leg_ann_size)
  }
  
  ggp_leg_horiz()
  # ggplot() + gg_na + coord_equal(ratio = 1) + t1
  # vp = viewport(width = unit(0.32, "npc"), height = unit(0.5, "npc"), x = 0.71, y = 0.91)
  # print(ggp_leg_horiz, vp = vp)
  # ggp_map
  
}
X11()


ggp_map = ggplot() + coord_equal(xlim = c(-2e6, 0.2e6), ylim = c(-1.2e6, 1e6)) + t1 +
  gg_lat_lon + gg_na + 
  gg_pine_range + gg_nf


vp = viewport(width = unit(0.32, "npc"), height = unit(0.5, "npc"), x = 0.71, y = 0.91)
png(width = 1600, height = 1600, filename = "figures/study_site_map_green_blue.png")
png(width = 2000, height = 2000, filename = "figures/study_site_map_green_blue_2k.png")
ggp_map
print(ggp_leg_horiz(leg_ann_size = 11, leg_y_horiz = -0.31), vp = vp)
dev.off()  



print(ggp_leg_horiz, vp = vp)


ggp_leg_horiz


vp = viewport(width = unit(0.35, "npc"), height = unit(0.25, "npc"), x = 0.72, y = 0.91)
print(ggp_leg_horiz, vp = vp)

ggplot() + 
  
  
  print(gg, vp = vp)
ggp_map



plot(0)
pts = locator(1)  
pts1 = data.frame(x = pts$x, y = pts$y, name = as.character("pine"))
for (i in 1:30)
{
  pts = locator(1)  
  graphics::text(pts$x, pts$y, labels = i, cex = 0.75)
  pts1 = rbind(pts1, c(x = pts$x, y = pts$y, name = "nf"))
}

pts1$name = as.character(pts1$name)
pts1$name[which(is.na(pts1$name))] = "nf"

gg_nf_l = geom_polygon(data = subset(pts1, name == "nf"), aes(x = x, y = y), color = line_nf, fill = fill_nf)
gg_pn_l = geom_polygon(data = subset(pts1, name == "pine"), aes(x = x, y = y), color = rgb(0, 0, 0, 0), fill = fill_pine)

# plot with legend -----
gg_main = ggplotGrob(ggplot() + coord_equal(xlim = c(-2e6, 0.2e6), ylim = c(-1.2e6, 1e6)) + t1 +
                       gg_lat_lon + gg_na + 
                       gg_pine_range + gg_nf)

gg_legend = ggplotGrob(
  ggplot() + gg_nf_l + gg_pn_l + theme_minimal() + 
    theme(
      panel.background = element_blank(), 
      panel.grid = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(), axis.text = element_blank()) +
    annotate("text", x = 0.85, y = -1, label = "Pine Range") +
    annotate("text", x = 1.15, y = -1, label = "National Forests") +
    ylim(-1.4, NA) +
    coord_fixed(ratio = 0.05)
)

png(height = 1800, width = 1600, filename = "figures/study_site_green_blue_legend.png")
grid.arrange(gg_main, gg_legend, heights = c(1, 0.15), ncol = 1)
dev.off()

gg_both = rbind(gg_main, gg_legend, size = "first")
gg_both$heights

set_panel_heights <- function(g, heights){
  g$heights <- grid:::unit.list(g$heights) # hack until R 3.3 comes out
  id_panels <- unique(g$layout[g$layout$name=="panel", "t"])
  g$heights[id_panels] <- heights
  g
}

g = set_panel_heights(gg_both, lapply(c(1, 0.1), grid::unit, "null"))

grid.draw(gg_both)

pts1$x = as.numeric(pts1$x)
pts1$y = as.numeric(pts1$y)

round(pts1[, 1], digits = 3)

plot(pts1$y ~ pts1$x, type = "l")

pts = locator(20)
plot(pts$x, pts$y, type = "l")

ggplot() + gp_legend_left + gp_legend_right + coord_equal() + t1

legend_horiz@data

ggplot() + geom_polypath(data = legend_horiz, aes(x = long, y = lat, group = group, fill = group))


