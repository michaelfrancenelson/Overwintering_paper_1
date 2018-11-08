# color for the lon/lat lines
lon_lat_col = gray(0.8, 0.4)

# color for the state boundaries
state_boundary_col = gray(0.3, alpha = 1)

# sets resolution for plotting the survival, use low value for faster speed
# maxpixels = 1e2
maxpixels = 1e7

# Max S.D to show in the min winter temp sd map.  Values above this show the max color.
max_w_lo_sd = 6

# Max minimum temperature to show in the map.  Vals above this are the max color.
max_w_lo_mean = 4

# Max sum of pines killed
max_pine_sum = 5000

# Theme for the map panels
t_map = theme(
  axis.text = element_blank(), axis.ticks = element_blank(), 
  panel.grid = element_blank(), panel.background = element_blank(),
  panel.border = element_rect(fill = NA),
  # panel.border = element_blank(),
  axis.title = element_blank())

# Thickness of the legend bands
legend_band_width = 29
legend_aspect_ratio = 0.1

# draw a map from a gplot object
plot_map = function(gg) {
  gg + gg_lat_lon + g_rast + states_poly_gray_1 + t_map + coords_map_panel
}

# x- and y- limits for the map plot panels
coords_map_panel = coord_equal(xlim = c(-2e6, 1e5), ylim = c(-1.1e6, 0.85e6), ratio = 1)

# create a raster for plotting with no legend  
g_rast = geom_raster(aes(fill = value), show.legend = F)


