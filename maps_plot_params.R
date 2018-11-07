# color for the lon/lat lines
lon_lat_col = gray(0.8, 0.4)

# color for the state boundaries
state_boundary_col = gray(0.3, alpha = 1)

# sets resolution for plotting the survival, use low value for faster speed
# maxpixels = 1e2
maxpixels = 1e7

# Theme for the map panels
t_map = theme(axis.text = element_blank(), axis.ticks = element_blank(), 
           panel.grid = element_blank(), panel.background = element_blank(),
           panel.border = element_rect(fill = NA),
           # panel.border = element_blank(),
           axis.title = element_blank())

# Thickness of the legend bands
legend_band_width = 22
legend_aspect_ratio = 0.5

# 
# # Theme for the legends
# t_leg = theme(
#   panel.background = element_blank(), 
#   axis.line.y = element_blank(), 
#   axis.ticks.y = element_blank(),
#   axis.text.y = element_blank(), 
#   axis.title.y = element_blank(),
#   axis.ticks.x = element_blank()
# )

# x- and y- limits for the map plot panels
coords_map_panel = coord_equal(xlim = c(-2e6, 1e5), ylim = c(-1.1e6, 0.85e6), ratio = 1)

# create a raster for plotting with no legend  
g_rast = geom_raster(aes(fill = value), show.legend = F)

# Create a custom legend theme given the input font sizes
t_leg_1 = function(label_size, tick_size)
{
  return (
    theme( 
      panel.background = element_blank(), 
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.title.y = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      axis.title.x = element_text(size = label_size),
      # axis.text.x = element_text(size = tick_size),
      axis.text.x = element_blank(),
      axis.text.x.top = element_text(size = tick_size))
    
    
    
  )
}


p <- ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# Create a simple secondary axis
p + scale_y_continuous(sec.axis = sec_axis(~.)) + theme(axis.text.y = element_blank(), axis.text.y.right = element_text(size = 20))

# Inherit the name from the primary axis
p + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(~.+10, name = derive()))

# Duplicate the primary axis
p + scale_y_continuous(sec.axis = dup_axis())

# You can pass in a formula as a shorthand
p + scale_y_continuous(sec.axis = ~.^2)

