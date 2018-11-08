
# some convenience functions ------------

{
  
  # model_l = arima_models[type == "arima" & lookback == 9]$model
  # arima_models[type == "arima" & lookback == 1]
  # arima_models[type == "arima" & lookback == 1 & transform == "log"]
  # model_l = model_stats[1, ]$model  
  # model_l = models[64, ]$model[[1]]
  # model_l = models[54, ]$model[[1]]
  
  # retrieve the p-value from a list containing one arima model
  get_p = function(model_l)
  {
    # fit = models[transform == "log" & type == "arima"][lb]$model[[1]]
    fit = model_l
    tab = coeftest(fit)
    p_col = which(grepl(pattern = "Pr\\(", colnames(tab)))
    p_row = which(grepl(pattern = "xreg", rownames(tab)))
    p_row = which(grepl("xreg", rownames(tab)))
    if(length(p_row) == 0)
      p_row = which(grepl("survival", rownames(tab)))
    return(tab[p_row, p_col])
  }
  
  # retrieve the regression coefficient from a list containing one arima model
  get_c = function(model_l)
  {
    fit = model_l
    coef_1 = coef(fit)
    if ("xreg" %in% names(coef_1)) 
      return(as.numeric(coef_1[which(grepl("xreg", names(coef_1)))]))
    return(as.numeric(coef_1[which(grepl("survival", names(coef_1)))]))
  }
  get_int = function(model_l)
  {
    fit = model_l
    coef_1 = coef(fit)
    if ("intercept" %in% names(coef_1)) 
      return (as.numeric(coef_1[which(grepl("intercept", names(coef_1)))]))
    return(NA)
  }
  
  # retrieve the sum of squared residuals from a list containing one arima model
  get_ssr = function(model_l)
  {
    fit = model_l[[1]]
    return(sum(residuals(fit) ^ 2))
  }
  
  # create a legend with the text scaled appropriately
  # scaled_legend = function(leg_gg, label_size = 40, tick_size = 20)
  # {
  #   
  #   
  #   # t_leg = scaled_theme_legend(label_size, tick_size)
  #   # leg_gg + t_leg
  # }
  
  # Create a legend given input range, and labels
  build_legend = function(
    leg_title, color_scale, 
    leg_breaks, leg_labels, legend_band_width,
    legend_aspect_ratio,
    label_size = 11, tick_size = 1, title_size = 1,
    ylim = c(0, 1.3), label_y = 0.3,
    log = F, n_pts = 1000 )
  {
    min_val = head(leg_breaks, 1)
    max_val = tail(leg_breaks, 1)
    mid_val = sum(min_val, max_val) / 2
    print(paste0("min: ", min_val, " max: ", max_val, "mid: ", mid_val))
    
    x = seq(min_val, max_val, len = n_pts)
    sc_y = scale_y_continuous(expand = c(0, 0), limits = ylim)
    line_leg = geom_line(size = legend_band_width, show.legend = F)
    
    if (log) 
    {
      coord_leg = coord_equal(ratio = legend_aspect_ratio * (log(max_val) - log(min_val))) 
      sc_x = scale_x_continuous(
        breaks = leg_breaks, labels = leg_labels, trans = "log", #name = leg_title,
        sec.axis = sec_axis(~., breaks = leg_breaks, labels = leg_labels))
      
      mid_val = exp(0.5 * sum(log(max_val), log(min_val)))
      
    } else
    {
      coord_leg = coord_equal(ratio = legend_aspect_ratio * (max_val - min_val))
      sc_x = scale_x_continuous(
        breaks = leg_breaks, labels = leg_labels, #name = leg_title,
        sec.axis = sec_axis(~., breaks = leg_breaks, labels = leg_labels))
    }
    
    th_leg = scaled_theme_legend(label_size, tick_size, title_size)
    
    ggplot(data.frame(x, y = 1), aes(x = x, y = y, color = x)) +
      line_leg +
      sc_y + sc_x +
      coord_leg +
      color_scale +
      # xlab(leg_title) +
      annotate("text", x = mid_val, y = label_y, label = leg_title, size = th_leg$text$size) +
      th_leg
    # theme(
    #   axis.line.y = element_blank(),
    #   axis.text.y = element_blank(),
    #   axis.title.y = element_blank(),
    #   axis.ticks.y = element_blank(),
    #   axis.line.x.bottom = element_blank(),
    #   axis.text.x.bottom = element_blank(),
    #   axis.ticks.x.bottom = element_blank(),
    #   panel.background = element_blank())
  }
  
  # Create a custom legend theme given the input font sizes
  # scaled_theme_legend = function(label_size, tick_size, tick_length_scale = 0.03, tick_size_scale = 0.08)
  scaled_theme_legend = function(label_size = 1, tick_size = 1, title_size = 1, tick_length = 0.3, tick_width = 0.04, margin_adj = 0.1)
  {
    return (
      theme( 
        text = element_text(size = title_size),
        
        panel.background = element_blank(), 
        plot.margin = margin(
          r = margin_adj, 
          l = -tick_length + margin_adj, 
          b = -tick_length + margin_adj, 
          t = 0,
          unit = "npc"),
        
        axis.ticks.length = unit(tick_length, "npc"),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(), 
        
        axis.title.x = element_blank(),
        
        # axis.text.x.top = element_text(size = tick_size),
        axis.text.x.top = element_text(size = label_size),
        axis.ticks.x.top = element_line(size = tick_width * tick_size, color = "black"),
        
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.line.x.bottom = element_blank()
        
        # axis.text.x = element_blank(),
        # axis.title.x = element_text(size = label_size, margin = margin(t = - tick_length, unit = "npc")),
        # axis.title.x = element_text(size = label_size, vjust = 1, margin = margin(t = -0.2, unit = "npc")),
        # axis.text.x = element_text(size = tick_size),
        # legend.justification  = c(0, 0.5)
        # plot.margin = margin(r = 0, l = -tick_length, unit = "npc")
      )
    )
  }
  
  # make a legend using some defaults
  legend_default = function(leg_title, color_scale, leg_breaks, leg_labels, log = F)
  {
    build_legend(
      leg_title = leg_title, color_scale = color_scale, 
      leg_breaks = leg_breaks, leg_labels = leg_labels, 
      label_size = label_adj, tick_size = tick_adj, title_size = title_adj,
      legend_band_width = legend_band_width,
      legend_aspect_ratio = legend_aspect_ratio, log = log)
  }
  
  
  }



