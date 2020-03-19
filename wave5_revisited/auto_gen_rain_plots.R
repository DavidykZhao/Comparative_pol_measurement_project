# This is the script to generate the rain plot for wave5 revisited

# load in the data
nonnegtive = read.csv("nonnegative_wv5_allvars.csv")


lb <- function(x) mean(x) - sd(x)
ub <- function(x) {
  if (mean(x) + sd(x) > 10){
    return(10)
  }
  return(mean(x) + sd(x))
}

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 10),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
  axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))

#mypalette = RColorBrewer::brewer.pal(26,"Spectral") not working
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(10)

rain_country_list = c("Brazil", "Japan", "Serbia", "Sweden", "United Kingdom",
            "United States")


rain_country_data = filter(nonnegtive, country %in% rain_country_list)
rain_country_data[, -1] = apply(rain_country_data[, -1], 2, as.numeric)


### IMPORTANT change datatype of answers to numeric...

sumld <- rain_country_data %>% 
  dplyr::select(country, tax) %>% 
  group_by(country) %>% 
  summarise_all(funs(mean, median, lower = lb, upper = ub))
sumld


 
  ggplot(data = rain_country_data, 
         aes(x = factor(country), y = tax, fill = factor(country))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), trim = TRUE, alpha = .8, scale = "width") +
  geom_point(aes(y = tax, color = factor(country)), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  geom_point(data = sumld, aes(x = factor(country), y = mean), 
             position = position_nudge(x = 0.3), size = 2.5) +
  geom_errorbar(data = sumld, aes(ymin = lower, ymax = upper, y = mean), 
                position = position_nudge(x = 0.3), width = 0)+
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_grey() +
  scale_fill_grey() +
  #coord_flip() + # flip or not
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 13),
    plot.margin=unit(c(1,1.5,1.5,1.2),"cm"),
    axis.text.y.left = element_text(margin = margin(t = 0, r = -30, b = 0, l = 0)),
    axis.ticks = element_blank()
  )+
    labs(y = '" Taxing rich to subsidize poor "' )

ggsave("Tax.png", width = 8)



# loop over variable names; need to use special treatment when passing args to dplyr

gen_rain_plot = function(var) {
  sumld <- rain_country_data %>% 
    dplyr::select(country, var) %>% 
    group_by(country) %>% 
    summarise_all(funs(mean, median, lower = lb, upper = ub))
  var <- sym(var)

  ggplot(data = rain_country_data, 
         aes(x = factor(country), y = !! var, fill = factor(country))) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), trim = TRUE, alpha = .8, scale = "width") +
    geom_point(aes(y = !! var, color = factor(country)), 
               position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
    geom_point(data = sumld, aes(x = factor(country), y = mean), 
               position = position_nudge(x = 0.3), size = 2.5) +
    geom_errorbar(data = sumld, aes(ymin = lower, ymax = upper, y = mean), 
                  position = position_nudge(x = 0.3), width = 0)+
    expand_limits(x = 5.25) +
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_color_manual(values = getPalette) +
    scale_fill_manual(values = getPalette) +
    #coord_flip() + # flip or not
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 13),
      plot.margin=unit(c(1,1.5,1.5,1.2),"cm"),
      axis.text.y.left = element_text(margin = margin(t = 0, r = -30, b = 0, l = 0)),
      axis.ticks = element_blank())
  
  ggsave(paste(var, ".png"), width = 8)
  
}


for(i in colnames(nonnegative)[-1]) gen_rain_plot(i)

