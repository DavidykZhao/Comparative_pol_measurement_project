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
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(22)

rain_country_list = c("Brazil", "Japan", "Serbia", "Sweden", "United Kingdom",
            "United States")


rain_country_data = filter(nonnegtive, country %in% rain_country_list)
rain_country_data[,2:7] = apply(rain_country_data[,2:7], 2, as.numeric)

### IMPORTANT change datatype of answers to numeric...
nonnegtive[,2:7] = apply(nonnegtive[,2:7], 2, as.numeric)

sumld <- nonnegtive %>% 
  dplyr::select(country, tax) %>% 
  group_by(country) %>% 
  summarise_all(funs(mean, median, lower = lb, upper = ub))
sumld


g <- 
  ggplot(data = nonnegtive, 
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
  scale_color_manual(values = getPalette) +
  scale_fill_manual(values = getPalette) +
  #coord_flip() + # flip or not
  theme_bw() +
  raincloud_theme +
  theme(axis.title = element_text(size = 42),
        axis.text=element_text(size=42))
g

png(filename="tax.png", width = 7000, height = 2000)
g
dev.off()