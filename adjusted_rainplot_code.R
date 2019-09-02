
sumld <- rain_country_data %>% 
  dplyr::select(country, civil_rights) %>% 
  group_by(country) %>% 
  summarise_all(funs(mean, median, lower = lb, upper = ub))
sumld

g <- 
  ggplot(data = rain_country_data, 
         aes(x = factor(country), y = civil_rights, fill = factor(country))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), trim = TRUE, alpha = .8, scale = "width") +
  geom_point(aes(y = civil_rights, color = factor(country)), 
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
        axis.text=element_text(size=42),
        plot.margin = margin(t = 2, r = 3, b = 2, l = 1.5,
                             unit = "cm"))


png(filename="civil_rights_raincountry.png", width = 2000, height = 1200)
g
dev.off()