source('Plotting function.R')
nonnegative5 = read.csv("nonzero_ds_wv5.csv")
nonnegative6 = read.csv("nonzero_ds_wv6.csv")


# we have 25 countries in common between wave 5 and 6
country_list =  intersect(unique(nonnegative5$country), unique(nonnegative6$country))

country_list = c("United States")


  
for (country in country_list) {
    require(patchwork)
    p1 = profile_plot(gen_country(nonnegative5, country), 5)
    p2 = profile_plot(gen_country(nonnegative6, country), 6)
    p1+p2
    ggsave(paste(country, "profile_plot.png"),device = "png", width = 16)
  }
  






