source('Plotting function.R')
nonnegative = read.csv("nonzero_ds.csv")



country_list = unique(nonnegative$country)
#country_list = c("Argentina", "Canada")

for (country in country_list) {
  profile_plot(gen_country(nonnegative, country))
  ggsave(paste(country, "profile_plot.png"),device = "png", width = 9)
}
