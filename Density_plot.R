library(tidyverse)
library(ggthemes)
#theme_set(theme_classic())
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(26)







# Plot
g <- ggplot(data_wv5_6up, aes(V152))+
  geom_density(aes(fill=factor(country)), alpha=0.3) + 
  scale_fill_manual(values = getPalette) +
  labs(title="Density plot of V151", 
       subtitle="Government taxes rich to subsidize poor ",
       caption="Source: World Value Survey Wave 5",
       x="Agreement level",
       fill="Country")
g
       # annotate("text", x = -4, y = 0.4, 
       #           label = "Colombia: 'notasked'")
       # #theme(legend.key.size = unit(0.2, "cm"))


png(filename="Vl52_test_nonnormality.png", width = 2000, height = 1600)
g
dev.off()
###check for the percentage of nonnormal values
tst = data_wv5_6up %>%
  group_by(country) %>%
  select(V152) %>%
  table()
tst
sort(rowSums(round(prop.table(tst, 1),3)[,1:3]), TRUE)


       
