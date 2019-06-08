ggplot(c1, aes(x = tax, y = state_aid))+
  geom_jitter(alpha = 0.4)

cor(as.numeric(c1$tax), as.numeric(c1$state_aid))

head(data_class)

c1 = data_class %>%
  filter(pred_class == 1)
glimpse(c1)
