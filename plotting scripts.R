jpeg(filename="democratic-ness.jpeg", width = 1024, height = 1024)
ggplot(additional_wv5, aes(x=factor(country), y=V163)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()+
  coord_flip()+
  labs(title="Boxplot of 'democratic-ness'",y="Answer_value", x = "Country")+
  theme_fivethirtyeight()+
  theme(title = element_text(size = 26),
        axis.title = element_text(size = 22),
        text = element_text(size=20),
        plot.margin = unit(c(3, 3,3,3), "cm"))
dev.off()

table(useful_wv5$V152)
#### percentage within each country below 0 answers
useful_wv5 %>%
  filter(country == "Canada") %>%
  select(V152) %>%
  table()

additional_wv5 = dat %>%
  filter(country %in% country_censored) %>%
  select(country, V148, V150, V151, V162, V163)


