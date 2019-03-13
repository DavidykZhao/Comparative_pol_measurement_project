library(tidyverse)
install.packages("labelled")
library("labelled")

dat <- readRDS("/Users/zhaoyikai/Desktop/Demo_MI_project/wv5.rds")
unique(dat["V2"])

polity = readxl::read_xls("polity_2017.xls")
summary(polity)
polity_7up = polity %>%
  filter(year == 2017 & democ >=7) %>%
  select(country, year, democ)
country_name = polity_7up$country # select the atomic column using $ rather than []
intersect(country_name,country_vector)
#### read in country_code
country_code = readxl::read_xlsx("country_code_5.xlsx", col_names = F)
colnames(country_code) = c("code", "country")
country_vector <- setNames(country_code$country, country_code$code)
country_w_wv5 = intersect(country_name,country_vector)
country_vector
dat$country = country_vector[as.character(dat$V2A)]
table(dat$country)

table(dat$V2A)

summary(dat)
demo_v5 = c("country", "V152", "V153", "V154", "V155", "V156", "V157", "V161")

data_wv5 = dat %>%
  select(demo_v5)
data_wv5 = as_data_frame(data_wv5)
colnames(data_wv5)
#lapply(data_wv5, function(x) any(is.na(x))) No NA values
summary(data_wv5) #### make sure the dtypes are correct

tst = data_wv5$V152
head(data_wv5["V152"])
labels(tst)
stack(attr(data_wv5$V152, 'labels'))

# ggplot(data_wv5, aes(V152))+
#   geom_bar()


apply(data_wv5[, -1], 2, show_bar)
################ Reading in wv6 data ######################
# Command+shift+c
# dat6 <- readRDS("wv6.rds")
# table(dat6$V2A)
# country_name
# country_code_6 = readxl::read_xlsx("country_code_6.xlsx", col_names = TRUE)
# colnames(country_code_6) = c("code", "country")
# country_vector_6 <- setNames(country_code_6$country, country_code_6$code)
# country_w_wv6 = intersect(country_name,country_vector_6)
# dat6$country = country_vector_6[as.character(dat6$V2A)]
# table(dat6$country)
# country_two_waves = intersect(country_w_wv6, country_w_wv5)
# demo_v6 = c("country", "V131", "V132", "V133", "V134", "V135",
#             "V136", "V139")
# data_wv6 = dat6 %>%
#   select(demo_v6)
########################how many unnormal entries are there
# for (i in c(2:8)) {
#   print(paste("No.", i, "var has the unnormal persentage of", 
#               mean((data_wv6[,i]) < 0)))
# }

################### inpute the abnormal value into 5
data_wv6_inputed = data_wv6
data_wv6_inputed$V131[data_wv6$V131<0] = 5
for (i in c(2:8)){
  data_wv6_inputed[, i][data_wv6_inputed[, i] <0] = 5
  print("finished inputing for var", i)
}
#******** inputing for wv5
data_wv5_inputed = data_wv5
data_wv5_inputed$V152[data_wv5$V152<0] = 5

# for (i in c(2:8)){
#   data_wv6_inputed[, i][data_wv6_inputed[, i] <0] = 5
#   print("finished inputing for var", i)
# }

#### start modeling on data_wv6_inputed
library(psych)
alpha_by_country = function(x) {
    aaa = data_wv6_inputed %>%
    filter(country == x) %>%
    select(V131, V132, V133, V134, V135, V136, V139) %>%
    psych::alpha()
  return(paste("the raw alpha for", x, "is", aaa$total[1]))
}

b = alpha_by_country("Australia")


country_list = unique(data_wv6$country)[-8] #there is a NA tag at the 8th location
# alpha_country_wv6= c()
# for (i in 1:length(country_list)) {
#   alpha_country_wv6[i] = alpha_by_country(country_list[i])
# }

# alpha_country_twowaves_wv6 = c()
# for (i in 1:length(country_two_waves)) {
#   alpha_country_twowaves_wv6[i] = alpha_by_country(country_two_waves[i])
# }
# alpha_country_twowaves_wv6

################################################
#######ploting distribution of items by countries 
country_censored = country_w_wv5[-c(3, 17, 25)]
useful_wv5 <- data_wv5 %>%
  filter(country %in% country_censored)

head(useful_wv5)
  

# ggplot(useful_wv5, aes(x = factor(country), fill = factor(V152))) +
#   geom_bar(position = position_stack(), stat = "identity", width = .7) +
#   geom_text(aes(label = factor(V152) ), position = position_stack(vjust = 0.5), size = 2) +
#   coord_flip()
install.packages("ggthemes")
library(ggthemes)

  
jpeg(filename="tax_rich.jpeg", width = 1024, height = 1024)
ggplot(useful_wv5, aes(x=factor(country), y=V152)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()+
  coord_flip()+
  labs(title="Boxplot of 'tax the rich' by countries",y="Answer_value", x = "Country")+
  theme_fivethirtyeight()+
  theme(title = element_text(size = 26),
        axis.title = element_text(size = 22),
        text = element_text(size=20),
        plot.margin = unit(c(3,3,3,2), "cm"))
dev.off()
#########################################################################

additional_wv5 = dat %>%
  filter(country %in% country_censored) %>%
  select(country, V148, V150, V151, V162, V163)

###################################Selecting country

polity_6up = polity %>%
  filter(year == 2017 & democ >=6) %>%
  select(country, year, democ)

country_6up = intersect(polity_6up$country,country_vector)
country_6up_censored = country_6up[-c(3, 5, 17, 19, 25,26,27,28, 31)]


data_wv5_6up = data_wv5 %>%
  filter(country %in% country_6up_censored)
head(data_wv5_6up)

#########figure out the percentage of unnormal values
apply(data_wv5_6up[-1], 2, table)

tst = data_wv5_6up %>%
  group_by(country) %>%
  select(V153) %>%
  table()

for (i in 1:nrow(tst)) {
  print(prop.table(table(tst[i,])))
}

rowSums(round(prop.table(tst, 1),3)[,1:3])
sort(rowSums(round(prop.table(tst, 1),3)[,1:3]), TRUE)


############ exploratory about how manu entries left if delete all of nonnormals
## original size 
d = c()
for (cn in c("Georgia", "Japan", "Bulgaria", "Serbia", "Slovenia")){
  a = data_wv5_6up %>%
    filter(country == cn) %>%
    nrow()
  d = c(d, a)
}
d

## filtered size
b = c()
for (cn in c("Georgia", "Japan", "Bulgaria", "Serbia", "Slovenia")){
  a = data_wv5_6up %>%
    filter(country == cn) %>%
    filter(V152>0 & V153>0 & V154>0 & V155 >0 & V156 >0 & V157 >0 & V161>0) %>%
    nrow()
  b = c(b,a)
}
b
b/d
