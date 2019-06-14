library(dplyr)
#install.packages("dplyr")

dat <- readRDS("/Users/zhaoyikai/Desktop/Demo_MI_project/wv5.rds")

country_code = readxl::read_xlsx("country_code_5.xlsx", col_names = F)
colnames(country_code) = c("code", "country")
country_vector <- setNames(country_code$country, country_code$code)
dat$country = country_vector[as.character(dat$V2A)]


polity = readxl::read_xls("polity_2017.xls")
polity_6up = polity %>%
  filter(year == 2017 & democ >= 6) %>%
  select(country, year, democ)
country_name = polity_6up$country # select the atomic column using $ rather than []


country_6up = intersect(polity_6up$country,country_vector)
country_6up_censored = country_6up[-c(3, 5, 17, 19, 25,26,27,28, 31)]



demo_v5 = c("country", "V152", "V153", "V154", "V155", "V157", "V161") 
# No V156

nonnegtive = dat %>% 
  dplyr::select(demo_v5) %>%
  filter(country %in% country_6up_censored) %>%
  filter_all(all_vars(. > 0)) 

nonnegtive[,2:length(nonnegtive)] <- lapply(nonnegtive[,2:length(nonnegtive)], factor)
colnames(nonnegtive) = c("country", "tax", "religion", "free_election", "state_aid",
                         "civil_rights", "women")
#### Export csv
write.csv(nonnegtive, "nonzero_dataset.csv", row.names = FALSE)



