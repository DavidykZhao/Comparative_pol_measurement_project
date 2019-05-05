## Delete all of the data entries with unnormal values
library(tidyverse)
table(data_wv5_6up$country)

nonnegtive = data_wv5_6up %>%
  filter_all(all_vars(. > 0)) %>%
  select(-V156)

(table(nonnegtive$country))/(table(data_wv5_6up$country))

###reverse code 153 (religious takeover) & 156 (army takeover)
nonnegtive_reversed = nonnegtive
#table(nonnegtive$V153)
nonnegtive_reversed$V153 = 11 - nonnegtive$V153
#table(nonnegtive_reversed$V153)

#table(nonnegtive$V156)
nonnegtive_reversed$V156 = 11 - nonnegtive$V156
#table(nonnegtive_reversed$V156)

##############start MI ####################
library("lavaan") # Package for SEM
library(semTools)

get_index = function(fit) {
  round(fitMeasures(fit)[c(3,4,5,17,27,42,50)], 3)
}

model_1 <-'
demo =~ V152 + V153 + V154 + V155 + V157 + V161
'
MI = measurementInvariance(model_1, data = nonnegtive_reversed, group = "country")
summary(MI, fit.measures = TRUE)
get_index(MI)

# fit1 = cfa(model_1, data = nonnegtive_reversed, group = "country",
#             estimator = "MLM")
# get_index(fit1)
# lavTestScore(fit1)
# View(parTable(fit1))
########### 
#### model1: 6 vars, all countries
model_1 <-'
demo =~ V152 + V153 + V154 + V155 + V157 + V161
'
MI = measurementInvariance(model_1, data = nonnegtive_reversed, group = "country")

####model2: 6 vars in two country groups
table(nonnegtive_reversed$country)
uniq_country = unique(nonnegtive_reversed$country)
western_country = c("Australia", "Canada", "Finland", "France", "Netherlands",
                    "Norway", "Sweden", "United Kingdom", "United States")
Rest_world = setdiff(uniq_country, western_country)
western_data = nonnegtive_reversed %>%
  filter(country %in% western_country )
rest_data = nonnegtive_reversed %>%
  filter(country %in% Rest_world )

MI_6var_western = measurementInvariance(model_1, data = western_data, group = "country")
MI_6var_rest = measurementInvariance(model_1, data = rest_data, group = "country")

########### two factors cross two country groups 
model_welfare <- '
welfare =~ V152 + V155
'

model_civil <- '
civil =~ V153 + V154 + V157 + V161
'


#MI_welfare_western = measurementInvariance(model_welfare, data = western_data, group = "country")
MI_welfare_western = cfa(model_welfare, data = rest_data, group = "country", group.equal = c("loadings"),
                   estimator = "MLM")
summary(MI_welfare_western, fit.measures = TRUE)
get_index(MI_welfare_western)

##### civil_western_loadingonly vs intercepts
MI_western_rest = cfa(model_civil, data = western_data, group = "country", group.equal = c("loadings"),
                         estimator = "MLM")
get_index(MI_western_rest)
MI_western_rest = cfa(model_civil, data = western_data, group = "country", group.equal = c("loadings", "intercepts"),
                      estimator = "MLM")
get_index(MI_western_rest)

names = colnames(western_data)[-1]


     
cn = unique(western_data$country) 


for (i in names){
  #print(i)
  print(cor(ausie$V152, ausie[i]))
}

  
ausie = western_data %>%
  filter(country == "Australia")
  


