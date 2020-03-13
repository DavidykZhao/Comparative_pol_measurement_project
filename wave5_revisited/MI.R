
## read in wave5 nonnegative dataset

ds5 = read.csv('nonnegative_wv5_allvars.csv')


# ###reverse code 153 (religious takeover) & 156 (army takeover)
# nonnegtive_reversed = nonnegtive
# nonnegtive_reversed$V153 = 11 - nonnegtive$V153
# #table(nonnegtive_reversed$V153)
# 
# nonnegtive_reversed$V156 = 11 - nonnegtive$V156
# #table(nonnegtive_reversed$V156)

##############start MI ####################
library("lavaan") # Package for SEM
library(semTools)

get_index = function(fit) {
  round(fitMeasures(fit)[c(3,4,5,17,27,42,50)], 3)
}

# model_1 <-'
# demo =~ V152 + V153 + V154 + V155 + V157 + V161
# '
# MI = measurementInvariance(model_1, data = nonnegtive_reversed, group = "country")
# summary(MI, fit.measures = TRUE)
# get_index(MI)
# 
# # fit1 = cfa(model_1, data = nonnegtive_reversed, group = "country",
# #             estimator = "MLM")
# # get_index(fit1)
# # lavTestScore(fit1)
# # View(parTable(fit1))
# ########### 


####### setup country groups
uniq_country = unique(ds5$country)
western_country = c("Australia", "Canada", "Finland", "France", "Netherlands",
                    "Norway", "Sweden", "Spain","Switzerland", "United Kingdom", "United States")
Rest_world = setdiff(uniq_country, western_country)

western_data = ds5 %>%
  filter(country %in% western_country)
rest_data = ds5 %>%
  filter(country %in% Rest_world )
################



########### two factors cross two country groups 
model_procedural_large <- '
procedural =~ Army + criminals+ referendums + religion+ free_election + civil_rights + women
'

model_procedural_small <- '
procedural =~ religion+ free_election + civil_rights + women
'

model_substantive <- '
substantive =~ tax + state_aid + prospering 
'
model_all <- '
all =~ tax + state_aid + prospering + Army + criminals+ referendums + religion+ free_election + civil_rights + women
'

#################################
# init a table for storing MI indices of different var groups for different country groups
MI_results = data.frame(matrix(data = NA, 16, 8))
colnames(MI_results) = c("situation", "chisq",  "df"   , "pvalue" , "cfi" ,  
                         "cfi.robust",   "rmsea",       
                         "rmsea.robust")
# -- write a funtion to generate

MI_generator = function(data, model, metric) {
  if(metric) {
    MI_obj = cfa(model, data = data, group = "country", 
                 group.equal = c("loadings"),
                 estimator = "MLM")
  } else {
    MI_obj = cfa(model, data = data, group = "country", 
                 group.equal = c("loadings", "intercepts"),
                 estimator = "MLM")
  }

  print(get_index(MI_obj))
  return(get_index(MI_obj))
}


# ----- western-model_procedural-metric
MI_procedural_western_metric = cfa(model_procedural, data = western_data, group = "country", 
                            group.equal = c("loadings"),
                   estimator = "MLM")
#summary(MI_procedural_western_metric, fit.measures = TRUE)
get_index(MI_procedural_western_metric)

MI_results[1, ] = c("procedural_western_metric", get_index(MI_procedural_western_metric))

# ----- western-model_procedural-scalar

MI_procedural_western_scalar = cfa(model_procedural, data = western_data, group = "country", 
                                   group.equal = c("loadings", "intercepts"),
                                   estimator = "MLM")
get_index(MI_procedural_western_scalar)

MI_results[2, ] = c("procedural_western_scalar", get_index(MI_procedural_western_scalar))

# ----- western-model_all-metric


MI_results[3, ] = c("all_western_metric", MI_generator(western_data, model_all, TRUE)
)

# ----- western-model_all-scalar


MI_results[4, ] = c("all_western_scalar", MI_generator(western_data, model_all, FALSE)
)

# ----- western-model_substantive-metric


MI_results[5, ] = c("substantive_western_metric", MI_generator(western_data, model_substantive, TRUE)
)

# ----- western-model_substantive-scalar


MI_results[6, ] = c("substantive_western_scalar", MI_generator(western_data, model_substantive, FALSE), 
                    "ov variance negative")

# ----- western-model_procedural_small-metric


MI_results[7, ] = c("procedural_small_western_metric", MI_generator(western_data, model_procedural_small, TRUE), "NA"
)

# ----- western-model_procedural_small-scalar


MI_results[8, ] = c("procedural_small_western_scalar", MI_generator(western_data, model_procedural_small, FALSE), NA
)


# ----- rest- model_procedural- metric

MI = cfa(model_procedural, data = rest_data, group = "country", 
                                group.equal = c("loadings", "intercepts"),  estimator = "MLM")

MI_results[9, ] = c("procedural_rest_metric",get_index(MI), NA)


# ----- rest- model_procedural- metric

MI = cfa(model_procedural, data = rest_data, group = "country", 
         group.equal = c("loadings"),  estimator = "MLM", check.gradient = FALSE)

MI_results[9, ] = c("procedural_rest_metric",get_index(MI), NA)

# ----- rest- model_procedural- scalar

MI = cfa(model_procedural, data = rest_data, group = "country", 
         group.equal = c("loadings", "intercepts"),  estimator = "MLM")

MI_results[10, ] = c("procedural_rest_scalar",get_index(MI), NA)

# ----- rest- model_all- metric

MI = cfa(model_all, data = rest_data, group = "country", 
         group.equal = c("loadings"),  estimator = "MLM")

MI_results[11, ] = c("all_rest_metric",get_index(MI), NA)

# ----- rest- model_all- scalar

MI = cfa(model_all, data = rest_data, group = "country", 
         group.equal = c("loadings", "intercepts"),  estimator = "MLM")

MI_results[12, ] = c("all_rest_scalar",get_index(MI), NA)

# ----- rest- model_substantive - metric

MI = cfa(model_substantive, data = rest_data, group = "country", 
         group.equal = c("loadings"),  estimator = "MLM")

MI_results[13, ] = c("substantive_rest_metric",get_index(MI), NA)

# ----- rest- model_substantive - scalar

MI = cfa(model_substantive, data = rest_data, group = "country", 
         group.equal = c("loadings" , "intercepts"),  estimator = "MLM")

MI_results[14, ] = c("substantive_rest_scalar",get_index(MI), NA)


# ----- rest- model_procedural_small - metric

MI = cfa(model_procedural_small, data = rest_data, group = "country", 
         group.equal = c("loadings"),  estimator = "MLM")

MI_results[15, ] = c("procedural_small_rest_metric",get_index(MI), NA)

# ----- rest- model_procedural_small - scalar

MI = cfa(model_procedural_small, data = rest_data, group = "country", 
         group.equal = c("loadings", "intercepts"),  estimator = "MLM")

MI_results[16, ] = c("procedural_small_rest_scalar", get_index(MI), "ov variance negative")

# -------- Here concludes the MI table

write.csv(MI_results, row.names = FALSE, file = "MI_table.csv")
  


