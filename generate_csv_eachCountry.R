###################TO generate csv output for each individual country

# There is a function to generate model running results and write to csv

## Generate dataset for a certain country if needed
US = nonnegtive %>%
  filter(country == "United States") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))
mydata = nonnegtive %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))
## LCA formula, note that the first arg is your dataset. 
f = with(US, cbind(tax, religion, free_election, state_aid, 
                   civil_rights, women)~1) 


## Some necessary funcitons
gen_country = function(country_name){
  a = nonnegtive %>%
    filter(country == country_name) %>%
    dplyr::select(c("tax", "religion", "free_election", "state_aid",
                    "civil_rights", "women"))
  return(a)
}




get_country_csv = function(country_name){
  country = gen_country(country_name)
  results = data.frame(matrix(, nrow=5, ncol=13))
  colnames(results) = c("model", "log-likelihood", "resid. df", "BIC",
                        "ABIC", "cAIC", "likelihood-ratio", "Entropy",
                        "Class1", "Class2", "Class3", "Class4", "Class5")
  entropy<-function (p) sum(-p*log(p))
  
  for (i in 1:5){
    #model = replicate(5, NA)
    model <- poLCA(f, data= US, nclass= i, na.rm = FALSE, nrep=15, maxiter=3500) 
    results[i,1] = paste("model", i)
    results[i,2]<- model$llik
    results[i,3]<- model$resid.df
    results[i,4]<- model$bic
    results[i,5]<- (-2* model$llik) + ((log((model$N + 2)/24)) * model$npar) #abic
    results[i,6]<- (-2* model$llik) + model$npar * (1 + log(model$N)) #caic
    results[i,7]<- model$Gsq
    results[i,8] <- round(((entropy(model$P) - mean(apply(model$posterior,1, entropy),na.rm = TRUE)) / entropy(model$P)),3)
    if (i == 1) {
      results[i, 8] = c("-")
    }
    results[i, 9:13] = c(round(model$P,3), rep("-", 5-i))
  }
  write.csv(results, paste("csv_each_country/", country_name, ".csv", sep = ""), row.names = FALSE)
}

country_list = unique(nonnegtive$country)[-c(1,19)]
#"Argentina",  "Sweden" NEED FURTHER WORK


for (i in country_list){
  get_country_csv(i)
}


model <- poLCA(f, data= US, nclass= 3, na.rm = FALSE, nrep=15, maxiter=3500) 
model$Chisq



## To calculate classification error
posteriors <- data.frame(model$posterior, predclass=model$predclass) 
classification_table <-
  ddply(posteriors, .(predclass), function(x) colSums(x[,1:3])) 
sum(diag(as.matrix(classification_table[2:4])))/sum(classification_table)
##################################

results = data.frame(matrix(, nrow=5, ncol=13))
colnames(results) = c("model", "log-likelihood", "resid. df", "BIC",
                      "ABIC", "cAIC", "likelihood-ratio", "Entropy",
                      "Class1", "Class2", "Class3", "Class4", "Class5")





