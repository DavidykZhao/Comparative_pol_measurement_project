
results = data.frame(matrix(, nrow=5, ncol=13))
colnames(results) = c("model", "log-likelihood", "resid. df", "BIC",
                      "ABIC", "cAIC", "likelihood-ratio", "Entropy",
                      "Class1", "Class2", "Class3", "Class4", "Class5")
entropy<-function (p) sum(-p*log(p))

Argentina = nonnegtive %>%
  filter(country == "Argentina") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))


for (i in 1:5){
  #model = replicate(5, NA)
  model <- poLCA(f, data= Argentina, nclass= i, na.rm = FALSE, nrep=15, maxiter=3500) 
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


write.csv(results, paste("csv_each_country/Argentina",  ".csv", sep = ""), row.names = FALSE)
