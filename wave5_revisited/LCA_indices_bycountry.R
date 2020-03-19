
# This is the script to generate results table for each individual countries

source('Plotting function.R')
nonnegative = read.csv("nonnegative_wv5_allvars.csv")


# The function to generate a csv output of LCA indices
gen_LCA_indices = function(country_name) {
  
  # The nonnegative ds here is the global variable
  country_codes = gen_country(nonnegative,country_name)[[1]]
  
  results = data.frame(matrix(NA, nrow=5, ncol=13))
  colnames(results) = c("model", "log-likelihood", "resid. df", "BIC",
                        "ABIC", "cAIC", "likelihood-ratio", "Entropy",
                        "Class1", "Class2", "Class3", "Class4", "Class5")
  entropy<-function (p) sum(-p*log(p))
  
  tryCatch({
    f = with(country_codes, cbind(tax, religion, free_election, state_aid, Army, civil_rights, 
                                  prospering, criminals, referendums, women)~1)
    for (i in 1:5){
      #model = replicate(5, NA)
      model <- poLCA(f, data= country_codes, nclass= i, na.rm = FALSE, nrep=15, maxiter=3500) 
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
      
      filename = paste("LCA_indices/", country_name,  ".csv", sep = "")
    }
  }, error = function(err) {
    
    filename = paste("BAD_FILE_", country_name,  ".csv", sep = "")
  }
  ) # End tryCatch

  
  write.csv(results, file = filename , row.names = FALSE)
  
}

# loop over countries to generate the indices for each individual countries into csv files
for (c in unique(nonnegative$country)) gen_LCA_indices(c)






