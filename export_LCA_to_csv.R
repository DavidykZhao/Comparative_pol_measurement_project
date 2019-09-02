#install.packages(c("ggplot2", "tibble", "tidyr", "purrr", "stringr"))
library(tidyverse)
library("poLCA")
library(dplyr)
## read in the nonnegative dataset (without NA's and "Dont'know"s)
nonnegtive = read.csv("nonnegative_dataset.csv")

### turn columns into factor type
nonnegtive[,2:length(nonnegtive)] <- lapply(nonnegtive[,2:length(nonnegtive)], factor)
colnames(nonnegtive) = c("country", "tax", "religion", "free_election", "state_aid",
                         "civil_rights", "women")


## Generate dataset for a certain country if needed
US = nonnegtive %>%
  filter(country == "United States") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))
mydata = nonnegtive %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))
## LCA formula, note that the first arg is your dataset. 
f = with(US, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 

# producing the results table
## models with different number of groups without covariates:
set.seed(01012)
lc1<-poLCA(f, data=mydata, nclass=1, na.rm = FALSE, nrep=10, maxiter=3000) #Loglinear independence model.
lc2<-poLCA(f, data=mydata, nclass=2, na.rm = FALSE, nrep=10, maxiter=3000)
lc3<-poLCA(f, data=mydata, nclass=3, na.rm = FALSE, nrep=10, maxiter=3000)
lc4<-poLCA(f, data=mydata, nclass=4, na.rm = FALSE, nrep=10, maxiter=3000) 
lc5<-poLCA(f, data=mydata, nclass=5, na.rm = FALSE, nrep=10, maxiter=3000)
lc6<-poLCA(f, data=mydata, nclass=6, na.rm = FALSE, nrep=10, maxiter=3000)
lc7<-poLCA(f, data=mydata, nclass=7, na.rm = FALSE, nrep=10, maxiter=3000)
lc8<-poLCA(f, data=mydata, nclass=8, na.rm = FALSE, nrep=10, maxiter=3000) 
lc9<-poLCA(f, data=mydata, nclass=9, na.rm = FALSE, nrep=5, maxiter=1000)
lc10<-poLCA(f, data=mydata, nclass=10, na.rm = FALSE, nrep=5, maxiter=2000)
lc11<-poLCA(f, data=mydata, nclass=11, na.rm = FALSE, nrep=5, maxiter=2000)
lc12<-poLCA(f, data=mydata, nclass=12, na.rm = FALSE, nrep=5, maxiter=2000)
lc13<-poLCA(f, data=mydata, nclass=13, na.rm = FALSE, nrep=5, maxiter=2000)

# generate dataframe with fit-values

results <- data.frame(Modell=c("Modell 1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)
results$Modell<-as.integer(results$Modell)
results[1,1]<-c("Modell 1")
results[2,1]<-c("Model 2")
results[3,1]<-c("Model 3")
results[4,1]<-c("Model 4")
results[5,1]<-c("Model 5")
results[6,1]<-c("Model 6")
results[7,1]<-c("Model 7")
results[8,1]<-c("Model 8")
results[9,1]<-c("Model 9")
results[10,1]<-c("Model 10")
results[11,1]<-c("Model 11")
results[12,1]<-c("Model 12")
results[13,1]<-c("Model 13")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik
results[7,2]<-lc7$llik
results[8,2]<-lc8$llik
results[9,2]<-lc9$llik
results[10,2]<-lc10$llik
results[11,2]<-lc11$llik
results[12,2]<-lc12$llik
results[13,2]<-lc13$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df
results[7,3]<-lc7$resid.df
results[8,3]<-lc8$resid.df
results[9,3]<-lc9$resid.df
results[10,3]<-lc10$resid.df
results[11,3]<-lc11$resid.df
results[12,3]<-lc12$resid.df
results[13,3]<-lc13$resid.df

results[2,4]<-lc2$bic
results[3,4]<-lc3$bic
results[4,4]<-lc4$bic
results[5,4]<-lc5$bic
results[6,4]<-lc6$bic
results[7,4]<-lc7$bic
results[8,4]<-lc8$bic
results[9,4]<-lc9$bic
results[10,4]<-lc10$bic
results[11,4]<-lc11$bic
results[12,4]<-lc12$bic
results[13,4]<-lc13$bic

results[2,5]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,5]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,5]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,5]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,5]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
results[7,5]<-(-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
results[8,5]<-(-2*lc8$llik) + ((log((lc8$N + 2)/24)) * lc8$npar)
results[9,5]<-(-2*lc9$llik) + ((log((lc9$N + 2)/24)) * lc9$npar)
results[10,5]<-(-2*lc10$llik) + ((log((lc10$N + 2)/24)) * lc10$npar)
results[11,5]<-(-2*lc11$llik) + ((log((lc11$N + 2)/24)) * lc11$npar)
results[12,5]<-(-2*lc12$llik) + ((log((lc12$N + 2)/24)) * lc12$npar)
results[13,5]<-(-2*lc13$llik) + ((log((lc13$N + 2)/24)) * lc13$npar)

results[2,6]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,6]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,6]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,6]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,6]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
results[7,6]<- (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))
results[8,6]<- (-2*lc8$llik) + lc8$npar * (1 + log(lc8$N))
results[9,6]<- (-2*lc9$llik) + lc9$npar * (1 + log(lc9$N))
results[10,6]<- (-2*lc10$llik) + lc10$npar * (1 + log(lc10$N))
results[11,6]<- (-2*lc11$llik) + lc11$npar * (1 + log(lc11$N))
results[12,6]<- (-2*lc12$llik) + lc12$npar * (1 + log(lc12$N))
results[13,6]<- (-2*lc13$llik) + lc13$npar * (1 + log(lc13$N))


results[2,7]<-lc2$Gsq
results[3,7]<-lc3$Gsq
results[4,7]<-lc4$Gsq
results[5,7]<-lc5$Gsq
results[6,7]<-lc6$Gsq
results[7,7]<-lc7$Gsq
results[8,7]<-lc8$Gsq
results[9,7]<-lc9$Gsq
results[10,7]<-lc10$Gsq
results[11,7]<-lc11$Gsq
results[12,7]<-lc12$Gsq
results[13,7]<-lc13$Gsq











## find out the best model --> yiedling a best model object
min_bic <- 100000
for(i in 2:7){
  lc <- poLCA(f, US, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	

# produce the posterior matrix; round up the posterior to the closest 3 digit
posterior_three_class = round(LCA_best_model$posterior, 3)

# produce the predicted class vector 
pred_class = LCA_best_model$predclass

# column bind the data matrix with the posterior matrix and pred_class vector
data_class = cbind(US, posterior_three_class, pred_class)

# change the column name. Note that the classes may not be 3 in other datasets
colnames(data_class)[7:9] <- c(paste(("posterior_C"), c(1:3), sep = ""))

# produce the multinomial probability of each class to each question matrix
class_multinomial = LCA_best_model$probs

# write csv out 
#
write.csv(results, "ALL_data_class.csv", row.names = FALSE)
write.csv(class_multinomial, "US_multinomial.csv", row.names = FALSE)
write.csv(data_class, "US_class_prob.csv", row.names = FALSE)

