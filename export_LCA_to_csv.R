
library(tidyverse)
library("poLCA")

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

## LCA formula, note that the first arg is your dataset. 
f = with(US, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 

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
write.csv(class_multinomial, "US_multinomial.csv", row.names = FALSE)
write.csv(data_class, "US_class_prob.csv", row.names = FALSE)

