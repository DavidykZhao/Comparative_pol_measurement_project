# To generate pooled LCA incice csv and plot
# --------- To generate LCA incices csv
source('Plotting function.R')
nonnegative = read.csv("nonnegative_wv5_allvars.csv")
# Getting only the numeric columns without the country column
num_data = nonnegative[, -1]


results = data.frame(matrix(, nrow=12, ncol=20))
colnames(results) = c("model", "log-likelihood", "resid_df", "BIC",
                      "ABIC", "cAIC", "likelihood-ratio", "Entropy",
                      "Class1", "Class2", "Class3", "Class4", "Class5",
                      "Class6", "Class7", "Class8", "Class9", "Class10",
                      "Class11", "Class12")
entropy<-function (p) sum(-p*log(p))
f = with(num_data, cbind(tax, religion, free_election, state_aid, Army, civil_rights, 
                              prospering, criminals, referendums, women)~1)
for (i in 1:12){
  model <- poLCA(f, data= num_data, nclass= i, na.rm = FALSE, nrep=15, maxiter=3500) 
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
  results[i, 9:20] = c(round(model$P,3), rep("-", 12-i))
}

write.csv(results, "pooled_LCA.csv", row.names = FALSE)


results1 = data.frame(matrix(, nrow=3, ncol=23))

for (i in 13:15){
  model <- poLCA(f, data= num_data, nclass= i, na.rm = FALSE, nrep=10, maxiter=3000) 
  results1[i-12,1] = paste("model", i)
  results1[i-12,2]<- model$llik
  results1[i-12,3]<- model$resid.df
  results1[i-12,4]<- model$bic
  results1[i-12,5]<- (-2* model$llik) + ((log((model$N + 2)/24)) * model$npar) #abic
  results1[i-12,6]<- (-2* model$llik) + model$npar * (1 + log(model$N)) #caic
  results1[i-12,7]<- model$Gsq
  results1[i-12,8] <- round(((entropy(model$P) - mean(apply(model$posterior,1, entropy),na.rm = TRUE)) / entropy(model$P)),3)
  if (i == 1) {
    results1[i, 8] = c("-")
  }
  results1[i-12, 9:20] = c(round(model$P,3), rep("-", 15-i))
}

# started at 9:30 pm; ended at 1:18am
results_15_class = rbind(results, results1)
write.csv(results_15_class, "pooled_LCA_final.csv", row.names = FALSE)


# --------- Add more clusters
ls_container = list()
results2 = data.frame(matrix(, nrow=3, ncol=28))

for (i in 16:20){
  model <- poLCA(f, data= num_data, nclass= i, na.rm = FALSE, nrep=5, maxiter=2000) 
  ls_container[[i-15]] = model
  results2[i-15,1] = paste("model", i)
  results2[i-15,2]<- model$llik
  results2[i-15,3]<- model$resid.df
  results2[i-15,4]<- model$bic
  results2[i-15,5]<- (-2* model$llik) + ((log((model$N + 2)/24)) * model$npar) #abic
  results2[i-15,6]<- (-2* model$llik) + model$npar * (1 + log(model$N)) #caic
  results2[i-15,7]<- model$Gsq
  results2[i-15,8] <- round(((entropy(model$P) - mean(apply(model$posterior,1, entropy),na.rm = TRUE)) / entropy(model$P)),3)
  if (i == 1) {
    results2[i, 8] = c("-")
  }
  results2[i-15, 9:28] = c(round(model$P,3), rep("-", 20-i))
}

colnames(results2) = c("model", "log-likelihood", "resid_df", "BIC",
                      "ABIC", "cAIC", "likelihood-ratio", "Entropy",
                      "Class1", "Class2", "Class3", "Class4", "Class5",
                      "Class6", "Class7", "Class8", "Class9", "Class10",
                      "Class11", "Class12","Class13", "Class14",
                      "Class15", "Class16","Class17", "Class18",
                      "Class19", "Class20")

results_15_class[1:15, 24:28] = matrix(data= "-", nrow = 15, ncol = 5)
colnames(results_15_class)[24:28] = c("Class16","Class17", "Class18",
                                      "Class19", "Class20")
results_20_class = rbind(results_15_class, results2)
write.csv(results_20_class, "pooled_LCA_final_2.csv", row.names = FALSE)



# --------- Add more clusters 2nd round
ls_container = list()
results3 = data.frame(matrix(, nrow=5, ncol=33))

for (i in 21:25){
  model <- poLCA(f, data= num_data, nclass= i, na.rm = FALSE, nrep=5, maxiter=2000) 
  ls_container[[i-20]] = model
  results3[i-20,1] = paste("model", i)
  results3[i-20,2]<- model$llik
  results3[i-20,3]<- model$resid.df
  results3[i-20,4]<- model$bic
  results3[i-20,5]<- (-2* model$llik) + ((log((model$N + 2)/24)) * model$npar) #abic
  results3[i-20,6]<- (-2* model$llik) + model$npar * (1 + log(model$N)) #caic
  results3[i-20,7]<- model$Gsq
  results3[i-20,8] <- round(((entropy(model$P) - mean(apply(model$posterior,1, entropy),na.rm = TRUE)) / entropy(model$P)),3)
  if (i == 1) {
    results2[i, 8] = c("-")
  }
  results2[i-20, 9:33] = c(round(model$P,3), rep("-", 25-i))
}


x <- serialize(ls_container, NULL)
unserialize(x)

# -------------- To generate pooled plot

f = with(data, cbind(tax, religion, free_election, state_aid, Army, civil_rights, 
                     prospering, criminals, referendums, women)~1) 
   	
LCA_best_model = poLCA(f, num_data, nclass=i, maxiter=3000, 
                       tol=1e-5, na.rm=FALSE,  
                       nrep=10, verbose=TRUE, calc.se=TRUE)
probs = LCA_best_model$probs
n_class = length(LCA_best_model$P)

profile_tb = data.frame(
  tax = rep(NA, n_class),
  religion = rep(NA, n_class),
  free_election = rep(NA, n_class),
  state_aid = rep(NA, n_class),
  Army = rep(NA, n_class), # add Army var
  civil_rights = rep(NA, n_class),
  women = rep(NA, n_class))

for (i in 1:7) {
  if (length(probs[[i]][1,]) < 10) {
    probs[[i]] = cbind(probs[[i]], matrix(0, nrow = nrow(probs[[i]]), ncol = 10 - ncol(probs[[i]]))) 
  } 
  profile_tb[, i] = probs[[i]] %*% 1:10
}

rownames(profile_tb) = paste(rep("class", n_class), seq(1, n_class, 1), sep = "_")
profile_tb = rownames_to_column(profile_tb)
colnames(profile_tb)[1] = "class"
profile_long = reshape::melt(profile_tb, id.vars = "class")

write.csv(profile_long, "pooled_output.csv", row.names = FALSE)
p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class)) +
  geom_point(size = 2.25)+
  geom_line(size = 1.00) +
  labs(x = NULL, y = "Mean value of the response") +
  theme_bw(base_size = 12)+
  ggtitle(paste(paste("Class", 1:length(LCA_best_model$P), sep = "_"), ":", 
                round(LCA_best_model$P, 3), collapse = ", "))+
  theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm"))+
  theme(axis.text.x = element_text(size = 9))+
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  scale_color_discrete(name = "Class & probability", labels = paste(paste("Class", 1:length(LCA_best_model$P), sep = "_"), ":", 
                                                                    round(LCA_best_model$P, 3)))+
  labs(title = paste("The profile plot of", country_name))+
  theme(legend.title = element_text(size = 12))+
  theme(
    plot.title = element_text(hjust = 0)
  )+
  theme(legend.position = "bottom")




