#install.packages("poLCA")
library("poLCA")
library("reshape2")
library(tidyverse)

nonnegtive = read.csv("nonnegative_dataset.csv")
### turn columns into factor type
nonnegtive[,2:length(nonnegtive)] <- lapply(nonnegtive[,2:length(nonnegtive)], factor)
colnames(nonnegtive) = c("country", "tax", "religion", "free_election", "state_aid",
                         "civil_rights", "women")


############ Recode 10-scale to 3-level

# recode = function(df){
#   for (i in df){
#     cut(i,
#         breaks=c(0, 4, 5, 10),
#         labels=c())
#   }
#   return(df)
# }
# 
# df_3level = recode(nonnegtive)
# 
# #for (i in nonnegtive) print(summary(i))


############

AUS = nonnegtive %>%
  filter(country == "Australia") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))

US = nonnegtive %>%
  filter(country == "United States") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))

SWEDEN = nonnegtive %>%
  filter(country == "Sweden") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))

f = with(US, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 

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
LCA_best_model
probs = LCA_best_model$probs
probs$women %*% 1:10

profile_tb = data.frame(
                        tax = replicate(2, NA),
                        religion = replicate(2, NA),
                        free_election = replicate(2, NA),
                        state_aid = replicate(2, NA),
                        civil_rights = replicate(2, NA),
                        women = replicate(2, NA))

for (i in 1:6) {
  if (length(probs[[i]][1,]) < 10) {

    probs[[i]] = cbind(probs[[i]], matrix(0, nrow = nrow(probs[[i]]), ncol = 10 - ncol(probs[[i]]))) 
  } 
  profile_tb[, i] = probs[[i]] %*% 1:10
}


rownames(profile_tb) = c("class1", "class2")
profile_tb = rownames_to_column(profile_tb)
colnames(profile_tb)[1] = "class"
profile_long = reshape::melt(profile_tb, id.vars = "class")


p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class)) +
  geom_point(size = 2.25)+
  geom_line(size = 1.25) +
  labs(x = NULL, y = "Mean value of the response", main = "Profile plot") +
  theme_bw(base_size = 14)

library(plotly)

ggplotly(p, tooltip = "all") %>%
  layout(legend = list(orientation = "h", y = 1.2))


######

LCA_best_model$P

