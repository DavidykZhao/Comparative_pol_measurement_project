AUS = nonnegtive %>%
  filter(country == "Australia") %>%
  dplyr::select(c("tax", "religion", "free_election", "state_aid",
                  "civil_rights", "women"))



lc <- poLCA(f, AUS, nclass=4, maxiter=3000, 
            tol=1e-5, na.rm=FALSE,  
            nrep=10, verbose=TRUE, calc.se=TRUE)

pred_class = lc$predclass
data_class = cbind(AUS, pred_class)



library(tidyverse)

g1 <-  ggplot(c4, aes(x = free_election, y = state_aid))+
  geom_jitter(alpha = 0.4)
g2 <-  ggplot(c3, aes(x = free_election, y = state_aid))+
  geom_jitter(alpha = 0.4)




for (i in 1:5){
  cl = data_class %>%
    filter(pred_class == i)
  print(cor(as.numeric(cl$tax), as.numeric(cl$state_aid)))
}
cor(as.numeric(data_class$tax), as.numeric(data_class$state_aid))



glimpse(c4)
