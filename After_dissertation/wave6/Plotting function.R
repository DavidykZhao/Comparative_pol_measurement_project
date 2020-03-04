
#setwd("/Users/zhaoyikai/Comparative_pol_measurement_project/After_dissertation/wave6")



profile_plot = function(data_ls, wv){
  # wv is just an int denoting the # of the wave, taking 5 or 6.
  # The arg is a list
  data = data_ls[[1]]
  country_name = data_ls[[2]]
  require(poLCA)
  f = with(data, cbind(tax, religion, free_election, state_aid, Army, civil_rights, women)~1) 
  min_bic <- 1000000
  
  for(i in 1:7){
    lc <- poLCA(f, data, nclass=i, maxiter=3000, 
                tol=1e-5, na.rm=FALSE,  
                nrep=10, verbose=TRUE, calc.se=TRUE)
    if(lc$bic < min_bic){
      min_bic <- lc$bic
      LCA_best_model <- lc
    }
  }    	
  
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
    labs(title = paste("The profile plot of", country_name, "of wave", wv))+
    theme(legend.title = element_text(size = 12))+
    theme(
      plot.title = element_text(hjust = 0)
    )+
    theme(legend.position = "bottom")

  
  
  
  
  #print(p)
  return(p)
  
  # library(plotly)
  # 
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", y = 1.2))
  # print(plotly_p)
  # return(plotly_p)
}

gen_country = function(df_name, country_name){
  a = df_name %>%
    filter(country == country_name) %>%
    dplyr::select(c("tax", "religion", "free_election", "state_aid", "Army",
                    "civil_rights", "women"))
  
  return(list(a, country_name)) # now returns a list!
}

# country_list = unique(nonnegtive$country)
# 
# 
# for (country in country_list) {
#   pdf(paste(country,"LCA.pdf", sep = "_"), width = 9) 
#   profile_plot(gen_country(country))
#   dev.off()  
# }


















#######-----------Prototying code
# f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 
# min_bic <- 100000
# 
# 
# for(i in 2:7){
#   lc <- poLCA(f, data, nclass=i, maxiter=3000, 
#               tol=1e-5, na.rm=FALSE,  
#               nrep=10, verbose=TRUE, calc.se=TRUE)
#   if(lc$bic < min_bic){
#     min_bic <- lc$bic
#     LCA_best_model<-lc
#   }
# }    	
# 
# probs = LCA_best_model$probs
# n_class = length(LCA_best_model$P)
# 
# profile_tb = data.frame(
#   tax = replicate(n_class, NA),
#   religion = replicate(n_class, NA),
#   free_election = replicate(n_class, NA),
#   state_aid = replicate(n_class, NA),
#   civil_rights = replicate(n_class, NA),
#   women = replicate(n_class, NA))
# 
# for (i in 1:6) {
#   if (length(probs[[i]][1,]) < 10) {
#     probs[[i]] = cbind(probs[[i]], matrix(0, nrow = nrow(probs[[i]]), ncol = 10 - ncol(probs[[i]]))) 
#   } 
#   profile_tb[, i] = probs[[i]] %*% 1:10
# }
# 
# 
# rownames(profile_tb) = paste(rep("class", n_class), seq(1, n_class, 1), sep = "_")
# profile_tb = rownames_to_column(profile_tb)
# colnames(profile_tb)[1] = "class"
# profile_long = reshape::melt(profile_tb, id.vars = "class")
# 
# 
# p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class)) +
#   geom_point(size = 2.25)+
#   geom_line(size = 1.25) +
#   labs(x = NULL, y = "Mean value of the response", main = "Profile plot") +
#   theme_bw(base_size = 14)
# 
# library(plotly)
# 
# ggplotly(p, tooltip = "all") %>%
#   layout(legend = list(orientation = "h", y = 1.2))