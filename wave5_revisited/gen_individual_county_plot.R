data= read.csv("nonnegative_wv5_allvars.csv")

countries = as.character(unique(data$country))

source('Plotting function.R')

library(tidyverse)

for (i in countries) {
  profile_plot(gen_country(data, i))
  ggsave(paste(i, "_plot.png"), width = 10)
}


# The multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


country_names = c("Brazil", "Japan", "United States", "Sweden")
gs = vector("list", length(country_names))


# def a minimal profile_plot that could be generated from the optimal #
# and also without axis.text.x
profile_plot_minimal = function(data_ls, num_class){
  # The arg is a list
  data = data_ls[[1]]
  country_name = data_ls[[2]]
  require(poLCA)
  f = with(data, cbind(tax, religion, free_election, state_aid, Army, civil_rights, 
                       prospering, criminals, referendums, women)~1) 
  
  
  LCA_best_model <- poLCA(f, data, nclass=num_class, maxiter=2000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=5, verbose=TRUE, calc.se=TRUE)  	
  
  probs = LCA_best_model$probs
  n_class = length(LCA_best_model$P)
  
  profile_tb = data.frame(
    tax = rep(NA, n_class),
    religion = rep(NA, n_class),
    free_election = rep(NA, n_class),
    state_aid = rep(NA, n_class),
    army = rep(NA, n_class), # add Army var
    civil_rights = rep(NA, n_class),
    prospering = rep(NA, n_class) , 
    criminals = rep(NA, n_class), 
    referendums = rep(NA, n_class),
    women = rep(NA, n_class))
  
  for (i in 1:10) {
    if (length(probs[[i]][1,]) < 10) {
      probs[[i]] = cbind(probs[[i]], matrix(0, nrow = nrow(probs[[i]]), ncol = 10 - ncol(probs[[i]]))) 
    } 
    profile_tb[, i] = probs[[i]] %*% 1:10
  }
  
  rownames(profile_tb) = paste(rep("class", n_class), seq(1, n_class, 1), sep = "_")
  profile_tb = rownames_to_column(profile_tb)
  colnames(profile_tb)[1] = "class"
  profile_long = reshape::melt(profile_tb, id.vars = "class")
  
  #write.csv(profile_long, "pooled_output.csv", row.names = FALSE)
  # p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class)) +
  #   geom_point(size = 2.25)+
  #   geom_line(size = 1.00) +
  #   labs(x = NULL, y = "Mean value of the response") +
  #   theme_bw(base_size = 12)+
  #   theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  #   theme(axis.text.x = element_text(size = 9))+
  #   scale_color_discrete(name = "Class & probability", labels = paste(paste("Class", 1:length(LCA_best_model$P), sep = "_"), ":", 
  #                                                                     round(LCA_best_model$P, 3)))+
  #   labs(title = country_name)+
  #   theme(legend.title = element_text(size = 12))+
  #   theme(
  #     plot.title = element_text(hjust = 0),
  #     panel.border = element_blank()
  #   )+
  #   theme(legend.position = "bottom")
  
  p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class)) +
    geom_point(size = 2.25)+
    geom_line(size = 1.00) +
    labs(x = NULL, y = "Mean value of the response") +
    theme_minimal()+
    # theme(plot.margin=unit(c(1,1,1,1),"cm"))+
    #theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
    theme(axis.text.x = element_blank())+
    theme(plot.title = element_text(hjust = 0, size = 10))+
    theme(legend.position = "none")+
    theme(axis.title.y = element_blank())+
    theme(axis.ticks.x = element_blank())+
    theme(axis.text.x = element_text(size = 7))+
    scale_color_discrete(name = "Class & probability", labels = paste(paste("Class", 1:length(LCA_best_model$P), sep = "_"), ":", 
                                                                      round(LCA_best_model$P, 3)))+
    labs(title = country_name)+
    theme(legend.position = c(0.7, 0.0),
          legend.title = element_text(size=6.5),
          legend.text = element_text(size=6))+
    guides(col = guide_legend(nrow = 1))
    
  
  
  
  print(p)
  return(p)
  
  # library(plotly)
  # 
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", y = 1.2))
  # print(plotly_p)
  # return(plotly_p)
}
###########

g1 = profile_plot_minimal(gen_country(data, "Japan"), 3)
g2 = profile_plot_minimal(gen_country(data, "Brazil"), 3)
g3 = profile_plot_minimal(gen_country(data, "United States"), 4)
g4 = profile_plot_minimal(gen_country(data, "Sweden"), 2)

g1 = g1+theme(
  axis.text.x = element_blank()
)

g2 = g2+theme(
  axis.text.x = element_blank()
)

g3 = g3+theme(
  legend.position = c(0.7, 0.1)
)

g4 = g4+theme(
  legend.position = c(0.7, 0.1)
)

# for (i in seq_along(country_names)){
#   gs[[i]] = profile_plot(gen_country(data, country_names[i]))
# }
# 
# 
# 
# for (i in 1:4){
#   gs[[i]] = gs[[i]]+
#     labs(title = paste(country_names[i]))+
#     theme(plot.title = element_text(size = 10, hjust = 0) )
# }
# 
# for (i in 4:6){
#   gs[[i]] = gs[[i]]+
#     labs(title = paste(country_names_6[i]))+
#     theme(axis.text.x = element_text(angle = 30, hjust = 0.5, size = 8))+
#     theme(axis.text.x = element_text(margin=margin(-5,0,0,0)))+
#     theme(axis.ticks.x = element_blank())
# }


p = plot_grid(g1, g2,g3,g4, ncol=2)
title <- ggdraw() + draw_label("Profile plots of 4 countries", fontface='bold',x = 0.02, hjust = 0)
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1.6)) # rel_heights values control title margins

ggsave('test_plot.png', width = 12, height = 7)
