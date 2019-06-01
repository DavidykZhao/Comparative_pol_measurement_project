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


#########################

country_names_6 = unique(selected_nonneg$country)
gs = vector("list", length(country_names_6))

for (i in seq_along(country_names_6)){
  gs[[i]] = profile_plot(gen_country(country_names_6[i]))
}



profile_plot = function(data){
  f = with(data, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 
  min_bic <- 1000000
  for(i in 2:5){
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
    tax = replicate(n_class, NA),
    religion = replicate(n_class, NA),
    free_election = replicate(n_class, NA),
    state_aid = replicate(n_class, NA),
    civil_rights = replicate(n_class, NA),
    women = replicate(n_class, NA))
  
  for (i in 1:6) {
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
    geom_point(size = 2.25, aes(shape = class))+
    geom_line(size = 1.00) +
    labs(x = NULL, y = "Mean value of the response") +
    theme_minimal()+
    labs(subtitle = (paste(paste("Class", 1:length(LCA_best_model$P), sep = "_"),  
                  round(LCA_best_model$P, 2), collapse = ", ")))+
    theme(plot.subtitle = element_text(size = 8))+
   # theme(plot.margin=unit(c(1,1,1,1),"cm"))+
    #theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
    theme(axis.text.x = element_blank())+
    theme(plot.title = element_text(hjust = 0.5, size = 10))+
    theme(legend.position = "none")+
    theme(axis.title.y = element_blank())+
    theme(axis.ticks.x = element_blank())
   # theme(aspect.ratio = 1.5)
  print(p)
  return(p)
  
  # library(plotly)
  # 
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", y = 1.2))
  # print(plotly_p)
  # return(plotly_p)
}
#profile_plot(US)


country_names_6 = unique(selected_nonneg$country)
gs = vector("list", length(country_names_6))

for (i in seq_along(country_names_6)){
  gs[[i]] = profile_plot(gen_country(country_names_6[i]))
}



for (i in 1:6){
  gs[[i]] = gs[[i]]+
    labs(title = paste(country_names_6[i]))+
    theme(plot.title = element_text(size = 10, hjust = 0) )
}

for (i in 4:6){
  gs[[i]] = gs[[i]]+
    labs(title = paste(country_names_6[i]))+
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, size = 8))+
    theme(axis.text.x = element_text(margin=margin(-5,0,0,0)))+
    theme(axis.ticks.x = element_blank())
}

gs[[4]]
a = profile_plot(US)






#library(cowplot)
p = plot_grid(gs[[1]], gs[[2]],gs[[3]],
          gs[[4]],gs[[5]],gs[[6]], ncol=3)
title <- ggdraw() + draw_label("Profile plots of 6 countries", fontface='bold',x = 0.02, hjust = 0)
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1.6)) # rel_heights values control title margins



