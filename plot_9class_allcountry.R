f = with(nonnegtive[, 2:ncol(nonnegtive)], cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 
lc <- poLCA(f, nonnegtive[, 2:ncol(nonnegtive)], nclass=9, maxiter=3000, 
            tol=1e-5, na.rm=FALSE,  
            nrep=10, verbose=TRUE, calc.se=TRUE)

probs = lc$probs
n_class = length(lc$P)

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
  scale_shape_manual(values=c(1:9))+
  
  geom_line(size = 1.00) +
  labs(x = NULL, y = "Mean value of the response") +
  theme_minimal()+
  labs(caption = paste(paste("Class", 1:length(lc$P), sep = " "),  
                             round(lc$P, 3), sep = ": ", collapse = ", "))+
  theme(plot.caption = element_text(size = 8, hjust = 0))+
  # labs(caption = paste(paste("class", 1:length(lc$P), sep = " "),  
  #                      round(lc$P, 3), sep = ": ", collapse = ", "))+
  #ggtitle()+
  theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm"))+
  theme(axis.text.x = element_text(hjust = 0.1, size = 10))+
  theme(axis.title.y = element_text(size = 15))+
  labs(title = "Latent Profile plot using the pooled sample across all countries ")+
  theme(plot.title = element_text(hjust = 0, size = 16))+
  theme(aspect.ratio = 0.5) +
  theme(legend.position = "bottom")
print(p)



png(filename="profile_plot_pooled.png", width = 1000, height = 500)
p
dev.off()

ggsave("profile_plot_pooled.png", width = 9)
