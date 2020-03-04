# This is the shiny script for after_dissertation version App
library(shiny)
library(tidyverse)
#install.packages("DT")
library(DT)
#install.packages("poLCA")
library("poLCA")
library("reshape2")
library(plotly)

dta = read.csv("nonzero_ds.csv")
plot_n = function(dta, num_cluster ){
  country_name = dta[[2]]
  dta = dta[[1]]
  f = with(dta, cbind(tax, religion, free_election, state_aid, Army, civil_rights, women)~1) 
  lc <- poLCA(f, dta, nclass= num_cluster, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  probs = lc$probs
  n_class = length(lc$P)
  
  profile_tb = data.frame(
    tax = rep(NA, n_class),
    religion = rep(NA, n_class),
    free_election = rep(NA, n_class),
    state_aid = rep(NA, n_class),
    civil_rights = rep(NA, n_class),
    Army = rep(NA, n_class),
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
  
  p = ggplot(profile_long, aes(x = variable, y = value, group = class, color = class))+
    geom_point(size = 2.25)+
    geom_line(size = 1.00) +
    labs(x = NULL, y = "Mean value of the response") +
    theme_bw(base_size = 12)+
    ggtitle(paste(paste("Class", 1:length(lc$P), sep = "_"), ":", 
                  round(lc$P, 3), collapse = ", "))+
    theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm"))+
    theme(axis.text.x = element_text(size = 9))+
    theme(plot.title = element_text(hjust = 0.5, size = 14))+
    scale_color_discrete(name = "Class & probability", labels = paste(paste("Class", 1:length(lc$P), sep = "_"), ":", 
                                                                      round(lc$P, 3)))+
    labs(title = paste("The profile plot of", country_name))+
    theme(legend.title = element_text(size = 12))+
    theme(
      plot.title = element_text(hjust = 0)
    )+
    theme(legend.position = "bottom")
  
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
  #                        x = 0.5, y = -0.8))
  return(p)
}




best_plot = function(data_ls){
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
    labs(title = paste("The profile plot of", country_name))+
    theme(legend.title = element_text(size = 12))+
    theme(
      plot.title = element_text(hjust = 0)
    )+
    theme(legend.position = "bottom")
  
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", y = -0.8, x = 0.5))
  return(p)
  

  
  # library(plotly)
  # 
  # plotly_p = ggplotly(p, tooltip = "all") %>%
  #   layout(legend = list(orientation = "h", y = 1.2))
  # print(plotly_p)
  # return(plotly_p)
}





############### The start of UI
#*****************************************************************************************
#********************************************************************************************

ui <- fluidPage(
  
  titlePanel(
    fluidRow(
      column(9, p("Visualization of 'Democracy' variables - Army var included" , style = "padding:20px; font-size:100%")),
      column(3, img(height = 120, width = 150, src = "tamu.png"))
    )
  ),
  
  theme = "yeti.css",
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "COUNTRY",
                  label = "Country to visualize",
                  choices = unique(dta$country)),
      sliderInput(inputId = "NUM",
                  label = "# of clusters/groups/profiles",
                  min = 1,
                  max = 7,
                  value = 3
      )
      #actionButton("GENERATE", label= "Generate plot", style='padding:4px; font-size:200%', icon = icon("pencil-ruler", "fa-1.5x"))
      
      #closure for the sidebar panel
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data table", dataTableOutput(outputId = "TABLE")),
        tabPanel("Profile plot", plotOutput("NUM_N_PLOT"),
                 p(strong("*Double click the legend to view isolated plot")),
                 p(("The plotting may take 10-20 secs, please wait & do not change tabs to avoid 
          unneccesary computation lags :)" ), style= 'font-size:100%')),
        tabPanel("3-D plot", plotOutput("THREE_D_PLOT"), height = "auto"),
        tabPanel(paste("Profile plot", "of the best model -- DO NOT RANDOMLY CLICK, OTHERWISE NEED 30s to RUN!", sep = "\n"), 
                 plotOutput("BEST_PLOT"), height = "auto",
                 p("The plotting may take 30-50 secs, please wait & do not change tabs within this app to avoid 
          unneccesary computation lags :)" , style= 'font-size:100%'),
                 p("The #cluster slider is ineffective in this computation"))
        
        #closure for the tabset panel
      )
      
    )
    
  )
)


server <- function(input, output){
  DATA = reactive(list({dta %>%
      filter(country == input$COUNTRY) %>%
      dplyr::select(c("tax", "religion", "free_election", "state_aid", "Army",
                      "civil_rights", "women"))}, input$COUNTRY))
  f = reactive({with(DATA()[[1]], cbind(tax, religion, free_election, state_aid, Army, civil_rights, women)~1)}) 
  
  
  output$TABLE = DT::renderDataTable({
    DATA()[[1]]
  })
  
  output$NUM_N_PLOT = renderPlot({
    plot_n(DATA(), input$NUM)})
  
  output$THREE_D_PLOT = renderPlot({
    p = poLCA(f(), DATA()[[1]], nclass= input$NUM, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=FALSE, graphs = TRUE)
    print(p)
  }, height = function(){300+100*input$NUM})
  
  output$BEST_PLOT = renderPlot({
    best_plot(DATA())
  })
  
}


shinyApp(ui = ui, server = server)