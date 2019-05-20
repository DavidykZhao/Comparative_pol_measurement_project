library(shiny)
library(tidyverse)
#install.packages("DT")
library(DT)
#install.packages("poLCA")
library("poLCA")
library("reshape2")
library(plotly)

dta = read.csv("nonzero_dataset.csv")
plot_n = function(dta, num_cluster ){
  f = with(dta, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 
  lc <- poLCA(f, dta, nclass= num_cluster, maxiter=3000, 
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
    geom_line(size = 1.00) +
    labs(x = NULL, y = "Mean value of the response") +
    theme_bw(base_size = 14)+
    ggtitle(paste(paste("class", 1:length(lc$P), sep = "_"),  
                  round(lc$P, 3), collapse = ", "))+
    theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))+
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  plotly_p = ggplotly(p, tooltip = "all") %>%
    layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                         x = 0.5, y = -0.8))
  return(plotly_p)
}


best_plot = function(dta){
  f = with(dta, cbind(tax, religion, free_election, state_aid, civil_rights, women)~1) 
  min_bic <- 100000
  for(i in 1:7){
    lc <- poLCA(f, dta, nclass=i, maxiter=3000, 
                tol=1e-5, na.rm=FALSE,  
                nrep=10, verbose=TRUE, calc.se=TRUE)
    if(lc$bic < min_bic){
      min_bic <- lc$bic
      LCA_best_model<-lc
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
    theme_bw(base_size = 14)+
    ggtitle(paste(paste("class", 1:length(LCA_best_model$P), sep = "_"),  
                  round(LCA_best_model$P, 3), collapse = ", "))+
    theme(plot.margin=unit(c(1.5,1.5,1.5,1.2),"cm"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))+
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  
  
  plotly_p = ggplotly(p, tooltip = "all") %>%
    layout(legend = list(orientation = "h", y = -0.8, x = 0.5))
  return(plotly_p)
}


############### The start of UI
#*****************************************************************************************
#********************************************************************************************
  
ui <- fluidPage(
  
  titlePanel(
    fluidRow(
      column(9, p("Visualization of LCA of 'Democracy' variables", style = "padding:20px; font-size:200%")),
      column(3, img(height = 120, width = 180, src = "tamu.png"))
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
        tabPanel("Profile plot", plotlyOutput("NUM_N_PLOT"),
                 p(strong("*Double click the legend to view isolated plot")),
                 p(("The plotting may take 10-20 secs, please wait & do not change tabs to avoid 
          unneccesary computation lags :)" ), style= 'font-size:150%')),
        tabPanel("3-D plot", plotOutput("THREE_D_PLOT"), height = "auto",
                 p(("The plotting may take 10-20 secs, please wait & do not change tabs to avoid 
          unneccesary computation lags :)" ), style= 'font-size:150%')),
        tabPanel(paste("Profile plot", "of the best model -- DO NOT RANDOMLY CLICK, OTHERWISE NEED 30s to RUN!", sep = "\n"), 
                 plotlyOutput("BEST_PLOT"), height = "auto",
        p("The plotting may take 30-50 secs, please wait & do not change tabs within this app to avoid 
          unneccesary computation lags :)" , style= 'font-size:150%'),
        p("The #cluster slider is ineffective in this computation"))
  
#closure for the tabset panel
    )
    
  )
  
)
)


server <- function(input, output){
  DATA = reactive({dta %>%
    filter(country == input$COUNTRY) %>%
    dplyr::select(c("tax", "religion", "free_election", "state_aid",
                    "civil_rights", "women"))})
  f = reactive({with(DATA(), cbind(tax, religion, free_election, state_aid, civil_rights, women)~1)}) 
  
  
  output$TABLE = DT::renderDataTable({
     DATA()
  })
  
    output$NUM_N_PLOT = renderPlotly({
      plot_n(DATA(), input$NUM)})
    
    output$THREE_D_PLOT = renderPlot({
      p = poLCA(f(), DATA(), nclass= input$NUM, maxiter=3000, 
                tol=1e-5, na.rm=FALSE,  
                nrep=10, verbose=TRUE, calc.se=FALSE, graphs = TRUE)
      print(p)
    }, height = function(){200+100*input$NUM})
  
    output$BEST_PLOT = renderPlotly({
      best_plot(DATA())
    })
    
}


shinyApp(ui = ui, server = server)