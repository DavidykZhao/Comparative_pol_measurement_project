#devtools::install_github("hadley/lineprof")
library(lineprof)
source("Gen_wave56_collate_plot.R")
l <- lineprof(f())
l
