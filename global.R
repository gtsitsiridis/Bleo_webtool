library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggthemes)
library(rhdf5)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(tidyr)
library(shiny)
library(shinyjs)


source("functions.R")
### File names
expression.file <- "data/Bleo_scaledData.h5"
spline_results_list.file <- "data/spline_results_list.RData"

### Load files
# object name: spline_results_list
load(spline_results_list.file)
# We could also use an RDS object instead and do the following:
# spline_results_list <- readRDS("data/spline_results_list.RDS")
cell_types <- names(spline_results_list)
genes <- rhdf5::h5ls(expression.file)[, "name"]


print(paste("Expression file:", expression.file))
print(paste("Spline_results_list file:", spline_results_list.file))

