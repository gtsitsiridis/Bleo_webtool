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
metadata.file <- "data/UMAP_metadata.txt"
expression.file <- "data/Bleo_scaledData.h5"

### Load files
# load(example)
meta <- read.delim(metadata.file)
cell_types <- unique(na.omit(meta$cell.type_spline) %>% as.character())
res.2 <- unique(na.omit(meta$res.2))
genes <- rhdf5::h5ls(expression.file)[, "name"]

print(paste("Expression file:", expression.file))
print(paste("Metadata file:", metadata.file))
