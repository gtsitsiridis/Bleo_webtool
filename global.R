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
library(gridExtra)
library(grid)


source("functions.R")

checkFile <- function(f) {
  if(!file.exists(f)){
    stop(paste0("could not find ", f, "!"))
  }
  print(paste("File :", f))
}

### File names
expression_files <- list()
expression_files$wholelung <- "data/current/expression/WholeLung_data.h5"
expression_files$hires <- "data/current/expression/Epi_highres_data.h5"
metadata_files <- list()
metadata_files$hires <- "data/current/metadata/HiRes_metadata_final.txt"
metadata_files$wholelung <- "data/current/metadata/WholeLung_metadata_final.txt"
markers_cell_types.dir <- "data/current/markersCellTypes"
genes.file <- "data/current/genes.txt"

metadata <- list()

### Load files
checkFile(expression_files$hires)
checkFile(expression_files$wholelung)
checkFile(metadata_files$hires)
metadata$hires <- fread(metadata_files$hires)
checkFile(metadata_files$wholelung)
metadata$wholelung <- fread(metadata_files$wholelung)

cell_types <- unique(union(metadata$hires$cell.type, metadata$wholelung$cell.type))
cell_types <- cell_types[which(!is.na(cell_types))]
cell_types <- cell_types[order(cell_types)]

markers_cell_types_files <- list()
markers_cell_types_files$hires <- list.files(file.path(markers_cell_types.dir, "hiRes"), full.names = TRUE)
markers_cell_types_files$wholelung <- list.files(file.path(markers_cell_types.dir, "wholeLung"), full.names = TRUE)
checkFile(genes.file)
genes <- scan(what = character(), genes.file, sep = "\n")
genes <- genes[order(genes)]
