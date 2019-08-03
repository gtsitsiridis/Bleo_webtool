## My Version - Tidy
## Read in all Global Variable prior and set filenames
library(rhdf5)
library(ggplot2)
library(plyr)
library(Seurat)
library(readxl)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(data.table)
library(tidyr)
library(dplyr)
library(gridExtra)
library(grid)

load("ADI_AT1_trajectory.RData")
load("Convergence_trajectory.RData")
css_file = "www/style.css"
source("functions.R")

## Whole Lung Files
filename = "WholeLung_data.h5" 
metafile <- read.delim("WholeLung_metadata.txt", sep = "\t", header = T, stringsAsFactors = F)
genes <- read.delim("WholeLung_genes.txt", stringsAsFactors = F)[, 1]
markers_table <- data.frame(read_excel("S1_Bleo_AllMarkers_annotated.xlsx"))

## High Resolution Epithel Files
epi_filename = "Epi_highres_data.h5"
epi_metafile <- read.delim("HighresEpi_meta.txt", sep = "\t", header = T, stringsAsFactors = F)
epi_genes <- read.delim("HighresEpi_genes.txt", stringsAsFactors = F)[, 1]
epi_markers_table <- data.frame(read_excel("AllMarkers_Epi_inOne.xlsx",
                                           col_types = c("text", "numeric", "numeric", "numeric", "text", "text")))
spline_expr <- read.delim("Table_S2_adjpval_025.txt", stringsAsFactors = F)

## Table from Spline fits for Cell-Cell Communication
rec_lig <- read.delim(file = "RecLig_merged_withSpline_adapted.txt", sep = "\t", stringsAsFactors = F)

checkFile <- function(f) {
  if(!file.exists(f)){
    stop(paste0("could not find ", f, "!"))
  }
  print(paste("File :", f))
}