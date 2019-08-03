## My Version - Tidy
## Read in all Global Variable prior and set filenames
library(rhdf5)
library(ggplot2)
library(plyr)
#library(Seurat)
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
library(viridis)

checkFile <- function(f) {
  if(!file.exists(f)){
    stop(paste0("could not find ", f, "!"))
  }
  print(paste("File :", f))
}

css_file = "www/style.css"
source("functions.R")

adi_at1_path <- "data/ADI_AT1_trajectory.RData"
checkFile(adi_at1_path)
load(adi_at1_path)
colnames(adi_at1_annot) <- c("Annotation", "Meaning")
convergence_path = "data/Convergence_trajectory.RData"
checkFile(convergence_path)
load(convergence_path)
colnames(convergence_annot) <- c("Annotation", "Meaning")

## Whole Lung Files
filename = "data/WholeLung_data.h5" 
checkFile(filename)
metafile_path = "data/WholeLung_metadata.txt"
checkFile(metafile_path)
metafile <- read.delim(metafile_path, sep = "\t", header = T, stringsAsFactors = F)
genes_path <- "data/WholeLung_genes.txt"
checkFile(genes_path)
genes <- read.delim(genes_path, stringsAsFactors = F)[, 1]
markers_table_path <- "data/S1_Bleo_AllMarkers_annotated.xlsx"
checkFile(markers_table_path)
markers_table <- data.frame(read_excel(markers_table_path))

## High Resolution Epithel Files
epi_filename = "data/Epi_highres_data.h5"
checkFile(epi_filename)
epi_metafile_path <- "data/HighresEpi_meta.txt"
checkFile(epi_metafile_path)
epi_metafile <- read.delim(epi_metafile_path, sep = "\t", header = T, stringsAsFactors = F)
epi_genes_path <- "data/HighresEpi_genes.txt"
checkFile(epi_genes_path)
epi_genes <- read.delim(epi_genes_path, stringsAsFactors = F)[, 1]
epi_markers_table_path <- "data/AllMarkers_Epi_inOne.xlsx"
checkFile(epi_markers_table_path)
epi_markers_table <- data.frame(read_excel(epi_markers_table_path,
                                           col_types = c("text", "numeric", "numeric", "numeric", "text", "text")))
spline_expr_path <- "data/Table_S2_adjpval_025.txt"
checkFile(spline_expr_path)
spline_expr <- read.delim(spline_expr_path, stringsAsFactors = F)

## Table from Spline fits for Cell-Cell Communication
rec_lig_path <- "data/RecLig_merged_withSpline_adapted.txt"
checkFile(rec_lig_path)
rec_lig <- read.delim(file = rec_lig_path, sep = "\t", stringsAsFactors = F)

## Whole Lung Spline Table
wholeLung_spline_path <- "data/WholeLung_spline.RData"
checkFile(wholeLung_spline_path)
load(wholeLung_spline_path)
