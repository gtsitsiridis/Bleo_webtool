## My Version - Tidy
## Read in all Global Variable prior and set filenames
library(rhdf5)
library(ggplot2)
library(plyr)
#library(Seurat)
library(cowplot)
# library(readxl)
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

adi_at1_path <- "data/adi_at1.RDS"
checkFile(adi_at1_path)
adi_at1 <- readRDS(adi_at1_path)

adi_at1_annot_path <- "data/adi_at1_annot.RDS"
checkFile(adi_at1_annot_path)
adi_at1_annot <- readRDS(adi_at1_annot_path)

convergence_path <- "data/adi_at1.RDS"
checkFile(adi_at1_path)
adi_at1 <- readRDS(adi_at1_path)

convergence_path <- "data/convergence.RDS"
checkFile(convergence_path)
convergence <- readRDS(convergence_path)

convergence_annot_path <- "data/convergence_annot.RDS"
checkFile(convergence_annot_path)
convergence_annot <- readRDS(convergence_annot_path)

convergence_path = "data/v01/Convergence_trajectory.RData"
checkFile(convergence_path)
load(convergence_path)

## Whole Lung Files
filename = "data/WholeLung_data.h5" 
checkFile(filename)
metafile_path = "data/wholelung_metafile.RDS"
checkFile(metafile_path)
metafile <- readRDS(metafile_path)
genes_path <- "data/wholelung_genes.RDS"
checkFile(genes_path)
genes <- readRDS(genes_path)
markers_table_path <- "data/wholelung_markers_table.RDS"
checkFile(markers_table_path)
markers_table <- readRDS(markers_table_path)

## High Resolution Epithel Files
epi_filename = "data/Epi_highres_data.h5"
checkFile(epi_filename)
epi_metafile_path <- "data/epi_metafile.RDS"
checkFile(epi_metafile_path)
epi_metafile <- readRDS(epi_metafile_path)
epi_genes_path <- "data/epi_genes.RDS"
checkFile(epi_genes_path)
epi_genes <- readRDS(epi_genes_path)
epi_markers_table_path <- "data/epi_markers_table.RDS"
checkFile(epi_markers_table_path)
epi_markers_table <- readRDS(epi_markers_table_path)
spline_expr_path <- "data/spline_expr.RDS"
checkFile(spline_expr_path)
spline_expr <- readRDS(spline_expr_path)

## Table from Spline fits for Cell-Cell Communication
rec_lig_path <- "data/rec_lig.RDS"
checkFile(rec_lig_path)
rec_lig <- readRDS(rec_lig_path)

## Whole Lung Spline Table
wholeLung_spline_path <- "data/wholelung_spline.RDS"
checkFile(wholeLung_spline_path)
wholeLung_spline <- readRDS(wholeLung_spline_path)
