## My Version - Tidy
## Define all Functions used in WebTool later on
## for testing, go to ../my_version/outsource_functions.R

spinner <- function(ui_element) {
  withSpinner(ui_element, type = 3, color.background = "white")
}


emptyPlot <- function(type="general") {
  txt <- "not enough data"
  if(type == "cc"){
    txt <- "please click on row in table to select receptor-ligand pair" 
  }
  df <- data.frame(x = 5, y = 5, text = txt)
  p <- ggplot(df, aes(x, y, label = text)) +
    geom_point(col = "white") + xlim(0, 10) + ylim(0, 10) + geom_text(fontface="bold") + theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    ) +
    theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
  class(p)[4] <- "empty_plot"
  p
}

## Define which cell types will be displayed in query list (give type for epi version)
select_cell_type <- function(meta, column = "cell_type", type = F){
  if(type == F){
    ct <- unique(meta[, column])
    return(ct[which(!is.na(ct))])
  }
  else{
    ct <- unique(meta[meta$type == type, "cell_type"])
    return(ct[order(ct)])
  }
}

genUMAPplot <- function(h5, meta, gene_name = 'Sftpc') {
  gene <- h5read(h5, gene_name)
  gene <- (gene - min(gene))/(max(gene) - min(gene))
  H5close()
  dt <- cbind(meta[, c("UMAP1", "UMAP2")], expression = gene)
  sc <- scale_colour_gradientn(colours = magma(20, alpha = 0.5, direction = -1), limits= range(gene))
  ggplot(dt) + geom_point(aes(UMAP1, UMAP2, col = gene), size = 0.3, alpha = .5) +
    guides(col = F) +
    ggtitle(gene_name) + sc
}

## DotPlot (taken and adapted from Aging)
dotPlot <- function (h5, meta, gene_name = "Scgb1a1") {
  # Defaults
  cols.use = c("lightgrey", "blue")
  plot.legend = FALSE
  do.return = FALSE
  x.lab.rot = FALSE
  
  scale.func <- switch(EXPR = "radius", 'size' = scale_size, 'radius' = scale_radius,
                       stop("'scale.by' must be either 'size' or 'radius'"))
  
  MinMax <- function (data, min, max) {
    data2 <- data
    data2[data2 > max] <- max
    data2[data2 < min] <- min
    return(data2)
  }
  
  PercentAbove <- function (x, threshold) {
    return(length(x = x[x > threshold]) / length(x = x))
  }
  
  # Load gene expression
  genExp <- h5read(h5, gene_name)
  data.to.plot <- data.table(genExp)
  colnames(x = data.to.plot) <- "expression"
  data.to.plot$id <- meta$cell_type

  # filtering step: is there a cluster that has at least 10 cells. (5 no?)
  if(data.to.plot[, sum(expression > 0), by = id][, sum(V1 >= 5) == 0])
    return(emptyPlot())
  
  data.to.plot[, expression := (expression - mean(expression)) / sd(expression)]
  setnames(data.to.plot, "expression", gene_name)
  data.to.plot <-
    data.to.plot %>% gather(key = genes.plot, value = expression, -c(id))
  data.to.plot <- data.to.plot %>% group_by(id, genes.plot) %>%
    dplyr::summarize(avg.exp = mean(expm1(x = expression)),
                     pct.exp = PercentAbove(x = expression,  threshold = 0))
  data.to.plot <-
    data.to.plot %>% ungroup() %>% group_by(genes.plot) %>%
    mutate(avg.exp.scale = scale(x = avg.exp)) %>% mutate(avg.exp.scale = MinMax(
      data = avg.exp.scale,
      max = 2.5,
      min = -2.5
    ))
  data.to.plot$pct.exp[data.to.plot$pct.exp < 0] <- NA
  data.to.plot <- as.data.frame(data.to.plot)
  colnames(data.to.plot) <- c("CellType", "Gene", "AvgExpr", "PctExpressed", "AvgRelExpr")
  bad <- which(data.to.plot$CellType == "unassigned")
  if(length(bad) > 0) data.to.plot <- data.to.plot[-bad,]
  ## Exclude NA cell type (there are none in highres Epi)
  if(sum(is.na(data.to.plot$CellType)) > 0){
    data.to.plot <- data.to.plot[-which(is.na(data.to.plot$CellType)),]
  }
  data.to.plot$CellType <- factor(data.to.plot$CellType, levels = rev(sort(as.character(data.to.plot$CellType))))

  p <-
    ggplot(data = data.to.plot, mapping = aes(x = Gene, y = CellType)) +
    geom_point(mapping = aes(size = PctExpressed, color = AvgRelExpr)) +
    scale.func(range = c(0, 10), limits = c(NA, NA)) +
    theme(
      axis.text.y = element_text(size = 13),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.position = c(0.75, 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  p
}
## Plot mean expression on each measured day (sample needs to have min_cells 
## cells expressing the gene to be considered)
genLinePlot <- function(h5, gene, cell_type, meta, type = "meta_cell_type", min_cells = 5, smooth = F, epi = F){
  genExp <- data.frame(expression = h5read(h5, gene))
  genExp$cell_type = meta[, type]
  genExp$identifier = meta$identifier
  genExp$day = as.numeric(gsub("d", "", meta$grouping))
  genExp = genExp[genExp$cell_type == cell_type,]
  tmp = data.table(genExp)
  tmp = tmp[, sum(expression > 0), by = identifier]
  ids = tmp$identifier[tmp$V1 >= min_cells]
  
  data.to.plot <- genExp[genExp$identifier %in% ids,]
  data.to.plot <- aggregate(data.to.plot$expression, list(data.to.plot$identifier), mean)
  names(data.to.plot) <- c("identifier", "expression")
  map <- unique(meta[, c("identifier", "grouping")])
  
  ## Different Ordering on x-Axis if high resolution Epithelium is plotted (treat days as factors)
  if(epi == T){
    data.to.plot$day <- gsub("d", "", map[match(data.to.plot$identifier, map$identifier), "grouping"])
    order <- sort(as.numeric(unique(data.to.plot$day)))
    data.to.plot$day <- as.numeric(factor(data.to.plot$day, levels = order))
    #order <- c(0, seq(2, 13), 15, 21, 28, 36, 54)
    #data.to.plot$day <- factor(gsub("d", "", map[match(data.to.plot$identifier, map$identifier), "grouping"]), levels = order)
  }
  else{
    data.to.plot$day <- as.numeric(gsub("d", "", map[match(data.to.plot$identifier, map$identifier), "grouping"]))
  }
  
  if(smooth == T){
    p <- ggplot(data.to.plot, aes(y = expression, x = day)) + 
      geom_point(col = "palevioletred2") + ggtitle(gene) +
      geom_smooth(se = T, level = 0.8, col = "royalblue", method = "loess") +
      ylab(paste0("mean expression in ", cell_type)) + xlab("days")
  }
  else{
    ## with error bars
    agg <- ddply(data.to.plot, .(day), function(x) c(mean = mean(x$expression, na.rm = T), se = sd(x$expression, na.rm = T) / sqrt(length(x$expression))))
    agg$lower <- agg$mean + agg$se
    agg$upper <- agg$mean - agg$se
    p <- ggplot(agg, aes(y = mean, x = day)) +   ## add col = id to have dot colour per sample
      geom_errorbar(aes(ymin=lower, ymax=upper), width=.5, col = "gray") +
      geom_point(col = "royalblue") + ggtitle(gene) +
      stat_summary(fun.y = mean, geom = "smooth", lwd=1) +
      ylab(paste0("mean expression in ", cell_type))
    # if you specifically want labels on days we collected
    #scale_x_discrete(name ="days", limits = sort(unique(agg$day))) 
  }
  if(epi == T)
    p <- p + scale_x_discrete(limits = sort(unique(data.to.plot$day)), labels = order)
  p
}

## Markers Table with more sophisticated Version for highres Epi
getMarkersTable <- function(cell_type = "Alveolar macrophages", resolution = F) {
  ## If no resolution is given, assume epithel markers table
  if(resolution != F){
    dt = epi_markers_table[epi_markers_table$type == resolution,]
    dt <- dt[dt$cell_type == cell_type, ]
  }
  else{
    dt <- markers_table[markers_table$louvain_cluster== cell_type, ]
  }
  dt <- dt[, c("gene", "avg_logFC", "p_val","p_val_adj")]
  dt$avg_logFC <- round(dt$avg_logFC, 3)
  dt$p_val <- as.numeric(formatC(dt$p_val, digits = 3, format = "e"))
  dt$p_val_adj <- as.numeric(formatC(dt$p_val_adj, digits = 3, format = "e"))
  dt
}

## Lukas’ Versions - average Gene Kinetic Plots
## As I get lots of Errors while reading in that table, extracted genes with pval < 0.25
## In Bleo/short_scripts/check_nr_significant_genes.R
plotSingleGene <- function(celltype = 'fibroblasts', gene = "Tnc"){
  tmp <- wholeLung_spline[[celltype]]
  stats <- tmp[,1:2]
  prop <- as.numeric(tmp[gene, -c(1:2)])
  nom <- colnames(tmp)[-(1:2)]
  day <- unlist(lapply(nom, function(x) strsplit(x, '_', fixed = T)[[1]][3]))
  day[grep('PBS', nom)] <- 'd0'
  day <- gsub('d', '', day)
  day <- as.numeric(day)
  aframe <- data.frame(prop, day)    

  nom <- paste(celltype, "(Spline regression P", signif(stats[gene, 1],2), ")")
  
  p <- ggplot(aframe, aes(y = prop, x = day)) + 
    geom_point(color = "violet") + geom_smooth(method = "loess") +
    ylab(paste("Relative", gene, "expression")) + xlab("Days") + ggtitle(nom)
  p
}

## Only have significant genes as suggestions in query
select_spline_genes <- function(cell_type = "alv_epithelium"){
  genes <- spline_expr$gene[spline_expr$cell.type == cell_type]
  return(genes)
}

getRecLigTable <- function(cell_type_rec = "Macrophages", cell_type_lig = "Fibroblasts") {
  dt <- rec_lig[rec_lig$metacelltype.rec == cell_type_rec & rec_lig$metacelltype.lig == cell_type_lig, ]
  cols <- c(paste0(c("gene.", "metacelltype.", "avg_logFC.", "p_val.", "p_val_adj.", "spline_adj_pval_"), "rec"),
            paste0(c("gene.", "metacelltype.", "avg_logFC.", "p_val.", "p_val_adj.", "spline_adj_pval_"), "lig"))
  dt <- dt[, cols]
  for(col in c(paste0(c("avg_logFC.", "p_val.", "p_val_adj.", "spline_adj_pval_"), "rec"), 
               paste0(c("avg_logFC.", "p_val.", "p_val_adj.", "spline_adj_pval_"), "lig"))){
    dt[, col] <- suppressWarnings(as.numeric(formatC(dt[, col], digits = 3, format = "e")))
  }
  for(col in c("avg_logFC.rec", "avg_logFC.lig")){
    dt[, col] <- round(dt[, col], 3)
  }
  dt
}

## Tab 3 Average Expression of Ligand Receptor Pairs
return_scaled_expr <- function(gene_name, cell_type, mode, type = "meta_cell_type"){
  scale <- function(x) (x - min(x))/(max(x) - min(x))
  
  expr <- data.frame(h5read(filename, gene_name))
  expr$cell_type = gsub(" ", "_", metafile[, type])   ## different meta cell type sep
  expr$day = as.numeric(gsub("d", "", metafile$grouping))
  expr <- expr[expr$cell_type == cell_type, ]
  expr <- na.omit(expr)
  expr$expression <- scale(expr[, 1])
  expr <- data.frame(expression = expr$expression, type = paste0(mode, ": ", gene_name, "   "), day = expr$day)
  expr
}

plot_RecLig_expression <- function(rec, lig, rec_ct, lig_ct){
  rec_expr <- return_scaled_expr(rec, rec_ct, mode = "Receptor")
  lig_expr <- return_scaled_expr(lig, lig_ct, mode = "Ligand")
  dt <- rbind(rec_expr, lig_expr)
  ggplot(dt, aes(y = expression, x = day, group = type, color = type)) + 
    geom_smooth(se = T, level = 0.8, method = "loess") +
    ylab(paste("Scaled expression")) + xlab("days") + 
    theme(legend.position="top", legend.title = element_blank(), legend.justification = "center")
    #ggtitle(paste0(rec, " - ", lig))
}

## Tab 6 Convergence club and AT2 to ADI
convergence_traj_single_gene <- function(gene_name = 'Krt8'){
  gene <- h5read(epi_filename, gene_name)
  gene <- gene[match(rownames(convergence), epi_metafile$X)]
  
  aframe <- data.frame(convergence, gene)
  
  melted <- melt(data = aframe, id.vars = "t", measure.vars = "gene")
  
  ggplot(melted, aes(y = value, x = t, group = variable, color = variable)) + geom_smooth(method = "loess") +
    ylab(paste(gene_name, "expression")) + xlab("Pseudotime") + ggtitle("Convergence trajectory") + theme(legend.position="none")
}

convergence_feature_plot <- function(gene_name = "Krt8"){
  gene <- h5read(epi_filename, gene_name)
  gene <- gene[match(rownames(convergence), epi_metafile$X)]
  gene <- (gene - min(gene))/(max(gene) - min(gene))
  H5close()
  
  aframe <- data.frame(convergence, gene)
  
  sc <- scale_colour_gradientn(colours = magma(20, alpha = 0.5, direction = -1), limits= range(gene))
  
  dt <- aframe
  
  ggplot(dt) + geom_point(aes(DC1, DC2, col = gene), size = 0.5, alpha = .9) +
    guides(col = F) +
    ggtitle(gene_name) + sc#scale_color_continuous(low = "grey", high = high) 
}

## Tab 7 Trajectory ADI to AT1
adi_at1_traj_single_gene <- function(gene_name = 'Krt8'){
  gene <- h5read(epi_filename, gene_name)
  gene <- gene[match(rownames(adi_at1), epi_metafile$X)]
  aframe <- data.frame(adi_at1, gene)
  melted <- melt(data = aframe, id.vars = "t", measure.vars = "gene")
  
  ggplot(melted, aes(y = value, x = t, group = variable, color = variable)) + geom_smooth(method = "loess") +
    ylab(paste(gene_name, "expression")) + xlab("Pseudotime") + ggtitle("Krt8+ progenitors -> AT1 trajectory") + theme(legend.position="none")
}

adi_at1_feature_plot <- function(gene_name = "Krt8"){
  gene <- h5read(epi_filename, gene_name)
  gene <- gene[match(rownames(adi_at1), epi_metafile$X)]
  gene <- (gene - min(gene))/(max(gene) - min(gene))
  H5close()
  
  aframe <- data.frame(adi_at1, gene)

  sc <- scale_colour_gradientn(colours = magma(20, alpha = 0.5, direction = -1), limits= range(gene))
  
  dt <- aframe
  ggplot(dt) + geom_point(aes(umap1, umap2, col = gene), size = 2, alpha = .9) +
    guides(col = F) +
    ggtitle(gene_name) +  sc #+scale_color_continuous(low = "grey", high = high) 
}




