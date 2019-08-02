# UMAP plot
plot_UMAP_colored_by_expr <-
  function(gene, expression.file = "data/Bleo_scaledData.h5") {
    expr <- h5read(file = expression.file, name = gene)
    
    # Cut extreme quantiles off
    #quants <- quantile(expr, c(0.05, 0.95))
    #expr[which(expr <= quants[1])] <- quants[1]
    #expr[which(expr >= quants[2])] <- quants[2]
    
    # Scale to 0 1
    expr <- (expr - min(expr)) / (max(expr) - min(expr))
    
    # Generate UMAP plot
    tmp <- data.frame(meta, expr)
    ggplot(tmp, aes(x = UMAP1, y = UMAP2, color = expr)) +
      geom_point(alpha = 0.8) +
      scale_colour_gradient(low = "grey", high = "darkblue") +
      ggtitle(gene)
  }

# Spline kinetics plot
genLinePlot <- function(celltype, gene) {
  spline_results <- spline_results_list[[celltype]]
  prop <- data.matrix(spline_results[,-c(1:3)])
  day <-
    unlist(lapply(colnames(prop), function(x)
      strsplit(x, "_", fixed = T)[[1]][3]))
  day[grep("PBS", colnames(prop))] <- "0"
  day <- gsub("d", "", day)
  day <- as.numeric(day)
  
  aframe <- data.frame(expression = prop[gene,], day)
  
  ggplot(aframe, aes(y = expression, x = day)) +
    geom_point(col = "violet") +
    geom_smooth(method = "loess") +
    ylab(paste("Percent of cells expressing", gene)) + xlab("Days") +
    ggtitle(gene)
}


emptyPlot <- function() {
  df <- data.frame(x = 5, y = 5, text = "not enough data")
  p <- ggplot(df, aes(x, y, label = text)) +
    geom_point(col = "white") + xlim(0, 10) + ylim(0, 10) + geom_text() + theme_bw() +
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

# Dot plot
dotPlot <- function (gene_name = "Scgb1a1", dataset_name="hires") {
  # Defaults
  cols.use = c("lightgrey", "blue")
  plot.legend = FALSE
  do.return = FALSE
  x.lab.rot = FALSE
  
  scale.func <- switch(EXPR = "radius",
                       'size' = scale_size,
                       'radius' = scale_radius,
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
  expression <-
    h5read(expression_files[[dataset_name]], name = as.character(gene_name))
  H5close() 
  data.to.plot <- data.table(expression)
  colnames(x = data.to.plot) <- "expression"
  data.to.plot$id <- metadata[[dataset_name]]$cell.type
  # filtering step: is there a cluster that has at least 10 cells.
  if(data.to.plot[,sum(expression>0),by=id][,sum(V1 >= 5) == 0])
    return(emptyPlot())
  data.to.plot[, expression := (expression - mean(expression)) / sd(expression)]
  setnames(data.to.plot, "expression", gene_name)
  data.to.plot <-
    data.to.plot %>% gather(key = genes.plot, value = expression,-c(id))
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
  colnames(data.to.plot) <-
    c("Cell_type", "Gene", "AvgExpr", "PctExpressed", "AvgRelExpr")
  
  # bad <-
  #   c("red_blood_cells",
  #     "Gamma-Delta_T_cells",
  #     "low_quality_cells")
  # data.to.plot <-
  #   data.to.plot[-match(bad, as.character(data.to.plot$Cell_type)),]
   # data.to.plot <-data.to.plot[-which(is.na(data.to.plot$Cell_type)),]
    
  celltype_order <- rev(cell_types)
  
  data.to.plot$Cell_type <-
    factor(data.to.plot$Cell_type, levels = celltype_order)
  
  p <-
    ggplot(data = data.to.plot, mapping = aes(x = Gene, y = Cell_type)) +
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

genUMAPplot <- function(gene_name = 'Frem1', dataset_name="hires") {
  gene <- h5read(expression_files[[dataset_name]], name = as.character(gene_name))
  umap_coord <- metadata[[dataset_name]][,.(UMAP1, UMAP2)]  
  # gene.min <- quantile(gene, 0.01)
  # gene.max <- quantile(gene, 0.99)
  # gene[which(gene > gene.max)] <- gene.max
  # gene[which(gene < gene.min)] <- gene.min
  H5close()
  dt <- cbind(umap_coord, expression = gene)
  high <- "darkblue"
  if (all(gene == 0)) {
    high = "grey"
    
  }
  ggplot(dt) + geom_point(aes(UMAP1, UMAP2, col = gene), alpha = .5) +
    guides(col = F) +
    ggtitle(gene_name) + scale_color_continuous(low = "grey", high = high)
  
}

getMarkersTable <- function(cell_type = "Plasma cells",  dataset_name="hires", file) {
  c_name <- "cluster"
  if(dataset_name=="wholelung"){
    c_name <- "cell.type"
  }
  markers_table <-  fread(file)
  colnames(markers_table)[colnames(markers_table)==c_name] <- "c_name"
  dt <-markers_table[c_name == cell_type,-c(which(colnames(markers_table) == "c_name")), with = FALSE]
  dt
}

plotSingleGene <- function(celltype = 'fibroblasts', gene = "Tnc"){
  res <- suppressWarnings(read_excel(tab.file, sheet = celltype))
  tmp <- res
  stats <- tmp[,1:3]
  if(!gene %in% stats$gene){
    stop("Gene not found")
  }
  prop <- as.numeric(tmp[which(stats$gene == gene), -c(1:3)])
  nom <- colnames(tmp)[-(1:3)]
  day <- unlist(lapply(nom, function(x) strsplit(x, '_', fixed = T)[[1]][3]))
  day[grep('PBS', nom)] <- 'd0'
  day <- gsub('d', '', day)
  day <- as.numeric(day)

  aframe <- data.frame(prop, day)
  p <- ggplot(aframe, aes(y = prop, x = day)) +
    geom_point(color = "violet") + geom_smooth(method = "loess") +
    ylab(paste(gene, "expression")) + xlab("Days") + ggtitle(celltype)
  p
}