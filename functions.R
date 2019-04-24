# UMAP plot
plot_UMAP_colored_by_expr <- function(gene, expression.file="data/Bleo_scaledData.h5"){
  expr <- h5read(file = expression.file, name = gene)  
  
  # Cut extreme quantiles off
  #quants <- quantile(expr, c(0.05, 0.95))
  #expr[which(expr <= quants[1])] <- quants[1]
  #expr[which(expr >= quants[2])] <- quants[2]
  
  # Scale to 0 1
  expr <- (expr - min(expr)) / (max(expr) - min(expr))
  
  # Generate UMAP plot
  tmp <- data.frame(meta, expr)
  ggplot(tmp, aes(x = UMAP1, y = UMAP2, color=expr)) +
    geom_point(alpha = 0.8) +
    scale_colour_gradient(low = "grey", high = "darkblue") +
    ggtitle(gene)
}

# Spline kinetics plot
genLinePlot <- function(celltype, gene){
  spline_results <- spline_results_list[[celltype]]
  prop <- data.matrix(spline_results[, -c(1:3)])
  day <- unlist(lapply(colnames(prop), function(x) strsplit(x, "_", fixed = T)[[1]][3]))
  day[grep("PBS", colnames(prop))] <- "0"
  day <- gsub("d", "", day)
  day <- as.numeric(day)
  
  aframe <- data.frame(expression = prop[gene, ], day)
  
  ggplot(aframe, aes(y = expression, x = day)) + 
    geom_point(col = "violet") + 
    geom_smooth(method = "loess") +
    ylab(paste("Percent of cells expressing", gene)) + xlab("Days") +
    ggtitle(gene)
}
