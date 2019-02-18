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
  tmp <- data.frame(metadata, expr)
  ggplot(tmp, aes(x = UMAP1, y = UMAP2, color=expr)) +
    geom_point(alpha = 0.8) +
    scale_colour_gradient(low = "grey", high = "darkblue") +
    ggtitle(gene)
}
