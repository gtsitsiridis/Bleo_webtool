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

# Spline kinetics plot
genLinePlot <- function(gene="Acta2", clust = "Fibroblasts", meta, col = "blue", type = "cell.type", smooth = F){
  genExp <- h5read("data/Bleo_scaledData.h5", gene)
  if(clust == "all"){
    print("Using full Data Set")
    cluster <- "all"
  }
  
  else if(type == "louvain"){
    genExp = genExp[which(meta$res.2 %in% clust)]
    meta <- meta[which(meta$res.2 %in% clust), ]
    cluster <- paste(clust, collapse = ", ")
  }
  else if(type == "cell.type"){
    genExp = genExp[which(meta$cell.type %in% clust)]
    meta <- meta[which(meta$cell.type %in% clust), ]
    cluster <- clust
  }
  identifier <- meta$identifier
  
  ## Try excluding those Samples, where there are less than 5 cells in current cell type
  tmp <- table(identifier)
  ids <- names(tmp[tmp >= 5])
  meta <- meta[which(meta$identifier %in% ids), ]
  genExp = genExp[which(meta$identifier %in% ids)]
  means <- unlist(lapply(split(genExp, meta$identifier), mean))
  
  timepoint <- unique(meta[,c("identifier", "grouping")])
  timepoint$grouping <- gsub("d", "", as.character(timepoint$grouping))
  timepoint$grouping <- gsub("PBS", "0", timepoint$grouping)
  data <- data.frame(expression = means, day = timepoint$grouping[match(names(means), timepoint$identifier)])
  data$day <- as.numeric(as.character(data$day))
  data <- na.omit(data)
  
  agg <- ddply(data, .(day), function(x) c(mean = mean(x$expression, na.rm = T), se = sd(x$expression, na.rm = T) / sqrt(length(x$expression))))
  agg$lower <- agg$mean + agg$se
  agg$upper <- agg$mean - agg$se
  data$id <- unlist(lapply(rownames(data), function(x){strsplit(x, "_")[[1]][1]}))
  
  if(smooth == T){
    p <- ggplot(data, aes(y = expression, x = day)) + 
      geom_point(col = "palevioletred2") + ggtitle(gene) +
      #stat_summary(fun.y = mean, geom = "smooth", lwd = 1,) +
      geom_smooth(se = F, col = col) +
      ylab(paste0("mean expression in ", cluster)) + xlab("days") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  else{
    p <- ggplot(agg, aes(y = mean, x = day)) +   ## add col = id to have dot colour per sample
      geom_errorbar(aes(ymin=lower, ymax=upper), width=.3, col = col) +
      geom_point(col = col) + ggtitle(title) +
      stat_summary(fun.y = mean, geom = "smooth", lwd=1) +
      ylab(paste0("mean expression in ", cluster)) + xlab("days") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  p
}
