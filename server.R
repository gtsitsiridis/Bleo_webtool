## My Version - Tidy
## Shiny Webtool - Server

#server <-function(input, output, session){
shinyServer(function(input, output, session){
  check_save <- function(plot) {
    # Check if exists
    if (class(plot)[1] == "try-error") {
      plot <- emptyPlot()
    } else{
      cl <- class(plot)[3]
      plots[[cl]] <- plot
    }
    plot
  }
  
  values <- reactiveValues(gene = NULL, cell_type = NULL, meta_cell_type = NULL, res = NULL,
                           smooth = NULL, spline_gene = NULL, spline_cell_type = NULL,
                           ccn = NULL, ccn_rec = NULL, ccn_lig = NULL,
                           epi_gene = NULL, epi_cell_type = NULL, epi_res = NULL,
                           tab1_markers_table = NULL, tab4_markers_table = NULL)
  
  plots <- reactiveValues(
    wholelung_umap = NULL,
    wholelung_dotplot = NULL,
    wholelung_kinplot = NULL,
    wholelung_george_kinplot = NULL,
    ccn_spline_plot = NULL,
    epi_umap = NULL,
    epi_dotplot = NULL,
    epi_kinplot = NULL,
    conv_diffmap = NULL,
    conv_traj = NULL,
    at1adi_diffmap = NULL,
    at1adi_traj = NULL
  )

  observeEvent(input$gene, {
    new_gene_name <- input$gene
    values$gene <- new_gene_name
  })
  observeEvent(input$cell_type, {
    new_cell_type <- input$cell_type
    values$cell_type <- new_cell_type
  })
  observeEvent(input$meta_cell_type, {
    new_meta_cell_type <- input$meta_cell_type
    values$meta_cell_type <- new_meta_cell_type
  })
  observeEvent(input$res, {
    new_res <- input$res
    values$res <- new_res
  })
  
  observeEvent(input$spline_cell_type, {
    new_spline_cell_type <- input$spline_cell_type
    values$spline_cell_type <- new_spline_cell_type
  })
  observeEvent(input$spline_gene, {
    new_spline_gene <- input$spline_gene
    values$spline_gene <- new_spline_gene
  })
  
  observeEvent(input$smooth, {
    new_smooth<- input$smooth
    values$smooth <- new_smooth
  })
  # observeEvent(input$min_cells, {
  #   new_min_cells<- input$min_cells
  #   values$min_cells <- new_min_cells
  # })
  # 
  observeEvent(input$ccn_rec_ct, {
    new_rec <- input$ccn_rec_ct
    values$ccn_rec_ct <- new_rec
  })
  observeEvent(input$ccn_lig_ct, {
    new_lig <- input$ccn_lig_ct
    values$ccn_lig_ct <- new_lig
  })
  
  observeEvent(input$epi_cell_type, {
    new_epi_cell_type <- input$epi_cell_type
    values$epi_cell_type <- new_epi_cell_type
  })
  
  observeEvent(input$epi_gene, {
    new_epi_gene_name <- input$epi_gene
    values$epi_gene <- new_epi_gene_name
  })

  observeEvent(input$epi_res, {
    new_res <- input$epi_res
    values$epi_res <- new_res
  })
  
  ### Define gene and cell type selectors
  output$cell_type_selector <- renderUI({
    selectInput("cell_type", "Query cell type:", select_cell_type(metafile, column = "cell_type"), selected = "Macrophages")
  })
  output$gene_selector <- renderUI({
    selectInput("gene", "Query gene:", genes, selected = "Arg1")
  })
  output$smooth_selector <- renderUI({
    checkboxInput("smooth", "smooth Plot", value = T)
  })
  output$min_cell_selector <- renderUI({
    sliderInput("min_cells", label = "Minimum cell number expressed per sample", 
                min = 5, max = 20, value = 5)
  })
  output$meta_cell_type_selector <- renderUI({
    res = values$res
    selectInput("meta_cell_type", "Query cell type:", select_cell_type(metafile, column = res), selected = "Macrophages")
  })
  output$res_selector <- renderUI({
    radioButtons("res", "Resolution",  
                 choices = c("cell_type", "meta_cell_type"), selected = "meta_cell_type")
  })
  output$ccn_rec_selector <- renderUI({
    selectInput("ccn_rec_ct", "Query receptor:", select_cell_type(rec_lig, column = "metacelltype.rec"), selected = "Macrophages")
  })
  output$ccn_lig_selector <- renderUI({
    selectInput("ccn_lig_ct", "Query ligand:", select_cell_type(rec_lig, column = "metacelltype.lig"), selected = "Fibroblasts")
  })
  
  output$spline_cell_type_selector <- renderUI({
<<<<<<< Updated upstream
    selectInput("spline_cell_type", "Query cell type:", select_cell_type(spline_expr, column = "cell.type"), selected = "alv_epithelium")
  })
  output$spline_gene_selector <- renderUI({
    cell_type = values$spline_cell_type
    selectInput("spline_gene", "Query gene:", select_spline_genes(cell_type), selected = "Krt8")
=======
    selectInput("spline_cell_type", "Query cell type:", names(wholeLung_spline), selected = "alv_epithelium")
  })
  output$spline_gene_selector <- renderUI({
    cell_type = values$spline_cell_type
    if(is.null(cell_type)) selectInput("spline_gene", "Query gene:", "Krt8", selected = "Krt8")
    else selectInput("spline_gene", "Query gene:", rownames(wholeLung_spline[[cell_type]]), selected = "Krt8")
>>>>>>> Stashed changes
  })
  
  output$epi_gene_selector <- renderUI({
    selectInput("epi_gene", "Query gene:", epi_genes, selected = "Sftpc")
  })
  output$epi_res_selector <- renderUI({
    radioButtons("epi_res", "Resolution",  
                 choices = c("res_2", "cell_type_2", "cell_type_4"), selected = "cell_type_2")
  })
  output$epi_cell_type_selector <- renderUI({
    if(is.null(values$epi_res))
      return()
    #res = "cell_type_2"
    res = values$epi_res
    selectInput("epi_cell_type", "Query cell type:", select_cell_type(epi_markers_table, type = res), selected = "AT2 cells")
  })
  
  
  ## Code for Figures
  output$tab1_celltype_panel <- renderPlot({
    gene_name <- values$gene
    if (is.null(gene_name)) {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(genUMAPplot(h5 = filename, meta = metafile, gene_name = gene_name), silent = T)
      class(p)[3] <- "wholelung_umap"
      umap <-
        check_save(p) + theme(plot.margin = unit(c(2, 0, 2, 0), "lines"))
        #p + theme(plot.margin = unit(c(1, 2, 1, 2), "lines"))
      incProgress(0.4, detail = "Umap")
      
      p <- try(dotPlot(h5 = filename, meta = metafile, gene_name = gene_name), silent = T)
      class(p)[3] <- "wholelung_dotplot"
      dotplot <-
        check_save(p) + theme(plot.margin = unit(c(2, 2, 2, 1), "lines"))
        #p + theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
      incProgress(0.4, detail = "Dotplot")
      
      p <- grid.arrange(umap, dotplot, widths = c(1, 0.8), padding = unit(0, "line"))
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  ## Markers Table
  output$tab1_markers_table <- DT::renderDataTable({
    cell_type <- values$cell_type
    if (is.null(cell_type)) {
      return()
    }
    dt <- getMarkersTable(cell_type = cell_type, resolution = F)
    DT::datatable(dt, extensions = 'Buttons', options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px",
                                                             searchHighlight = T, dom = '<"top"Bf>rt<"bottom"lip><"clear">',
                                                             buttons = list('print', list(extend =  "csv", title = "file"),
                                                                            list(extend =  "pdf", title = "file")),
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    rownames = FALSE, selection = list(mode = 'single', target = 'row')
    )
  })
  
  ## Gene Kinetic Plots from Spline Table
  output$tab2_george_whole_kin <- renderPlot({
    gene <- values$spline_gene
    cell_type <- values$spline_cell_type
    #smooth = values$smooth
    if (is.null(cell_type) || is.null(gene)) {
      return()
    }
    withProgress(session = session, value = 0, {
      p <- try(plotSingleGene(celltype = cell_type, gene = gene), silent = T)
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  
  ## Gene Kinetic Plots of whole Lung (todo, change to meta cell type)
  #
  output$tab2_whole_kin <- renderPlot({
    gene <- values$gene
    cell_type <- values$meta_cell_type
    #gene <- "Arg1"
    smooth = values$smooth
    type = values$res
    #cell_type <- "Macrophages"
    if (is.null(cell_type) || is.null(gene)) {
      return()
    }
    withProgress(session = session, value = 0, {
      p <- try(genLinePlot(filename, gene, cell_type, metafile, min_cells = 5, type = type, smooth = smooth), silent = T)
      class(p)[3] <- "wholelung_kinplot"
      incProgress(0.2, detail = "Plotting...")
      p <- check_save(p)
      p
    })
  })
  output$tab3_ccn_table <- DT::renderDataTable({
    rec <- values$ccn_rec_ct
    lig <- values$ccn_lig_ct
    if (is.null(rec) || is.null(lig)) {
      return()
    }
    dt <- getRecLigTable(cell_type_rec = rec, cell_type_lig = lig)
    DT::datatable(dt, extensions = 'Buttons', options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px",
                                                             searchHighlight = T, dom = '<"top"Bf>rt<"bottom"lip><"clear">',
                                                             buttons = list('print', list(extend =  "csv", title = "file"),
                                                                            list(extend =  "pdf", title = "file")),
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    rownames = FALSE,
    selection = list(mode = 'single', target = 'row')
    )
  })
  output$tab3_ccn_splines <- renderPlot({
    rec_ct = values$ccn_rec_ct
    lig_ct = values$ccn_lig_ct
    rec = values$ccn_rec
    lig = values$ccn_lig
    if (is.null(rec) | is.null(rec_ct)) {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(plot_RecLig_expression(rec = rec, lig = lig, rec_ct = rec_ct, lig_ct = lig_ct), silent = T)
      class(p)[3] <- "ccn_spline_plot"
      incProgress(0.2, detail = "Plotting...")
      check_save(p)
      })
    })
  
  ## High Resolution Epithelium
  output$tab4_celltype_panel <- renderPlot({
    gene_name <- values$epi_gene
    #gene_name = "Sftpc"
    if (is.null(gene_name)) {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(genUMAPplot(h5 = epi_filename, meta = epi_metafile, gene_name = gene_name), silent = T)
      class(p)[3] <- "epi_umap"
      umap <-
        check_save(p) + theme(plot.margin = unit(c(2, 0, 2, 0), "lines"))
        #p + theme(plot.margin = unit(c(1, 2, 1, 2), "lines"))
      incProgress(0.4, detail = "Umap")
      
      p <- try(dotPlot(h5 = epi_filename, meta = epi_metafile, gene_name = gene_name), silent = T)
      class(p)[3] <- "epi_dotplot"
      dotplot <-
        check_save(p) + theme(plot.margin = unit(c(2, 2, 2, 1), "lines"))
        #p + theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
      incProgress(0.4, detail = "Dotplot")
      
      p <- grid.arrange(umap, dotplot, widths = c(1, 0.8), padding = unit(0, "line"))
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  ## Highres Epi Markers Table
  output$tab4_markers_table <- DT::renderDataTable({
    #cell_type <- values$cell_type
    epi_cell_type = values$epi_cell_type
    res = values$epi_res
    if (is.null(epi_cell_type)) {
      return()
    }
    dt <- getMarkersTable(cell_type = epi_cell_type, resolution = res)
    DT::datatable(dt, extensions = 'Buttons', options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px",
                                                             searchHighlight = T, dom = '<"top"Bf>rt<"bottom"lip><"clear">',
                                                             buttons = list('print', list(extend =  "csv", title = "file"),
                                                                            list(extend =  "pdf", title = "file")),
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    rownames = FALSE,
    selection = list(mode = 'single', target = 'row')
    )
  })
  
  output$tab5_epi_kin <- renderPlot({
    gene <- values$epi_gene
    cell_type <- values$epi_cell_type
    smooth = values$smooth
    if (is.null(cell_type) || is.null(gene)) {
      return()
    }
    withProgress(session = session, value = 0, {
      p <- try(genLinePlot(epi_filename, gene, cell_type, epi_metafile, min_cells = 5, type = "meta_cell_type", smooth = smooth, epi = T), silent = T)
      class(p)[3] <- "epi_kinplot"
      incProgress(0.2, detail = "Plotting...")
      check_save(p)
    })
  })
  
  ## Convergence
  output$tab6_conv <- renderPlot({
    gene_name <- values$epi_gene
    if (is.null(gene_name)) {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(convergence_feature_plot(gene_name = gene_name), silent = T)
      class(p)[3] <- "conv_diffmap"
      diffmap <-
        check_save(p) + theme(plot.margin = unit(c(2, 1, 2, 1), "lines"))
      incProgress(0.4, detail = "Generating Diffusionmap")
      
      p <- try(convergence_traj_single_gene(gene_name = gene_name), silent = T)
      class(p)[3] <- "conv_traj"
      traj <-
        check_save(p) + theme(plot.margin = unit(c(2, 2, 2, 1), "lines"))
      incProgress(0.4, detail = "Generating Trajectory")
      
      p <- grid.arrange(diffmap, traj, widths = c(1, 1), padding = unit(1, "line"))
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  
  ## Trajectory
  output$tab7_traj <- renderPlot({
    gene_name <- values$epi_gene
    #gene_name = "Sftpc"
    if (is.null(gene_name)) {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(adi_at1_feature_plot(gene_name = gene_name), silent = T)
      class(p)[3] <- "at1adi_diffmap"
      diffmap <-
        check_save(p) + theme(plot.margin = unit(c(2, 1, 2, 1), "lines"))  #top, right, bottom, and left 
      #p + theme(plot.margin = unit(c(1, 2, 1, 2), "lines"))
      incProgress(0.4, detail = "Diffmap")
      
      p <- try(adi_at1_traj_single_gene(gene_name = gene_name), silent = T)
      class(p)[3] <- "at1adi_traj"
      traj <-
        check_save(p) + theme(plot.margin = unit(c(2, 2, 2, 1), "lines"))
      #p + theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
      incProgress(0.4, detail = "Trajectory")
      
      p <- grid.arrange(diffmap, traj, widths = c(1, 1), padding = unit(1, "line"))
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  
  ## Extras
  #deal with selection from marker's table
  observeEvent(input$tab1_markers_table_rows_selected, {
    row_selected <- input$tab1_markers_table_rows_selected
    
    isolate(cell_type <- values$cell_type)
    dt <- markers_table[markers_table$cell_type == cell_type,]
    
    new_gene_name <- dt[row_selected, "gene"]
    values$gene <- new_gene_name
  })
  
  observeEvent(input$tab4_markers_table_rows_selected, {
    row_selected <- input$tab4_markers_table_rows_selected
    
    isolate(epi_cell_type <- values$epi_cell_type)
    isolate(epi_res <- values$epi_res)
    dt <- getMarkersTable(cell_type = epi_cell_type, resolution = epi_res)
    
    new_gene_name <- dt[row_selected, "gene"]
    values$epi_gene <- new_gene_name
  })
  
  #deal with selection from Receptor Ligand table
  observeEvent(input$tab3_ccn_table_rows_selected, {
    row_selected <- input$tab3_ccn_table_rows_selected
    
    isolate(rec_ct <- values$ccn_rec_ct)
    isolate(lig_ct <- values$ccn_lig_ct)
    dt <- getRecLigTable(cell_type_rec = rec_ct, cell_type_lig = lig_ct)
    
    new_rec <- dt[row_selected, "gene.rec"]
    new_lig <- dt[row_selected, "gene.lig"]
    values$ccn_rec <- new_rec
    values$ccn_lig <- new_lig
  })
  
  observeEvent(input$tab4_markers_table_rows_selected, {
    row_selected <- input$tab4_markers_table_rows_selected
    
    isolate(epi_cell_type <- values$epi_cell_type)
    isolate(epi_res <- values$epi_res)
    dt <- getMarkersTable(cell_type = epi_cell_type, resolution = epi_res)
    
    new_gene_name <- dt[row_selected, "gene"]
    values$epi_gene <- new_gene_name
  })
  
  # Download plots
  output$download_plots_button <-
    downloadHandler(
      filename = function() {
        isolate(tab <- input$tabs)
        gene <- ifelse(length(grep("whole", tab) == 0), values$gene, values$epi_gene)
        file_name <- paste0(strsplit(tab, "_")[[1]][1], "_", gene, ".zip")
        #paste0(gsub("\\s", "_", tab), "_plots.zip")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        isolate(tab <- input$tabs)
        if (tab == "tab1_whole_celltype") {
          plot_names <- c("wholelung_umap", "wholelung_dotplot")
          gene <- values$gene
        } else if (tab == "tab4_epi_celltype") {
          plot_names <- c("epi_umap", "epi_dotplot")
          gene <- values$epi_gene
        } else if (tab == "tab6_convergence") {
          plot_names <- c("conv_diffmap", "conv_traj")
          gene <- values$epi_gene
        } else if (tab == "tab7_AT1traj") {
          plot_names <- c("at1adi_diffmap", "at1adi_traj")
          gene <- values$epi_gene
        }
        
        files <- sapply(plot_names, function(x) {
          p <- plots[[x]]
          file_name <- paste0(x, "_", gene, ".pdf")
          if (is.null(p)) {
            return(NA)
          }
          ggsave(file = file_name, plot = p, dpi = 150)
          return(file_name)
        })
        files <- files[!is.na(files)]
        zip(file, files)
      }
    )
  
  output$download_plots_single_button <-
    downloadHandler(
      filename = function() {
        isolate(tab <- input$tabs)
        gene <- ifelse(length(grep("epi", tab) == 0), values$epi_gene, values$gene)
        file_name <- paste0(strsplit(tab, "_")[[1]][1], "_", gene, ".pdf")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        isolate(tab <- input$tabs)
        if (tab == "tab2_whole_kinetics") {
          plot_name <- "wholelung_kinplot"
        } else if (tab == "tab5_epi_kinetics") {
          plot_name <- "epi_kinplot"
        }
        p <- plots[[plot_name]]
        if (is.null(p)) {
          return(NA)
        }
        ggsave(file = file, plot = p, width = 9, height = 5, dpi = 150)
      }
    )
  
  output$download_ccn_plot_button <-
    downloadHandler(
      filename = function() {
        rec_ct = values$ccn_rec_ct
        lig_ct = values$ccn_lig_ct
        rec = values$ccn_rec
        lig = values$ccn_lig
        file_name <- paste0(rec_ct, "_", rec, "_", lig_ct, "_", lig, ".pdf")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        p <- plots[["ccn_spline_plot"]]
        if (is.null(p)) {
          return(NA)
        }
        ggsave(file = file, plot = p, width = 9, height = 5, dpi = 150)
      }
    )
}
)

#shinyApp(ui = ui, server = server)


  
  