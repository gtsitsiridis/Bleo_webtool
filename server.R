## My Version - Tidy
## Shiny Webtool - Server

load_data <- function() {
  Sys.sleep(2)
  hide(id = "loading-content1",
       anim = TRUE,
       animType = "fade")
  hide(id = "loading-content2",
       anim = TRUE,
       animType = "fade")
}


shinyServer(function(input, output, session){
    ### Descriptions
    help <- list()
    
    help[["tab1_whole_celltype"]] <-
      HTML('<center><strong><p>To select a cell type and gene, use the respective sidebar drop-down menus or click on a row in the table.</strong></p></center>')
    help[["tab2_george_whole_kinetics"]] <-
      HTML('<center><strong><p>To select a cell type and gene, use the respective sidebar drop-down menus.</strong></p></center>')
    help[["tab3_ccn"]] <-
      HTML('<center><strong><p>To select a receptor-ligand pair, click on a row in the table. Use the sidebar drop-down menus to narrow down your search.</strong></p></center>')
    help[["tab4_epi_celltype"]] <-
      HTML('<center><strong><p>To select a cell type and gene, use the respective sidebar drop-down menus or click on a row in the table.</strong></p></center>')
    help[["tab6_convergence"]] <-
      HTML('<center><strong><p>Please select a gene  by using the sidebar dropdown menu or clicking on a row in the table.</strong></p></center>')
    help[["tab7_AT1traj"]] <-
      HTML('<center><strong><p>Please select a gene  by using the sidebar dropdown menu or clicking on a row in the table.</strong></p></center>')
    
    output$description <-
      renderUI(HTML("<center><h1></h1></center>"))
    
    output$help <- renderUI({
      tab <- input$tabs
      help[[tab]]
    })
  
  
  check_save <- function(plot) {
    # Check if exists
    if (class(plot)[1] == "try-error") {
      type <- "general"
      if(class(plot)[3]=="ccn_spline_plot"){
        type <- "cc"
      }
      plot <- emptyPlot(type=type)
    } else{
      cl <- class(plot)[3]
      plots[[cl]] <- plot
    }
    plot
  }
  
  values <- reactiveValues(gene = NULL, cell_type = NULL, meta_cell_type = NULL, res = NULL,
                           smooth = NULL, spline_gene = NULL, spline_cell_type = NULL,
                           ccn = NULL, ccn_rec = NULL, ccn_lig = NULL,
                           epi_gene = NULL, epi_cell_type = NULL, epi_res = "cell_type_4",
                           tab1_markers_table = NULL, tab4_markers_table = NULL, tab2_whole_kin_table = NULL)
  
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
    req(input$gene)
    new_gene_name <- input$gene
    values$gene <- new_gene_name
  })
  observeEvent(input$cell_type, {
    req(input$cell_type)
    new_cell_type <- input$cell_type
    values$cell_type <- new_cell_type
  })
  observeEvent(input$meta_cell_type, {
    req(input$meta_cell_type)
    new_meta_cell_type <- input$meta_cell_type
    values$meta_cell_type <- new_meta_cell_type
  })
  observeEvent(input$res, {
    req(input$res)
    new_res <- input$res
    values$res <- new_res
  })
  
  observeEvent(input$spline_cell_type, {
    req(input$spline_cell_type)
    new_spline_cell_type <- input$spline_cell_type
    values$spline_cell_type <- new_spline_cell_type
  })
  observeEvent(input$spline_gene, {
    req(input$spline_gene)
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
    req(input$ccn_rec_ct)
    new_rec <- input$ccn_rec_ct
    values$ccn_rec_ct <- new_rec
  })
  observeEvent(input$ccn_lig_ct, {
    req(input$ccn_lig_ct)
    new_lig <- input$ccn_lig_ct
    values$ccn_lig_ct <- new_lig
  })
  
  observeEvent(input$epi_cell_type, {
    req(input$epi_cell_type)
    new_epi_cell_type <- input$epi_cell_type
    values$epi_cell_type <- new_epi_cell_type
  })
  
  observeEvent(input$epi_gene, {
    req(input$epi_gene)
    new_epi_gene_name <- input$epi_gene
    values$epi_gene <- new_epi_gene_name
  })
  
  observeEvent(input$conv_epi_gene, {
    req(input$conv_epi_gene)
    new_epi_gene_name <- input$conv_epi_gene
    values$epi_gene <- new_epi_gene_name
  })
  
  observeEvent(input$traj_epi_gene, {
    req(input$traj_epi_gene)
    new_epi_gene_name <- input$traj_epi_gene
    values$epi_gene <- new_epi_gene_name
  })

  observeEvent(input$epi_res, {
    req(input$epi_res) 
    new_res <- input$epi_res
    values$epi_res <- new_res
  })
  
  ### Define gene and cell type selectors
  output$meta_cell_type_selector <- renderUI({
    req(values$res)
    res = values$res
    selectInput("meta_cell_type", "Query cell type:", select_cell_type(metafile, column = res), selected = "Macrophages")
  })
  
  output$spline_gene_selector <- renderUI({
    req(values$tab2_whole_kin_table)
    if(is.null(values$tab2_whole_kin_table)) selectInput("spline_gene", "Query gene:", "Krt8", selected = "Krt8")
    else selectInput("spline_gene", "Query gene:", values$tab2_whole_kin_table$gene, selected = "Krt8")
  })

  output$epi_cell_type_selector <- renderUI({
    req(values$epi_res)
    if(is.null(values$epi_res))
      return()
    #res = "cell_type_2"
    res = values$epi_res
    selectInput("epi_cell_type", "Query cell type:", select_cell_type(epi_markers_table, type = res), selected = "AT2 cells")
  })
  
  # output$epi_gene_selector <- renderUI({
  #   req(input$tabs)
  #   sorted_genes <- epi_genes
  #   if(input$tabs == "tab6_convergence"){
  #     sorted_genes <- convergence_annot$Gene
  #   }else if (input$tabs == "tab7_AT1traj"){
  #     sorted_genes <- adi_at1_annot$Gene
  #   }
  #   
  #   selectInput("epi_gene", "Query gene:", sorted_genes, selected = "Sftpc")
  # })

  
  ## Code for Figures
  output$tab1_celltype_panel <- renderPlot({
    req(values$gene)
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
    req(values$cell_type)
    cell_type <- values$cell_type
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
    req(values$spline_gene, values$spline_cell_type)
    gene <- values$spline_gene
    cell_type <- values$spline_cell_type
    withProgress(session = session, value = 0, {
      p <- try(plotSingleGene(celltype = cell_type, gene = gene), silent = T)
      class(p)[3] <- "wholelung_george_kinplot"
      p <- check_save(p)
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  
  output$whole_kin_table <- DT::renderDataTable({
    req(values$spline_cell_type)
    cell_type <- values$spline_cell_type
    
    dt <- getWholeKinTable(cell_type)
    values$tab2_whole_kin_table <- dt
    DT::datatable(dt, extensions = 'Buttons', options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px",
                                                             searchHighlight = T, dom = '<"top"Bf>rt<"bottom"lip><"clear">',
                                                             buttons = list('print', list(extend =  "csv", title = "file"),
                                                                            list(extend =  "pdf", title = "file")),
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row')
    )
  })
  
  ## Gene Kinetic Plots of whole Lung (todo, change to meta cell type)
  #
  output$tab2_whole_kin <- renderPlot({
    req(values$gene, values$meta_cell_type)
    gene <- values$gene
    cell_type <- values$meta_cell_type
    #gene <- "Arg1"
    smooth = values$smooth
    type = values$res
    #cell_type <- "Macrophages"
    withProgress(session = session, value = 0, {
      p <- try(genLinePlot(filename, gene, cell_type, metafile, min_cells = 5, type = type, smooth = smooth), silent = T)
      class(p)[3] <- "wholelung_kinplot"
      incProgress(0.2, detail = "Plotting...")
      p <- check_save(p)
      p
    })
  })
  output$tab3_ccn_table <- DT::renderDataTable({
    req(values$ccn_rec_ct, values$ccn_lig_ct)
    rec <- values$ccn_rec_ct
    lig <- values$ccn_lig_ct
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
    req(values$ccn_rec_ct, values$ccn_lig_ct)
    rec_ct = values$ccn_rec_ct
    lig_ct = values$ccn_lig_ct
    rec = values$ccn_rec
    lig = values$ccn_lig
    # if (is.null(rec) | is.null(rec_ct)) {
    #   return()
    # }
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
    req(values$epi_gene)
    gene_name <- values$epi_gene
    #gene_name = "Sftpc"
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
    req(values$epi_cell_type)
    #cell_type <- values$cell_type
    epi_cell_type = values$epi_cell_type
    res = values$epi_res
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
    req(values$epi_gene, values$epi_cell_type)
    gene <- values$epi_gene
    cell_type <- values$epi_cell_type
    smooth = values$smooth
    withProgress(session = session, value = 0, {
      p <- try(genLinePlot(epi_filename, gene, cell_type, epi_metafile, min_cells = 5, type = "meta_cell_type", smooth = smooth, epi = T), silent = T)
      class(p)[3] <- "epi_kinplot"
      incProgress(0.2, detail = "Plotting...")
      check_save(p)
    })
  })
  
  ## Convergence
  output$tab6_conv <- renderPlot({
    req(values$epi_gene)
    gene_name <- values$epi_gene
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
    req(values$epi_gene)
    gene_name <- values$epi_gene
    #gene_name = "Sftpc"
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
  
   output$conv_annot_table  <- DT::renderDataTable({
    req(values$cell_type)
    cell_type <- values$cell_type
    dt <- convergence_annot
    DT::datatable(dt, extensions = 'Buttons', options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px",
                                                             searchHighlight = T, dom = '<"top"Bf>rt<"bottom"lip><"clear">',
                                                             buttons = list('print', list(extend =  "csv", title = "file"),
                                                                            list(extend =  "pdf", title = "file")),
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    rownames = FALSE, selection = list(mode = 'single', target = 'row')
    )
  })
  
 
  output$traj_annot_table <- DT::renderDataTable({
    req(values$cell_type)
    cell_type <- values$cell_type
    dt <- adi_at1_annot
    DT::datatable(dt, extensions = 'Buttons', options = list(pageLength = 25, scrollX = TRUE, scrollY = "400px",
                                                             searchHighlight = T, dom = '<"top"Bf>rt<"bottom"lip><"clear">',
                                                             buttons = list('print', list(extend =  "csv", title = "file"),
                                                                            list(extend =  "pdf", title = "file")),
                                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    rownames = FALSE, selection = list(mode = 'single', target = 'row')
    )
  })
     
  ## Extras
  #deal with selection from marker's table
  observeEvent(input$tab1_markers_table_rows_selected, {
    req(input$tab1_markers_table_rows_selected)
    row_selected <- input$tab1_markers_table_rows_selected
    isolate(cell_type <- values$cell_type)
    dt <- markers_table[markers_table$cell_type == cell_type,]
    
    new_gene_name <- dt[row_selected, "gene"]
    values$gene <- new_gene_name
  })
  
  observeEvent(input$tab4_markers_table_rows_selected, {
    req(input$tab4_markers_table_rows_selected)
    row_selected <- input$tab4_markers_table_rows_selected
    
    isolate(epi_cell_type <- values$epi_cell_type)
    isolate(epi_res <- values$epi_res)
    dt <- getMarkersTable(cell_type = epi_cell_type, resolution = epi_res)
    
    new_gene_name <- dt[row_selected, "gene"]
    values$epi_gene <- new_gene_name
  })
  
  
  
  #deal with selection from Receptor Ligand table
  observeEvent(input$tab3_ccn_table_rows_selected, {
    req(input$tab3_ccn_table_rows_selected)
    row_selected <- input$tab3_ccn_table_rows_selected
    
    isolate(rec_ct <- values$ccn_rec_ct)
    isolate(lig_ct <- values$ccn_lig_ct)
    dt <- getRecLigTable(cell_type_rec = rec_ct, cell_type_lig = lig_ct)
    
    new_rec <- dt[row_selected, "receptor"]
    new_lig <- dt[row_selected, "ligand"]
    values$ccn_rec <- new_rec
    values$ccn_lig <- new_lig
  })
  
  observeEvent(input$traj_annot_table_rows_selected, {
    req(input$traj_annot_table_rows_selected)    
    row_selected <- input$traj_annot_table_rows_selected

    dt <- adi_at1_annot
    
    new_gene_name <- dt[row_selected, "Gene"]
    values$epi_gene <- new_gene_name
  })
 
  observeEvent(input$conv_annot_table_rows_selected, {
    req(input$conv_annot_table_rows_selected)    
    row_selected <- input$conv_annot_table_rows_selected

    dt <- convergence_annot
    
    new_gene_name <- dt[row_selected, "Gene"]
    values$epi_gene <- new_gene_name
  }) 
  
  observeEvent(input$whole_kin_table_rows_selected, {
    req(input$whole_kin_table_rows_selected)    
    row_selected <- input$whole_kin_table_rows_selected
    
    dt <- values$tab2_whole_kin_table
    new_gene_name <- dt[row_selected, "gene"]
    values$spline_gene <- new_gene_name
  }) 
 
  observeEvent(input$tabs, {
    req(input$tabs)
    tab <- input$tabs
    mode <- 0
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
    } else if (tab == "tab2_george_whole_kinetics") {
      mode <- 1 
      plot_name <- "wholelung_george_kinplot"
    } else if (tab == "tab5_epi_kinetics") {
      mode <- 1
      plot_name <- "epi_kinplot"
    } else {
      return()
    }
    if (mode == 0){
      output$download_plots_button <-
        downloadHandler(
          filename = function() {
            gene <- ifelse(length(grep("whole", tab) == 0), values$gene, values$epi_gene)
            file_name <- paste0(strsplit(tab, "_")[[1]][1], "_", gene, ".zip")
          },
          content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
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
    }else if (mode == 1){
      output$download_plots_button <-
        downloadHandler(
          filename = function() {
            gene <- ifelse(length(grep("epi", tab) == 0), values$epi_gene, values$gene)
            file_name <- paste0(strsplit(tab, "_")[[1]][1], "_", gene, ".pdf")
            file_name
          },
          content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            p <- plots[[plot_name]]
            print(plot_name)
            if (is.null(p)) {
              print("test1")
              return(NA)
            }
            ggsave(file = file, plot = p, width = 9, height = 5, dpi = 150)
          }
        )
    } else if (mode==2){
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
  })
  
o <- observeEvent(input$gene, {
    if (!is.null(input$gene)) {
      load_data()
      o$destroy()
    }
  })
}
)

#shinyApp(ui = ui, server = server)


  
  