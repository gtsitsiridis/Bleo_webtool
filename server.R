load_data <- function() {
  Sys.sleep(2)
  hide(id = "loading-content1",
       anim = TRUE,
       animType = "fade")
  hide(id = "loading-content2",
       anim = TRUE,
       animType = "fade")
}

shinyServer(function(input, output, session) {
  ### Descriptions
  help <- list()
  
  help[["tab1"]] <-
    HTML('<center><strong><p>Instructions</strong></p></center>')
  help[["tab2"]] <-
    HTML('<center><strong>Instructions<p></strong></p></center>')
  help[["tab3"]] <-
    HTML('<center><strong><p></strong></p></center>')
  help[["tab4"]] <-
    HTML('')
  help[["enrichment_tab"]] <-
    HTML('<center><strong><p></strong></p></center>')
  
  output$description <-
    renderUI(HTML("<center><h1></h1></center>"))
  
  output$help <- renderUI({
    tab <- input$tabs
    help[[tab]]
  })
  
  ### Helper functions
  check_save <- function(plot) {
    # Check if exists
    if (class(plot) == "try-error") {
      plot  <-
        emptyPlot()
    } else{
      # Save plot
      cl <- class(plot)[3]
      plots[[cl]] <- plot
    }
    plot
  }
  
  ### Reactive values
  plots <- reactiveValues(
    wholelung_distplot = NULL,
    wholelung_dotplot = NULL,
    hires_distplot = NULL,
    hires_dotplot = NULL
  )
  
  values <-
    reactiveValues(
      gene = NULL,
      cell_type = NULL,
      hires_markers_table = NULL,
      wholelung_markers_table = NULL
    )
  
  ### Pass input to values
  observeEvent(input$gene, {
    values$gene <- input$gene
  })
  observeEvent(input$cell_type, {
    values$cell_type <- input$cell_type
  })
  
  ### Define gene and cell type selectors
  output$tab1_resolution_selector <- renderUI({
    selectInput("hires_resolution", "Select file:", markers_cell_types_files$hires)
  })
  
  output$tab2_resolution_selector <- renderUI({
    selectInput("wholelung_resolution", "Select file:", markers_cell_types_files$wholelung)
  })
  
  output$cell_type_selector <- renderUI({
    selectInput("cell_type", "Query cell type:", cell_types)
  })
  output$gene_selector <- renderUI({
    selectInput("gene", "Query gene/protein:", genes)
  })
  
  ### Create plots
  output$tab1_celltype_panel <- renderPlot({
    gene_name <- values$gene
    if (is.null(gene_name)|| gene_name=="") {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(dotPlot(gene_name, "hires"), silent = T)
      class(p)[3] <- "hires_dotplot"
      dotplot <-
        check_save(p) + theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
      incProgress(0.4, detail = "Dotplot")
      
      p <- try(genUMAPplot(gene_name, "hires"),
               silent = T)
      class(p)[3] <- "hires_distplot"
      distplot <-
        check_save(p) + theme(plot.margin = unit(c(1, 2, 1, 2), "lines"))
      incProgress(0.4, detail = "UMAP")
      
      p<- grid.arrange(dotplot, distplot, widths = c(1, 1))
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  
  output$tab2_celltype_panel <- renderPlot({
    gene_name <- values$gene
    if (is.null(gene_name) || gene_name=="") {
      return()
    }
    withProgress(session = session, value = 0, {
      setProgress(message = "Calculation in progress")
      p <- try(dotPlot(gene_name, "wholelung"), silent = T)
      class(p)[3] <- "wholelung_dotplot"
      dotplot <-
        check_save(p) + theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
      incProgress(0.4, detail = "Dotplot")
      
      p <- try(genUMAPplot(gene_name, "wholelung"),
               silent = T)
      class(p)[3] <- "wholelung_distplot"
      distplot <-
        check_save(p) + theme(plot.margin = unit(c(1, 2, 1, 2), "lines"))
      incProgress(0.4, detail = "UMAP")
      
      p<- grid.arrange(dotplot, distplot, widths = c(1, 1))
      incProgress(0.2, detail = "Plotting...")
      p
    })
  })
  
  output$tab1_markers_table <- DT::renderDataTable({
    cell_type <- values$cell_type
    file <- input$hires_resolution
    if (is.null(cell_type) || is.null(file)) {
      return()
    }
    # gene <- values$gene
    dt <- getMarkersTable(cell_type, "hires", file)
    values$hires_markers_table <- dt
    DT::datatable(
      dt,
      extensions = 'Buttons',
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        searchHighlight = T,
        dom = '<"top"Bf>rt<"bottom"lip><"clear">',
        buttons = list(
          'print',
          list(extend =  "csv",
               title = "file"),
          list(extend =  "pdf",
               title = "file")
        )
      ),
      rownames = FALSE,
      selection = list(mode = 'single',
                       target = 'row')
    )
  })
 
  output$tab2_markers_table <- DT::renderDataTable({
    cell_type <- values$cell_type
    file <- input$wholelung_resolution
    if (is.null(cell_type) || is.null(file)) {
      return()
    }
    dt <- getMarkersTable(cell_type, "wholelung", file)
    values$wholelung_markers_table <- dt
    DT::datatable(
      dt,
      extensions = 'Buttons',
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        searchHighlight = T,
        dom = '<"top"Bf>rt<"bottom"lip><"clear">',
        buttons = list(
          'print',
          list(extend =  "csv",
               title = "file"),
          list(extend =  "pdf",
               title = "file")
        )
      ),
      rownames = FALSE,
      selection = list(mode = 'single',
                       target = 'row')
    )
  })
   
  ### Extra features
  # Download plots
  output$download_plots_button <-
    downloadHandler(
      filename = function() {
        isolate(tab <- input$tabs)
        paste0(gsub("\\s", "_", tab), "_plots.zip")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        isolate(tab <- input$tabs)
        if (tab == "tab1") {
          plot_names <- c("umap_plot")
        }
        if (tab == "tab2") {
          plot_names <- c("spline_plot")
        }
        files <- sapply(plot_names, function(x) {
          p <- plots[[x]]
          file_name <- paste0(x, ".pdf")
          if (is.null(p)) {
            return(NA)
          }
          ggsave(file = file_name, plot = p)
          return(file_name)
        })
        files <- files[!is.na(files)]
        zip(file, files)
      }
    )
  o <- observeEvent(input$gene, {
    if (!is.null(input$gene)) {
      load_data()
      o$destroy()
    }
  })
  
  #deal with selection from marker's table
  observeEvent(input$tab1_markers_table_rows_selected, {
    row_selected <- input$tab1_markers_table_rows_selected
    dt <- values$hires_markers_table
    new_gene_name <- dt[row_selected, "gene"]
    values$gene <- unlist(new_gene_name)
  })
  
  observeEvent(input$tab2_markers_table_rows_selected, {
    row_selected <- input$tab2_markers_table_rows_selected
    dt <- values$wholelung_markers_table
    new_gene_name <- dt[row_selected, "gene"]
    values$gene <-unlist(new_gene_name)
  })
})


