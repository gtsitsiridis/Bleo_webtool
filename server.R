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
  plots <- reactiveValues(umap_plot = NULL, spline_plot = NULL)
  
  values <-
    reactiveValues(
      gene = NULL,
      cell_type = NULL,
      smooth = FALSE,
      res.2 = NULL,
      plot_type = NULL
    )
  
  ### Pass input to values
  observeEvent(input$gene, {
    values$gene <- input$gene
  })
  observeEvent(input$cell_type, {
    values$cell_type <- input$cell_type
  })
  observeEvent(input$plot_type, {
    values$plot_type <- input$plot_type
  })
  observeEvent(input$res.2, {
    values$res.2 <- input$res.2
  })
  ### Define gene and cell type selectors
  output$gene_selector <- renderUI({
    selectizeInput(
      inputId = "gene",
      label = "Query gene:",
      choices = genes,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  output$cell_type_selector <- renderUI({
    selectizeInput(
      inputId = "cell_type",
      label = "Query cell type:",
      choices = cell_types,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  output$res.2_selector <- renderUI({
    selectizeInput(
      inputId = "res.2",
      label = "Query res.2:",
      choices = res.2,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  output$plot_type_selector <- renderUI({
    radioButtons("plot_type", "Types", c("cell type", "louvain"), selected = "cell type")
  })
  observeEvent(plots$spline_plot, {
    if (!is.null(plots$spline_plot)) {
      output$smooth_button <- renderUI({
        checkboxInput("smooth_button",
                      "Smooth",
                      value = values$smooth,
                      width = "100%")
      })
    }
  })
  
  ### Create plots
  output$umap_plot <- renderPlot({
    withProgress(session = session, value = 0.5, {
      gene <- values$gene
      if (is.null(gene) || gene == "") {
        return(NULL)
      }
      p <-
        plot_UMAP_colored_by_expr(gene, expression.file = expression.file)
      class(p)[3] <- "umap_plot"
      p <- check_save(p)
      p
    })
  })
  
  output$spline_plot <- renderPlot({
    withProgress(session = session, value = 0.5, {
      gene <- values$gene
      smooth <-
        ifelse(is.null(input$smooth_button),
               FALSE,
               input$smooth_button)
      values$smooth <- smooth
      plot_type <- values$plot_type
      clust <- NULL
      if (!is.null(plot_type)) {
        clust <-
          ifelse((plot_type == "cell type"),
                 values$cell_type,
                 values$res.2)
      } else{
        return(NULL)
      }
      if (is.null(gene) ||
          gene == "" || is.null(clust) || clust == "") {
        return(NULL)
      }
      p <-
        genLinePlot(
          gene,
          clust,
          meta = meta,
          type = make.names(plot_type),
          expression.file = expression.file,
          smooth = smooth
        )
      class(p)[3] <- "spline_plot"
      p <- check_save(p)
      p
    })
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
})
