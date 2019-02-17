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
    HTML('<center><strong><p></strong></p></center>')
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
  plots <- reactiveValues(umap_plot = NULL)
  
  values <- reactiveValues(gene = NULL)
  
  ### Pass input to values
  observeEvent(input$gene, {
    values$gene <- input$gene
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
  
  ### Create plots
  output$umap_plot <- renderPlot({
    withProgress(session = session, value = 0.5, {
      gene <- values$gene
      if(is.null(gene) || gene == ""){
        return(NULL)
      }
      p <-plot_UMAP_colored_by_expr(gene, expression.file = expression.file)
      class(p)[3] <- "umap_plot"
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