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
    HTML('<center><strong><p></strong></p></center>')
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
  plots <- reactiveValues()
  
  values <- reactiveValues()
  
  ### Pass input to values
  observeEvent(input$value1, {
    
  })
  
  ### Define gene and cell type selectors
  output$selector1 <- renderUI({
    selectInput("value1", "Query value1:", c("tmp1", "tmp2"))
  })
  
  ### Create plots
  output$plot1 <- renderPlotly({
    withProgress(session = session, value = 0.5, {
      p <- ggplot2::qplot(1:10, 1:10, geom = "line")
      class(p)[3] <- "plot1"
      p <- ggplotly(check_save(p),
                    source = "plot1")
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
          plot_names <- c("plot1")
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
  o <- observeEvent(input$value1, {
    if (!is.null(input$value1)) {
      load_data()
      o$destroy()
    }
  })
})