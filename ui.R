# Define UI for application that plots random distributions
spinner <- function(ui_element) {
  withSpinner(ui_element, type = 3, color.background = "white")
}

shinyUI(tagList(
  includeCSS("www/style.css"),
  useShinyjs(),
  
  dashboardPage(
    skin = "black",
    dashboardHeader(
      titleWidth = 340,
      title = HTML(
        "Mouse lung injury and regeneration webtool -- Schiller and Theis labs @ Helmholtz Zentrum München - German Research Center for Environmental Health"
      ),
      tags$li(
        class = "dropdown",
        HTML(
          "<img src='Overview_logos.png' style='padding-top:10px;padding-right:10px;' height='70'/>"
        )
      )
    ),
    dashboardSidebar(
      width = 250,
      div(id = "loading-content1", class = "loading-content",
          style = "z-index:10;"),
      sidebarMenu(
        id = "tabs",
        menuItem("Tab1", tabName = "tab1"),
        menuItem("Whole lung time course regression", tabName = "tab2"),
        menuItem("Tab3", tabName = "tab3"),
        menuItem("Tab4", tabName = "tab4")
      ),
      
      conditionalPanel(
        "input.tabs == 'tab1' || input.tabs =='tab2'",
        uiOutput("gene_selector"),
        type = 2,
        color.background = "#222d32"
      ),
      conditionalPanel(
        "input.tabs =='tab2'",
        uiOutput("cell_type_selector"),
        type = 2,
        color.background = "#222d32"
      )
      # ,
      # HTML("<a id='github-btn'href='' target='_blank'><i class='fa fa-github'></i></a>")
    ),
    
    dashboardBody(
      div(
        id = "loading-content2",
        class = "loading-content",
        h2("Loading..."),
        HTML(
          "<img src='spinner.gif' style='padding-top:10px;padding-right:10px;' height='70'/>"
        ),
        style = "z-index:10;"
      ),
      fluidRow(column(10, htmlOutput("help")), column(
        2,
        conditionalPanel(
          condition = "input.tabs == 'tab1'||input.tabs == 'tab2'",
          downloadButton(label = "Download plots",
                         # class = 'btn-primary',
                         outputId = "download_plots_button")
        )
      )),
      
      tabItems(
        # First tab content
        tabItem(tabName = "tab1",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 12,
                    spinner(plotOutput("umap_plot", height = "600px"))
                  )
                )),
        tabItem(tabName = "tab2",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 12,
                    spinner(plotOutput("spline_plot", height = "600px")),
                    uiOutput("smooth_button")
                  )
                )),
        tabItem(tabName = "tab3",
                fluidRow(box(
                  collapsible = TRUE,
                  width = 12
                ))),
        tabItem(tabName = "tab4",
                fluidRow(box(
                  collapsible = TRUE,
                  width = 12
                )))
        
      )
    )
  )
))
