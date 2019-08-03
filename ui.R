## My Version - Tidy
## Whiny Webtool - User Interface
#ui <- tagList(
shinyUI(tagList(
  includeCSS(css_file),
  useShinyjs(),
  dashboardPage(
    skin = "black",
    dashboardHeader(
      titleWidth = 720,
      title = HTML("Mouse Lung Injury and Regeneration – Schiller and Theis labs @ Helmholtz Zentrum München"),
      tags$li(
        class = "dropdown",
        HTML(
          "<img src='Overview_logos.png' style='padding-top:10px;padding-right:10px;' height='70'/>"
        ),
        tags$style(
          ".main-header .logo {font-size:17px; text-align:left}") ## change to left alignment
      )
    ),
    dashboardSidebar(width = 310,
                     ## Makes it white / loading like
                     # div(
                     #   id = "loading-content1",class="loading-content",
                     #   style="z-index:10;"
                     # ),
                     sidebarMenu(
                       id = "tabs",
                       
                       menuItem("Whole Lung Cell Type Signatures", tabName = "tab1_whole_celltype"),
                       menuItem("Whole Lung Time Course Differential Expression", tabName = "tab2_george_whole_kinetics"),
                       # menuItem("Whole Lung Gene Kinetics_ALTERNATIVE", tabName = "tab2_whole_kinetics"),
                       menuItem("Whole Lung Cell-Cell Communication", tabName = "tab3_ccn"),
                       menuItem("Lung Epithelium Cell Type Signatures", tabName = "tab4_epi_celltype"),
                       # menuItem("Lung Epithelium Gene Kinetics", tabName = "tab5_epi_kinetics"),
                       menuItem("Lung Epithelium Convergence", tabName = "tab6_convergence"),
                       menuItem("Lung Epithelium ADI to AT1", tabName = "tab7_AT1traj")
                     ),
                     
                     conditionalPanel(
                       "input.tabs == 'tab1_whole_celltype'",
                       uiOutput("cell_type_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab2_whole_kinetics'",
                       uiOutput("meta_cell_type_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab1_whole_celltype' || input.tabs == 'tab2_whole_kinetics'",
                       uiOutput("gene_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab2_whole_kinetics'",
                       uiOutput("res_selector")),
                     
                     
                     conditionalPanel(
                       "input.tabs == 'tab2_george_whole_kinetics'",
                       uiOutput("spline_cell_type_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab2_george_whole_kinetics'",
                       uiOutput("spline_gene_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab3_ccn'",
                       uiOutput("ccn_rec_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab3_ccn'",
                       uiOutput("ccn_lig_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab4_epi_celltype' | input.tabs == 'tab5_epi_kinetics'",
                       uiOutput("epi_cell_type_selector")),
                     conditionalPanel(
                       paste0("input.tabs == 'tab4_epi_celltype' | input.tabs == 'tab5_epi_kinetics' ",
                         "| input.tabs == 'tab6_convergence' | input.tabs == 'tab7_AT1traj'"),
                       uiOutput("epi_gene_selector"),
                       type = 2, color.background = "#222d32"),
                     
                     conditionalPanel(
                       "input.tabs == 'tab4_epi_celltype' | input.tabs == 'tab5_epi_kinetics'",
                       uiOutput("epi_res_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab2_whole_kinetics' | input.tabs == 'tab5_epi_kinetics'",
                       uiOutput("smooth_selector"))
                     #conditionalPanel(
                     # "input.tabs == 'tab2_whole_kinetics' | input.tabs == 'tab5_epi_kinetics'",
                     # uiOutput("min_cell_selector"))
    ),
    
    
    dashboardBody(
      # div(
      #   id = "loading-content2",class="loading-content",
      #   h2("Loading..."), HTML("<img src='spinner.gif' style='padding-top:10px;padding-right:10px;' height='70'/>"
      #   ),
      #   style="z-index:10;"
      # ),
      fluidRow(column(10, htmlOutput("help")), column(
        2,
        conditionalPanel(
          condition = paste("input.tabs == 'tab1_whole_celltype' || input.tabs == 'tab4_epi_celltype'",
                            "|| input.tabs == 'tab6_convergence' || input.tabs == 'tab7_AT1traj'"),
          downloadButton(
            label = "Download Plots", outputId = "download_plots_button"
          )
        ),
        conditionalPanel(
          condition = paste("input.tabs == 'tab2_whole_kinetics' || input.tabs == 'tab2_george_whole_kinetics'",
                            "|| input.tabs == 'tab5_epi_kinetics'"),
          downloadButton(
            label = "Download Plot", outputId = "download_plots_single_button"
          )
        ),
        conditionalPanel(
          condition = "input.tabs == 'tab3_ccn'",
          downloadButton(
            label = "Download Plot", outputId = "download_ccn_plot_button"
          )
        )
      )),
      
      tabItems(
        tabItem(tabName = "tab1_whole_celltype",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 8,
                    spinner(plotOutput("tab1_celltype_panel", height = "500px"))
                  ),
                  box(
                    collapsible = TRUE,
                    width = 4,
                    spinner(DT::dataTableOutput("tab1_markers_table", height = "500px"))
                  )
                )),
        
        tabItem(tabName = "tab2_george_whole_kinetics",
                mainPanel(
                  spinner(plotOutput("tab2_george_whole_kin", height = "500px", width = "1100px"))
                )),
        
        tabItem(tabName = "tab2_whole_kinetics",
                mainPanel(
                  spinner(plotOutput("tab2_whole_kin", height = "500px", width = "1100px"))
                )),
        tabItem(tabName = "tab3_ccn",
                mainPanel(
                  spinner(DT::dataTableOutput("tab3_ccn_table", height = "400px", width = "1100px")),
                  spinner(plotOutput("tab3_ccn_splines", height = "500px", width = "1100px"))
                )),
        
        ## Highres Epithelium Tab
        tabItem(tabName = "tab4_epi_celltype",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 8,
                    spinner(plotOutput("tab4_celltype_panel", height = "500px"))
                  ),
                  box(
                    collapsible = TRUE,
                    width = 4,
                    #uiOutput("tab3_resolution_selector"),
                    spinner(DT::dataTableOutput("tab4_markers_table", height = "500px")))
                )),
        tabItem(tabName = "tab5_epi_kinetics",
                mainPanel(
                  spinner(plotOutput("tab5_epi_kin", height = "500px", width = "1100px"))
                )),
        tabItem(tabName = "tab6_convergence",
                box(
                  spinner(plotOutput("tab6_conv", height = "500px", width = "1100px")),
                  width="8"
                ),
                box(
                  spinner(tableOutput("conv_annot_table")),
                  width = 4,
                  height = "500px"
                )
                ),
        tabItem(tabName = "tab7_AT1traj",
                box(
                  spinner(plotOutput("tab7_traj", height = "500px", width = "1100px")),
                  width = 8
                ),
                box(
                  spinner(tableOutput("traj_annot_table")),
                  height = "500px",
                  width = 4
                ))
        
      ))
  ))
)
