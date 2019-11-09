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
      title = HTML("Mouse Lung Injury & Regeneration – Schiller & Theis labs @ Helmholtz Center Munich"),
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
                     div(
                       id = "loading-content1",class="loading-content",
                       style="z-index:10;"
                     ),
                     sidebarMenu(
                       id = "tabs",
                       
                       menuItem("Whole Lung Cell Type Signatures", tabName = "tab1_whole_celltype"),
                       menuItem("Whole Lung Time Course Differential Expression", tabName = "tab2_george_whole_kinetics"),
                       # menuItem("Whole Lung Gene Kinetics_ALTERNATIVE", tabName = "tab2_whole_kinetics"),
                       menuItem("Whole Lung Cell-Cell Communication", tabName = "tab3_ccn"),
                       menuItem("Lung Epithelium Cell Type Signatures", tabName = "tab4_epi_celltype"),
                       # menuItem("Lung Epithelium Gene Kinetics", tabName = "tab5_epi_kinetics"),
                       menuItem("Convergence trajectory", tabName = "tab6_convergence")#,
                       #menuItem("Krt8 progenitors to AT1 trajectory", tabName = "tab7_AT1traj")
                     ),
                     
                     conditionalPanel(
                       "input.tabs == 'tab1_whole_celltype'",
     selectInput("cell_type", "Query cell type:", {
       tmp_cell_types <- select_cell_type(metafile, column = "cell_type")
       tmp_cell_types[tmp_cell_types!="AM (Bleo)"]
       }, selected = "Macrophages")),
                      
                       # uiOutput("cell_type_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab2_whole_kinetics'",
                       uiOutput("meta_cell_type_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab1_whole_celltype' || input.tabs == 'tab2_whole_kinetics'",
                       selectizeInput("gene", label="Query gene:", choices=NULL, options = list(maxOptions = 100))),
                  
                       # uiOutput("gene_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab2_whole_kinetics'",
                       radioButtons("res", "Resolution",  
                                    choices = c("cell_type", "meta_cell_type"), selected = "meta_cell_type")), 
                       # uiOutput("res_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab2_george_whole_kinetics'",
                       selectInput("spline_cell_type", "Query cell type:", names(wholeLung_spline), selected = "alv_epithelium")),
                       # uiOutput("spline_cell_type_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab2_george_whole_kinetics'",
                       uiOutput("spline_gene_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab3_ccn'",
                       selectInput("ccn_rec_ct", "Query receptor:", select_cell_type(rec_lig, column = "cluster.rec"), selected = "Macrophages")),
                       # uiOutput("ccn_rec_selector")),
                     conditionalPanel(
                       "input.tabs == 'tab3_ccn'",
                       selectInput("ccn_lig_ct", "Query ligand:", select_cell_type(rec_lig, column = "cluster.lig"), selected = "Fibroblasts")),
                       # uiOutput("ccn_lig_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab4_epi_celltype' || input.tabs == 'tab5_epi_kinetics'",
                       uiOutput("epi_cell_type_selector")),
                     conditionalPanel(
                       paste0("input.tabs == 'tab4_epi_celltype' || input.tabs == 'tab5_epi_kinetics'"),
                       selectizeInput("epi_gene", label="Query gene:", choices=NULL, options = list(maxOptions = 100)),
                       type = 2, color.background = "#222d32"),
                     conditionalPanel(
                       "input.tabs == 'tab6_convergence'",
                       selectizeInput("conv_epi_gene", label="Query gene:", choices=NULL, options = list(maxOptions = 100))
                      ),
                     conditionalPanel(
                       "input.tabs == 'tab7_AT1traj'",
                       selectizeInput("traj_epi_gene", label="Query gene:", choices=NULL, options = list(maxOptions = 100))
                     ),
                     # conditionalPanel(
                     #   "input.tabs == 'tab4_epi_celltype' | input.tabs == 'tab5_epi_kinetics'",
                     #   radioButtons("epi_res", "Resolution",  
                     #                choices = c("res_2", "cell_type_2", "cell_type_4"), selected = "cell_type_2")),
                       # uiOutput("epi_res_selector")),
                     
                     conditionalPanel(
                       "input.tabs == 'tab2_whole_kinetics' | input.tabs == 'tab5_epi_kinetics'",
                       checkboxInput("smooth", "smooth Plot", value = T))
                       # uiOutput("smooth_selector"))
                     #conditionalPanel(
                     # "input.tabs == 'tab2_whole_kinetics' | input.tabs == 'tab5_epi_kinetics'",
                    # sliderInput("min_cells", label = "Minimum cell number expressed per sample", 
                      # min = 5, max = 20, value = 5))  
                     # # uiOutput("min_cell_selector"))
    ),
    
    
    dashboardBody(
      div(
        id = "loading-content2",class="loading-content",
        h2("Loading...please wait"),
        # HTML("<img src='spinner.gif' style='padding-top:50px;padding-right:10px;' height='100'/>"
        # ),
        style="z-index:10;"
      ),
      fluidRow(column(10, htmlOutput("help")), column(
        2,
        conditionalPanel(
          condition = paste("input.tabs == 'tab1_whole_celltype' || input.tabs == 'tab2_whole_kinetics' || input.tabs=='tab2_george_whole_kinetics' || 
                            input.tabs == 'tab5_epi_kinetics' || input.tabs == 'tab3_ccn'
                            || input.tabs == 'tab4_epi_celltype'",
                            "|| input.tabs == 'tab6_convergence' || input.tabs == 'tab7_AT1traj'"),
          downloadButton(
            label = "Download Plots", outputId = "download_plots_button"
          )
        )
      )),
      
      tabItems(
        tabItem(tabName = "tab1_whole_celltype",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 8,
                    spinner(plotOutput("tab1_celltype_panel", height = "700px"))
                  ),
                  box(
                    collapsible = TRUE,
                    width = 4,
                    spinner(DT::dataTableOutput("tab1_markers_table", height = "600px"))
                  )
                )),
        
        tabItem(tabName = "tab2_george_whole_kinetics",
                box(
                  collapsible = TRUE,
                  width = 8,
                  spinner(plotOutput("tab2_george_whole_kin", height = "500px"))
                ),
                box(
                  collapsible = TRUE,
                  width = 4,
                  spinner(DT::dataTableOutput("whole_kin_table", height = "500px"))
                )
                ),
        
        tabItem(tabName = "tab2_whole_kinetics",
                mainPanel(
                  spinner(plotOutput("tab2_whole_kin", height = "500px", width = "1100px"))
                )),
        tabItem(tabName = "tab3_ccn",
                fluidRow(box(
                  spinner(DT::dataTableOutput("tab3_ccn_table", height = "400px")), width=12)),
                fluidRow(box(spinner(plotOutput("tab3_ccn_splines", height = "500px", width = "1100px")), width = 12)
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
                fluidRow(box(
                  spinner(plotOutput("tab6_conv", height = "500px", width = "1100px")),
                  width=12
                )),
                fluidRow(box(
                  spinner(DT::dataTableOutput("conv_annot_table", height = "500px")),
                  width = 12
                ))
                ),
        tabItem(tabName = "tab7_AT1traj",
               fluidRow( box(
                  spinner(plotOutput("tab7_traj", height = "500px", width = "1100px")),
                  width = 12
                )),
                fluidRow(tabBox(
                  spinner(DT::dataTableOutput("traj_annot_table", height = "500px")),
                  height = "500px",
                  width = 12
                )))
        
      ))
  ))
)
