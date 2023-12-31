#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(httr)
library(dendextend)
library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(ggpubr)
library(plotly)
library(graphics)
library(visNetwork)
library(utils)
library(tibble)
library(stringr)
library(shinyWidgets)
library(shiny)
library(readr)
library(pheatmap)
library(mgm)
library(jtools)
library(grid)
library(heatmaply)
library(grDevices)
library(ggridges)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(datamods)
library(corrr)
library(reticulate)
library(igraph)
library(caret)
library(shinyjs)
library(jsonlite)
library(plotROC)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(shinyjs)

##### UI #####

ui <- dashboardPage(
  dashboardHeader(title = "MeTEor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("fas fa-home")),
      menuItem("File", tabName = "file", icon = icon("fas fa-file")),
      menuItem("Time",
        tabName = "helloWorld", icon = icon("fas fa-sliders-h"),
        sliderTextInput(
          inputId = "timepoint",
          label = "Select time point:",
          choices = "",
          selected = "",
          grid = TRUE,
          animate = TRUE
        ), startExpanded = F
      ),
      menuItem("Categorical Variables",
        tabName = "categorical",
        icon = icon("fas fa-border-all"),
        selectInput(
          inputId = "catVars",
          label = "Choose categorical variable",
          choice = "",
          selected = "",
          multiple = FALSE
        ),
        selectInput(
          inputId = "trtmt",
          label = "Categorical variable Levels:",
          choices = "",
          selected = "",
          multiple = TRUE
        ), startExpanded = F
      ),
      tags$head(
        tags$script("
      $(document).ready(function() {
        $('#my-virtual-select').selectize({
          onItemAdd: function(value, item) {
            // Prevent scrolling to the top of the page
            return false;
          }
        });
      });
    ")
      ),
      pickerInput("metabolite_Picker", "Select metabolites",
        choices = "", multiple = TRUE,
        options = pickerOptions(
          # actionsBox = TRUE,
          selectedTextFormat = "count",
          liveSearch = TRUE,
          showIcon = TRUE,
          showTick = TRUE,
        ),
      ),
      fluidRow(
        column(width = 4, actionButton("add_metabolites", "Add all")),
        column(width = 8, actionButton("remove_metabolites", "Remove all"))
      ),
      menuItem("Dimensionality Reduction",
        tabName = "dr", icon = icon("fas fa-dice-d20"),
        menuSubItem("PCA", tabName = "pca"),
        menuSubItem("Tensor Factorization", tabName = "tcam")
      ),
      menuItem("Ridge Plots",
        tabName = "rp", icon = icon("fas fa-water"),
        menuSubItem("Compare time points", tabName = "op1"),
        menuSubItem("Compare groups", tabName = "op2")
      ),
      menuItem("Prediction", tabName = "pred", icon = icon("fas fa-arrow-trend-up")),
      menuItem("Statistical Testing",
        tabName = "stattest", icon = icon("fas fa-chart-column"),
        menuSubItem("Friedman test", tabName = "fried"),
        menuSubItem("Mixed ANOVA", tabName = "mixedanova"),
        menuSubItem("RM ANOVA", tabName = "repanova"),
        menuSubItem("Linear mixed model", tabName = "lmm")
      ),
      menuItem("Network Diagram",
        tabName = "net", icon = icon("fas fa-circle-nodes"),
        menuSubItem("Pearson Correlation Network", tabName = "pcn"),
        menuSubItem("Gaussian Graphical Model", tabName = "ggm")
      ),
      menuItem("Line Plot",
        tabName = "lp", icon = icon("fas fa-chart-line"),
        menuSubItem("By individuals", tabName = "il"),
        menuSubItem("By groups", tabName = "gl"),
        menuSubItem("Mean Lines", tabName = "ml")
      ),
      menuItem("Cluster Heatmap",
        tabName = "clustheat", icon = icon("fas fa-temperature-full"),
        menuSubItem("Single metabolite", tabName = "sm"),
        menuSubItem("By timepoint", tabName = "bt")
      ),
      menuItem("Volcano Plot", tabName = "volcano", icon = icon("fas fa-volcano")),
      menuItem("Enrichment", tabName = "export", icon = icon("fas fa-file-export")),
      menuItem("Packages & References", tabName = "packages", icon = icon("fas fa-box"))
    )
  ),
  dashboardBody(

    ##### Theme #####

    tags$style("@import url(https://use.fontawesome.com/releases/v6.2.0/css/all.css);"),
    # tags$head(
    #   tags$style(
    #     HTML("
    #     .selectize-dropdown-content .option {
    #       color: red;
    #     }
    #   ")
    #   )
    # ),
    tags$head(tags$style(HTML("
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #8f2e37;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #af3040;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #af3040;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #2c242f;
                                }

                                /* siderbar text color
                                .skin-blue .main-sidebar .sidebar{
                                 color: #FFFFFF;}

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #24242b;
                                }

                                /* sidebar border left color
                                .skin-blue .sidebar-menu > li.active > a,
                                .skin-blue .sidebar-menu > li:hover > a {
                                  border-left-color: #8f2e37;
                                }


                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #42263a;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #edf0f4;
                                }

                                "))),


    #### tab items ####




    tabItems(
      tabItem(
        tabName = "intro",
        box(
          title = "MeTEor - Metabolite trajectory explorer",
          status = "primary",
          solidHeader = F,
          collapsible = F,
          width = 12,
          fluidRow(
            column(width = 6, HTML("<p>   High-throughput data are commonplace in metabolomics research.
                                          Moreover, there is a considerable increase in longitudinal, i.e.
                                          time course metabolomics experiments.
                                          These allow for more detailed insights into biological
                                          processes and time-resolved characterization of systems responses.
                                          Statistical analysis of such data, however, is more challenging than in the case of snapshot metabolomics.
                                          To make explorative analysis of these data readily available to researchers without
                                          a deep background in computer science and programming,
                                          we present MeTEor, an R Shiny app providing  a set of
                                          state-of-the-art statistical analysis methods,
                                          including correlation networks, clustermaps and vulcanoplots,
                                          as well as visualizations for interactive data exploration.
                                          Additionally, advanced methods for dimensionality reduction of
                                          multivariate longitudinal data are integrated [1].
                                          Hence, MeTEor streamlines exploratory analysis of
                                          time-resolved metabolomics data and offers user-friendly
                                          access to advanced data science approaches.</p>

                                          <p>References</p>
                                          <p>1 Mor, U., Cohen, Y., Valdés-Mas, R., Kviatcovsky, D., Elinav, E., & Avron, H. (2022).
                                          Dimensionality reduction of longitudinal’omics data using modern tensor factorizations.
                                          PLOS Computational Biology, 18(7), e1010212.</p>
                                          <p> Hex logo made by Lisa-Marie Bente. </P>")),
            column(
              width = 6, align = "center",
              img(
                src = "logo.png", width = "50%", height = "50%",
                style = "-webkit-filter: drop-shadow(5px 5px 5px #222);
                                      filter: drop-shadow(5px 5px 5px #222);
                                      position: top;"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "file",
        fluidRow(
          box(
            title = "Data import", status = "info", solidHeader = TRUE,
            htmlOutput("info_box_launch"),
            fluidRow(
              column(
                width = 4,
                actionButton("launch_modal", "Click for data importing")
              )
            ),
            column(
              width = 4,
              # sample_ui("sample_id")
            )
          )
        ),
        fluidRow(column(width = 11, DT::dataTableOutput("datatable"))),
        fluidRow(column(width = 11, DT::dataTableOutput("datatable.wide")))
      ),
      tabItem(tabName = "dr"),
      tabItem(
        tabName = "pca",
        fluidRow(box(
          title = "Principal Component Analysis (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_pca")
        )),
        fluidRow(box(
          title = "Select the principal components for dimensions 1-3.",
          status = "primary", solidHeader = TRUE,
          column(width = 4, selectInput(
            inputId = "pcA",
            label = "First Dim",
            choice = "",
            selected = "",
            multiple = FALSE
          )),
          column(width = 4, selectInput(
            inputId = "pcB",
            label = "Second Dim",
            choice = "",
            selected = "",
            multiple = FALSE
          )),
          column(width = 4, selectInput(
            inputId = "pcC",
            label = "Third Dim",
            choice = "",
            selected = "",
            multiple = FALSE
          ))
        )),
        fluidRow(
          column(width = 6, plotlyOutput("eigenPCA", width = "100%", height = "500px")),
          column(width = 6, plotlyOutput("indPCA", width = "100%", height = "500px"))
        ),
        fluidRow(
          column(width = 6, plotlyOutput("contribPCa", width = "100%", height = "500px")),
          column(width = 6, plotlyOutput("contribPCb", width = "100%", height = "500px"))
        ),
        fluidRow(column(width = 12, plotOutput("biplotPCA", width = "100%", height = "1000px"))),
        fluidRow(column(width = 12, actionButton(
          "add_graph_pc_cross_3d",
          "Add 3D PCA"
        ))),
        fluidRow(column(width = 12, plotlyOutput("biplot3D", width = "100%", height = "1000px") %>% withSpinner(color = "#0dc5c1")))
      ),
      tabItem(
        tabName = "tcam",
        fluidRow(box(
          title = "Modern Tensor Factorization (TCAM) & Principal Component Analysis (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_tcam")
        )),
        fluidRow(
          box(
            title = "Select the principal components for dimensions 1-3.",
            status = "primary", solidHeader = TRUE,
            column(width = 4, selectInput(
              inputId = "pcAlong",
              label = "First Dim",
              choice = "",
              selected = "",
              multiple = FALSE
            )),
            column(width = 4, selectInput(
              inputId = "pcBlong",
              label = "Second Dim",
              choice = "",
              selected = "",
              multiple = FALSE
            )),
            column(width = 4, selectInput(
              inputId = "pcClong",
              label = "Third Dim",
              choice = "",
              selected = "",
              multiple = FALSE
            ))
          ),
          box(
            title = "Select the factors for dimensions 1-3.",
            status = "primary", solidHeader = TRUE,
            column(width = 4, selectInput(
              inputId = "pcAfc",
              label = "First Dim",
              choice = "",
              selected = "",
              multiple = FALSE
            )),
            column(width = 4, selectInput(
              inputId = "pcBfc",
              label = "Second Dim",
              choice = "",
              selected = "",
              multiple = FALSE
            )),
            column(width = 4, selectInput(
              inputId = "pcCfc",
              label = "Third Dim",
              choice = "",
              selected = "",
              multiple = FALSE
            ))
          )
        ),
        # fluidRow(
        #
        # ),
        fluidRow(
          column(width = 6, plotlyOutput("ind_pca_long", width = "100%", height = "500px")),
          column(width = 6, plotlyOutput("ind_FC", width = "100%", height = "500px"))
        ),
        fluidRow(
          column(width = 6, plotlyOutput("pca_loadings_long", width = "100%", height = "500px")),
          column(width = 6, plotlyOutput("fc_loadings_long", width = "100%", height = "500px"))
        ),
        fluidRow(
          column(6, actionButton("add_graph_pc_3d", "Add 3D PCA")),
          column(6, actionButton("add_graph_fc_3d", "Add 3D FC"))
        ),
        fluidRow(
          column(width = 6, plotlyOutput("ind_pca_long_3D", width = "100%", height = "1000px")),
          column(width = 6, plotlyOutput("ind_FC_3D", width = "100%", height = "1000px"))
        )
      ),
      tabItem(tabName = "rp"),
      tabItem(
        tabName = "op1",
        fluidRow(box(
          title = "Ridge Plot (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_ridge1")
        )),
        fluidRow(box(
          title = "Select time points:",
          status = "primary", solidHeader = TRUE,
          fluidRow(column(
            width = 12,
            selectInput(
              inputId = "time.names",
              label = "Time point",
              choices = "",
              selected = "",
              multiple = TRUE
            )
          )),
        )),
        fluidRow(column(width = 12, actionButton(
          "rp_compute",
          "Compute"
        ))),
        plotOutput("ridgeplot1", width = "100%", height = "1000px")
      ),
      tabItem(
        tabName = "op2",
        fluidRow(box(
          title = "Ridge Plot (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_ridge2")
        )),
        fluidRow(column(width = 12, actionButton(
          "rp_compute2",
          "Compute"
        ))),
        plotOutput("ridgeplot2", width = "100%", height = "1000px")
      ),
      tabItem(
        tabName = "pred", ################# PREDCTION
        fluidRow(box(
          title = "Binary Prediction (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_pred")
        )),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              "prediction_method",
              "prediction_method",
              choices = c("RF", "LR", "XGB"),
              selected = "LR",
            )
          ),
          column(
            width = 3,
            radioButtons(
              "valitation_method",
              "valitation_method",
              choices = c("CV", "LOOCV"),
              selected = "CV",
            )
          )
        ),
        fluidRow(
          column(1, h5("testdata in %")),
          column(
            width = 2,
            selectInput("testdata", NULL, c(0, 0.1, 0.2), selected = 0.5),
          ),
        ),
        fluidRow(
          column(width = 1, actionButton(
            "rf_prediction",
            "compute"
          )),
          column(width = 6, actionButton(
            "load_top_features",
            "Add top features to selection"
          ))
        ),
        fluidRow(
          column(
            width = 6,
            conditionalPanel(
              "input.rf_prediction != false",
              conditionalPanel(
                condition = "input.testdata != 0",
                plotOutput("prediction")
              ),
              conditionalPanel(
                condition = "input.testdata == 0",
                textOutput("prediction_text")
              )
            ),
          ),
          column(width = 6, plotlyOutput("feature_imp_plot")),
        ),
        fluidRow(
          column(
            width = 12,
            DTOutput("pred_scores")
          )
        ),
      ),
      tabItem(tabName = "stattest"),
      tabItem(
        tabName = "fried",
        fluidRow(
          box(
            title = "Friedman test (Info)",
            status = "info", solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            htmlOutput("info_box_friedman")
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "id8", label = "Metabolites :",
              choices = "",
              selected = "",
              width = "800px",
              multiple = F,
              selectize = TRUE
            )
          )
        ),
        fluidRow(column(
          width = 4,
          actionButton("act_fried", "Compute")
        )),
        fluidRow(column(width = 12, DTOutput("summary_stat"))),
        fluidRow(column(width = 6, DTOutput("res_fried")), column(width = 6, DTOutput("res_fried_eff"))),
        # fluidRow(),
        fluidRow(column(width = 12, plotOutput("boxfried", width = "100%", height = "1000px")))
      ),
      tabItem(
        tabName = "mixedanova",
        tabsetPanel(
          tabPanel(
            "Mixed ANOVA",
            fluidRow(box(
              title = "Mixed ANOVA (Info)",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              htmlOutput("info_box_mixed_anova")
            )),
            fluidRow(
              column(
                width = 8,
                selectInput(
                  inputId = "id10", label = "Metabolites:",
                  choices = "",
                  selected = "",
                  width = "800px",
                  multiple = F,
                  selectize = TRUE
                ),
              )
            ),
            fluidRow(column(
              width = 4,
              actionButton("act_mixed_anova", "Compute!")
            )),
            fluidRow(column(width = 12, DT::DTOutput("summary_stats_mixed_anova"))),
            fluidRow(column(width = 12, box(
              title = "Mixed ANOVA results:",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = FALSE,
              verbatimTextOutput("res_mixed_anova")
            ))),
            fluidRow(column(width = 12, plotOutput("boxplot_mixed_anova", width = "100%", height = "1000px"))),
          ),
          tabPanel(
            "Mixed ANOVA for Feature Selection",
            fluidRow(box(
              title = "Mixed ANOVA for feature selection (Info)",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              htmlOutput("info_box_mixed_anova_selection")
            )),
            fluidRow(
              column(width = 3, selectInput("selectfactor", "Select factor:", c("Categorical", "Time", "Time x Categorical"), selected = "Time x Categorical"))
            ),
            fluidRow(
              column(width = 1, actionButton("act_mixed_anova_selection", "Compute!")),
              column(width = 2, actionButton("load_top_features_anova", "Add top features to selection"))
            ),
            fluidRow(column(width = 4, DTOutput("ranking.sorted"))),
          ))),
          tabItem(
            tabName = "repanova",
            tabsetPanel(
          tabPanel(
            "Repeated Measures ANOVA",
            fluidRow(box(
              title = "Repeated Measures ANOVA (Info)",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              htmlOutput("info_box_repeated_anova")
            )),
            fluidRow(
              column(
                width = 8,
                selectInput(
                  inputId = "repeated_anova_metabolite", label = "Metabolites:",
                  choices = "",
                  selected = "",
                  width = "800px",
                  multiple = F,
                  selectize = TRUE
                ),
              )
            ),
            fluidRow(
              column(
                width = 8,
                selectInput(
                  inputId = "repeated_anova_category",
                  label = "Category:",
                  choices = "",
                  selected = "",
                  width = "800px",
                  multiple = F,
                  selectize = TRUE
                ),
              )
            ),

            fluidRow(column(
              width = 4,
              actionButton("act_repeated_anova", "Compute!")
            )),
            fluidRow(column(width = 12, DT::DTOutput("summary_stats_repeated_anova"))),
            fluidRow(column(width = 12, box(
              title = "Repeated Measures ANOVA results:",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = FALSE,
              verbatimTextOutput("res_repeated_anova")
            ))),
            fluidRow(column(width = 12, plotOutput("boxplot_repeated_anova", width = "100%", height = 1000))),
          ),

          tabPanel(
            "Repeated Measures ANOVA for Feature Selection",
            fluidRow(box(
              title = "Repeated ANOVA (Info)",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              htmlOutput("info_box_repeated_anova_selection")
            )),

            fluidRow(
              column(
                width = 8,
                selectInput(
                  inputId = "repeated_category", label = "Category:",
                  choices = "",
                  selected = "",
                  width = "800px",
                  multiple = F,
                  selectize = TRUE
                ),
              )
            ),
            fluidRow(
              column(width = 1, actionButton("act_repeated_anova_selection", "Compute!")),
              column(width = 2, actionButton("load_top_features_repeated_anova", "Add top features to selection"))
            ),
            fluidRow(column(width = 4, DTOutput("repeated.ranking.sorted"))),
          )
        )




        # fluidRow(column(width = 12, actionButton("load_top_features_anova", "Add top features to selection")))
      ),
      tabItem(
        tabName = "lmm",
        tabsetPanel(
          tabPanel(
            "Linear mixed model",
            fluidRow(box(
              title = "Linear Mixed Model (Info)",
              status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              htmlOutput("info_box_linear_mixed_model")
            )),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "id9", label = "Metabolites:",
                  choices = "",
                  selected = "",
                  width = "800px",
                  multiple = F,
                  selectize = TRUE
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "lmm_select", label = "Select model:",
                  choices = c("Random intercept", "Random slope", "Random intercept + slope"),
                  selected = "Random intercept + slope",
                  width = "800px",
                  multiple = F,
                  selectize = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                actionButton("act_lmm", "Compute!"),
                style = "padding-bottom:17px",
              )
            ),
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Mixed model selection:",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  htmlOutput("model.code")
                )
              )
            ),
            fluidRow(column(
              width = 12,
              box(
                title = "Model summary:",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("result.single")
              )
            )),
            fluidRow(
              column(
                width = 6,
                plotlyOutput("lmm_model")
              ),
              column(
                width = 6,
                plotlyOutput("line_plot")
              )
            ),
          #  fluidRow(
          #    column(
          #      width = 12, height = 12,
          #      plotlyOutput("vis_grid")
          #    )
          #  ),
          ),
          tabPanel(
            "Model comparison",
            fluidRow(box(
              title = "Model comparison", status = "info", solidHeader = TRUE,
              htmlOutput("info_box_linear_mixed_model_comp")
            )),
            fluidRow(
              column(
                # align="center",
                width = 4,
                actionButton("act_lmm2", "Compute!"),
                style = "padding-bottom:17px",
              )
            ),
            fluidRow(
              column(
                width = 8,
                box(
                  width = 12,
                  title = "Model summary comparison:",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  htmlOutput("multi_tab")
                )
              )),
            fluidRow(
              column(
                width = 12,
                box(
                  width = 10,
                  title = "ANOVA results:",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput("res.anova")
                )
              ))
            )
        ),
        # fluidRow(
        #   column(
        #     width = 12,
        #     box(
        #       title = "Model summary comparison:",
        #       status = "primary", solidHeader = TRUE,
        #       collapsible = TRUE,
        #       htmlOutput("multi_tab")
        #     )
        #   )
        # )
      ),
      tabItem(tabName = "net"),
      tabItem(
        tabName = "pcn",
        fluidRow(box(
          title = "Pearson Correlation Network (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_pcn")
        )),
        fluidRow(
          column(width = 3, sliderInput(
            inputId = "abscorr",
            label = "Absolute correlation:",
            min = 0,
            max = 1,
            value = c(0.3, 0.6),
            step = 0.01
          )),
          column(width = 4, selectInput(
            inputId = "layoutselect.pcn",
            label = "Choose layout:",
            choices = c(
              "layout_as_star",
              "layout_as_tree",
              "layout_in_circle",
              "layout_nicely",
              "layout_on_grid",
              "layout_on_sphere",
              "layout_randomly",
              "layout_with_dh",
              "layout_with_fr",
              "layout_with_gem",
              "layout_with_graphopt",
              "layout_with_kk",
              "layout_with_lgl",
              "layout_with_mds",
              "layout_with_sugiyama"
            ),
            selected = "layout_nicely"
          ))
        ),
        fluidRow(
          column(
            width = 12,
            visNetworkOutput("network", height = "70vh")
          )
        ), # note that column widths in a fluidRow should sum to 12
        fluidRow(
          column(
            width = 12,
            actionButton("store_position.pcn", "Store positions!"),
            downloadLink("downloadNetwork.pcn", "Download network")
          )
        ),
        fluidRow(
          column(width = 12, "Click and hold nodes for a second to select additional nodes.")
        ),
        fluidRow(
          column(
            width = 12,
            DTOutput("tbl")
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput(outputId = "scatterplot")
          )
        )
      ),
      tabItem(
        tabName = "ggm",
        fluidRow(box(
          title = "Gaussian Graphical Model Network (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_ggm")
        )),
        fluidRow(
          column(width = 3, sliderInput(
            inputId = "abscorr_ggm",
            label = "Absolute correlation:",
            min = 0,
            max = 1,
            value = c(0.3, 0.6),
            step = 0.01
          )),
          column(width = 4, selectInput(
            inputId = "layoutselect.ggm",
            label = "Choose layout:",
            choices = c(
              "layout_as_star",
              "layout_as_tree",
              "layout_in_circle",
              "layout_nicely",
              "layout_on_grid",
              "layout_on_sphere",
              "layout_randomly",
              "layout_with_dh",
              "layout_with_fr",
              "layout_with_gem",
              "layout_with_graphopt",
              "layout_with_kk",
              "layout_with_lgl",
              "layout_with_mds",
              "layout_with_sugiyama"
            ),
            selected = "layout_nicely"
          ))
        ),
        fluidRow(column(width = 4, actionButton(
          "ggm_compute",
          "Compute"
        ))),
        fluidRow(
          column(
            width = 12,
            withSpinner(visNetworkOutput("network_ggm", height = "70vh"))
          )
        ), # note that column widths in a fluidRow should sum to 12
        fluidRow(
          column(
            width = 12,
            actionButton("store_position", "Store positions !"),
            downloadLink("downloadNetwork", "Download network")
          )
        ),
        fluidRow(
          column(width = 12, "Click and hold nodes for a second to select additional nodes.")
        ),
        fluidRow(
          column(
            width = 12,
            withSpinner(DTOutput("tbl_ggm"))
          )
        ),
        fluidRow(
          column(
            width = 12,
            withSpinner(plotOutput(outputId = "scatterplot_ggm"))
          )
        )
      ),
      # Line Plots
      tabItem("lp"),
      tabItem(
        fluidRow(box(
          title = "Line Plot individuals (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_il")
        )),
        tabName = "il",
        fluidRow(column(width = 12, actionButton(
          "lp_compute2",
          "Compute"
        ))),
        plotlyOutput("plot2", width = "100%", height = "1000px")
      ),
      tabItem(
        tabName = "gl",
        fluidRow(box(
          title = "Line Plot groups (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_gl")
        )),
        fluidRow(column(width = 12, actionButton(
          "lp_compute3",
          "Compute"
        ))),
        plotlyOutput("plot3", width = "100%", height = "1000px")
      ),
      tabItem(
        tabName = "ml",
        fluidRow(box(
          title = "Line Plot Mean (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_ml")
        )),
        fluidRow(column(width = 12, actionButton(
          "lp_compute4",
          "Compute"
        ))),
        plotlyOutput("plot4", width = "100%", height = "1000px")
      ),
      tabItem(tabName = "clustheat"),
      tabItem(
        tabName = "sm",
        fluidRow(box(
          title = "Single Metabolite Heatmap (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_sm")
        )),
        selectInput(
          inputId = "id6", label = "Metabolites :",
          choices = "",
          selected = "",
          width = "800px",
          multiple = F,
          selectize = TRUE
        ),
        fluidRow(
          column(width = 6, sliderInput("clustrow1", "Number of clusters (rows):",
            min = 2, max = 10, value = 2
          )),
          column(width = 6, sliderInput("clustcol1", "Number of clusters (cols):",
            min = 2, max = 10, value = 2
          ))
        ),
        imageOutput("clustheatmap", width = "300%", height = "200%")
      ),
      tabItem(
        tabName = "bt",
        fluidRow(box(
          title = "By Timepoint Heatmap (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_tb")
        )),
        fluidRow(column(width = 12, actionButton(
          "heatmap_compute",
          "Compute"
        ))),
        fluidRow(
          column(width = 6, sliderInput("clustrow", "Number of clusters (rows):",
            min = 2, max = 10, value = 2
          )),
          column(width = 6, sliderInput("clustcol", "Number of clusters (cols):",
            min = 2, max = 10, value = 2
          ))
        ),
        plotlyOutput("clustheatmap2", height = "1000px")
      ),

      # fluidRow(column(width = 12, plotlyOutput("clustheatmap2")))),
      tabItem(
        tabName = "volcano",
        fluidRow(box(
          title = "Volcano (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_volcano")
        )),
        fluidRow(
          tags$style("#categorytext {font-size:20px;
                             color:darkblue;}"),
          tags$style("#timetext {font-size:20px;
                             color:darkblue;}"),
          column(width = 6, textOutput("categorytext")),
          column(width = 6, textOutput("timetext"))
        ),
        fluidRow(
          column(width = 3, selectInput(
            inputId = "feature1",
            label = "Choose feature level 1",
            choice = "",
            selected = "",
            multiple = FALSE
          )),
          column(width = 6, selectInput(
            inputId = "feature2",
            label = "Choose feature level 2",
            choice = "",
            selected = "",
            multiple = FALSE
          )),
        ),
        fluidRow(column(width = 12, dataTableOutput("res.voc.table"))),
        fluidRow(
          column(width = 3, numericInput("pcut", "Cut-off statistical significance:", 1e-05, min = 0, max = 0.10)),
          column(width = 6, numericInput("fccut", "Cut-off for absolute log2 fold-change:", 1, min = 0, max = 10))
        ),
        # textOutput("text2"),
        fluidRow(
          column(
            width = 4,
            actionButton("act_volc", "Compute")
          ),
          column(width = 2, actionButton("load_top_features_volcano", "Add top features to selection"))


        ),
        fluidRow(column(width = 12, height = 20, withSpinner(plotOutput("volcano.plot", width = "100%", height = "1000px"))))
      ),
      tabItem(
        tabName = "export",
        fluidRow(box(
          title = "Enrichment (Info)",
          status = "info", solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE,
          htmlOutput("info_box_enrichment")
        )),
        fluidRow(
          column(
            width = 12,
            DTOutput("raw_list.dt")
          )
        ),
        fluidRow(
          column(
            width = 12,
            actionButton("api.go", "Make API request"),
          )
        ),
        fluidRow(column(width = 12, DTOutput("export.dt"))),
        fluidRow(column(width = 12, DTOutput("pathways.dt")))
      ),
      tabItem(
        tabName = "packages",
        fluidPage(
          uiOutput("markdown")
        )
      )
    )
  )
)
