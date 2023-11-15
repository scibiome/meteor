#' Run MeTEor Shiny App
#'
#' @importFrom httr content POST progress
#' @importFrom dendextend cutree
#' @importFrom stats prcomp predict reorder model.matrix
#' @importFrom dplyr vars all_of mutate filter arrange select slice between desc funs mutate_all mutate_at
#' @importFrom ggplot2 vars aes coord_equal coord_flip element_blank element_rect facet_wrap geom_bar geom_hline geom_line geom_point geom_smooth geom_vline ggplot ggtitle labs scale_color_manual stat_summary theme xlab ylab
#' @importFrom tidyr all_of drop_na pivot_wider
#' @importFrom DT JS datatable editData renderDT DTOutput
#' @importFrom shiny renderDataTable actionButton column downloadLink fluidRow HTML htmlOutput icon imageOutput img plotOutput selectInput sliderInput textOutput
#' @importFrom ggpubr mutate bgcolor ggarrange ggscatter ggboxplot  stat_pvalue_manual
#' @importFrom plotly mutate filter arrange select slice layout ggplotly plot_ly renderPlotly plotlyOutput style
#' @importFrom graphics plot
#' @importFrom visNetwork renderVisNetwork visEdges visEvents visExport visGetPositions visIgraphLayout visInteraction visNetwork visNetworkProxy visOptions visSave visNetworkOutput
#' @importFrom utils data head
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom stringr str_split
#' @importFrom shinyWidgets updateMultiInput updateSliderTextInput updateVirtualSelect sliderTextInput virtualSelectInput show_alert updatePickerInput
#' @importFrom shiny downloadHandler eventReactive HTML observe observeEvent reactive reactiveTimer reactiveVal reactiveValues renderImage renderPlot renderText renderUI req updateSelectInput
#' @importFrom readr read_csv
#' @importFrom pheatmap pheatmap
#' @importFrom mgm mgm
#' @importFrom jtools theme_apa
#' @importFrom grid gpar grid.draw rectGrob
#' @importFrom heatmaply heatmaply
#' @importFrom grDevices dev.off png
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggrepel geom_text_repel
#' @importFrom FactoMineR PCA
#' @importFrom factoextra fviz_contrib fviz_pca_ind fviz_pca_var fviz_screeplot
#' @importFrom datamods import_modal import_server
#' @importFrom corrr correlate stretch
#' @importFrom reticulate virtualenv_create use_virtualenv py_install source_python
#' @importFrom igraph cluster_fast_greedy simplify E graph.data.frame membership
#' @importFrom caret createDataPartition trainControl train varImp confusionMatrix createFolds
#' @importFrom shinyjs delay
#' @importFrom jsonlite fromJSON
#' @importFrom pROC roc
#' @importFrom plotROC geom_roc ggroc
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem menuSubItem sidebarMenu tabItem tabItems
#' @importFrom shinycssloaders withSpinner
#' @importFrom lme4 lmer
#' @importFrom sjPlot tab_model
#' @importFrom rlang sym
#' @importFrom Biobase ExpressionSet AnnotatedDataFrame exprs pData
#' @importFrom limma lmFit topTable eBayes
#' @importFrom EnhancedVolcano EnhancedVolcano
#' @importFrom remotes install_github
#' @importFrom rstatix get_summary_stats friedman_test friedman_effsize wilcox_test add_xy_position pairwise_t_test anova_test
#' @importFrom rmarkdown render
#' @importFrom knitr knit
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel stopCluster detectCores makeCluster clusterEvalQ
#' @importFrom foreach foreach %dopar%
#' @importFrom magrittr %>%
#' @export meteor
#'

meteor <- function() {
  appDir <- system.file("my_app", package = "MeTEor")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", host = "0.0.0.0", port = 3838, quiet = F)
}
