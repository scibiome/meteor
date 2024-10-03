#### Packages ####

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
library(pROC)
library(plotROC)
library(shinydashboard)
library(shinycssloaders)
library(randomForest)
library(xgboost)
library(MLmetrics)
library(rlang)
library(lme4)
library(sjPlot)
library(htmlTable)
library(Biobase)
library(rstatix)
library(EnhancedVolcano)
library(doParallel)
library(parallel)
library(foreach)
library(magrittr)
library(limma)
library(rmarkdown)
library(markdown)
library(KEGGREST)
library(rintrojs)
library(data.table) # TODO add to references
library(easystats)
library(performance)# TODO add to description + references
library(see) # TODO
library(patchwork)
library(shinybusy)

#### Python module ####

# Get information about the operating system
sys_info <- Sys.info()

# Extract the name of the operating system
os_name <- sys_info["sysname"]

IS_IN_CONTAINER <- Sys.getenv("IS_IN_CONTAINER")

if (IS_IN_CONTAINER == "TRUE") {
  print("In container, ENV already installed")
} else {
  print(IS_IN_CONTAINER)
  # Check the name of the operating system
  if (os_name == "Windows") {
    # Check if directory my_env exists
    if (!dir.exists("../my_env")) {
      reticulate::use_python(rminiconda::find_miniconda_python("miniconda_for_meteor", path = "C:\\miniconda_py_r"), required = TRUE)
      reticulate::conda_create(envname = "../my_env", python = rminiconda::find_miniconda_python("miniconda_for_meteor", path = "C:\\miniconda_py_r"))
      reticulate::use_condaenv("../my_env", required = T)
      reticulate::py_install(packages = c("pandas"))
      reticulate::py_install(packages = c("mprod-package"))
      reticulate::py_install(packages = c("numpy"))

    } else {
      reticulate::use_condaenv("../my_env", required = T)
    }
  } else if (os_name == "Linux") {
    # Check if directory my_env exists
    if (!dir.exists("../my_env")) {
      reticulate::use_python(rminiconda::find_miniconda_python("miniconda_for_meteor"), required = TRUE)
      reticulate::conda_create(envname = "../my_env", python = rminiconda::find_miniconda_python("miniconda_for_meteor"))
      reticulate::use_condaenv("../my_env", required = T)
      reticulate::py_install(packages = c(c("pandas == 1.5.2"), "mprod-package", c("numpy == 1.23.0")))
    } else {
      reticulate::use_condaenv("../my_env", required = T)
    }
  } else if (os_name == "Darwin") {
    # Check if directory my_env exists
    if (!dir.exists("../my_env")) {
      reticulate::use_python(rminiconda::find_miniconda_python("miniconda_for_meteor"), required = TRUE)
      reticulate::conda_create(envname = "../my_env", python = rminiconda::find_miniconda_python("miniconda_for_meteor"))
      reticulate::use_condaenv("../my_env", required = T)
      reticulate::py_install(packages = c(c("pandas == 1.5.2"), "mprod-package", c("numpy == 1.23.0")))
    } else {
      reticulate::use_condaenv("../my_env", required = T)
    }
  } else {
    print("Unknown operating system.")
  }
}


#### File size limit ####
options(shiny.maxRequestSize = 60 * 1024^2)

#### Server function #####
server <- function(input, output, session) {
  #### Example data ----
  # outside package
  # load("../../data/covid_data.rda")
  # load("../../data/example_data.rda")
  # inside package
  data("covid_data", package = "MeTEor")
  data("example_data", package = "MeTEor")

  #### Source: tcam_shiny.py ----
  reticulate::source_python("tcam_shiny.py")

  #### source: tutorial.R ====
  source(file.path("server", "tutorial.R"), local = TRUE)$value

  #### source: launch_modal.R ====
  source(file.path("server", "launch_modal.R"), local = TRUE)$value

  #### source: sidebar_selection.R ====
  source(file.path("server", "sidebar_selection.R"), local = TRUE)$value

  #### source: pca.R ====
  source(file.path("server", "pca.R"), local = TRUE)$value

  #### source: tcam.R ====
  source(file.path("server", "tcam.R"), local = TRUE)$value

  #### source: ridge_plot.R ====
  source(file.path("server", "ridge_plot.R"), local = TRUE)$value

  #### source: prediction_model.R ====
  source(file.path("server", "prediction_model.R"), local = TRUE)$value

  #### source: friedman_test.R ====
  source(file.path("server", "friedman_test.R"), local = TRUE)$value

  #### source: mixed_anova.R ====
  source(file.path("server", "mixed_anova.R"), local = TRUE)$value

  #### source: repeated_anova.R ====
  source(file.path("server", "repeated_measures_anova.R"), local = TRUE)$value

  #### source: linear_mixed_model.R ====
  source(file.path("server", "linear_mixed_model.R"), local = TRUE)$value

  ### source: network.R ====
  source(file.path("server", "network.R"), local = TRUE)$value

  #### source: line_plot.R ====
  source(file.path("server", "line_plot.R"), local = TRUE)$value

  #### source: cluster_heatmap.R ====
  source(file.path("server", "cluster_heatmap.R"), local = TRUE)$value

  #### source: volcano_plot.R ====
  source(file.path("server", "volcano_plot.R"), local = TRUE)$value

  #### source: query.R ====
  source(file.path("server", "query.R"), local = TRUE)$value

  #### source: packages_and_references ====
  source(file.path("server", "packages_and_references.R"), local = TRUE)$value
}
