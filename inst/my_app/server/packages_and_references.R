# output$markdown <- renderUI({
#   HTML(markdown::markdownToHTML(knitr::knit(file.path("server", "packages_and_references.Rmd"), quiet = TRUE), fragment.only = T))
# })

output$markdown <- renderUI({
  withMathJax(HTML(readLines(rmarkdown::render(input = file.path("server", "packages_and_references.Rmd"),
                                               output_format = rmarkdown::html_fragment(),
                                               quiet = TRUE
  ))))
})



#### Create references ####

# package_list <- c(
# "remotes", "httr", "dendextend", "stats", "dplyr", "ggplot2", "tidyr", "DT", "ggpubr",
# "plotly", "graphics", "visNetwork", "utils", "tibble", "stringr", "shinyWidgets",
# "shiny", "readr", "pheatmap", "mgm", "jtools", "grid", "heatmaply", "grDevices",
# "ggridges", "ggrepel", "FactoMineR", "factoextra", "datamods", "corrr", "reticulate",
# "igraph", "caret", "shinyjs", "jsonlite", "sjPlot", "pROC", "plotROC", "shinydashboard",
# "shinycssloaders", "rlang", "lme4", "Biobase", "rstatix", "EnhancedVolcano", "limma",
# "knitr", "rmarkdown", "markdown", "doParallel", "parallel", "foreach", "xgboost",
# "randomForest", "MLmetrics", "tidyverse", "rintrojs", "KEGGREST", "data.table", "easystats",
# "performance", "see"
# )
# knitr::write_bib(package_list, "packages.bib")
