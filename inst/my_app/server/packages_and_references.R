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

# package_list <- c("Biobase", "DT",
#                   "EnhancedVolcano", "FactoMineR",
#                   "caret", "corrr", "datamods",
#                   "dendextend", "dplyr", "ggplot2",
#                   "ggpubr", "ggridges", "ggrepel",
#                   "graphics", "grid", "htmlTable",
#                   "heatmaply", "httr", "igraph",
#                   "jtools", "jsonlite", "lme4",
#                   "metabolomics", "mgm", "pheatmap",
#                   "plotROC", "plotly", "rstatix",
#                   "readr", "reticulate",
#                   "shiny", "shinyWidgets",
#                   "shinydashboard", "shinycssloaders",
#                   "shinyjs", "stats", "stringr",
#                   "tidyr", "tibble", "utils",
#                   "visNetwork", "rlang")
#
# knitr::write_bib(package_list, "packages.bib")
