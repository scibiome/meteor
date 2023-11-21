output$conditionalsidebarMenuOutput <- renderMenu({
  conditionalPanel(condition = "input.menu1 !== 'file'",
                   sidebarMenu(id = "menu1",
                               menuItem("Introduction", tabName = "intro", icon = icon("fas fa-home")),
                               menuItem("File", tabName = "file", icon = icon("fas fa-file")),
                               menuItem("Time",
                                        tabName = "helloWorld", icon = icon("fas fa-sliders-h"), # TODO rename
                                        sliderTextInput(
                                          inputId = "timepoint",
                                          label = "Select time point:",
                                          choices = "",
                                          selected = "",
                                          grid = TRUE,
                                          animate = F
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
                   )),

  conditionalPanel(condition = "input.menu1 == 'file'",
                   sidebarMenu(id = "menu1",
                               menuItem("Introduction", tabName = "intro", icon = icon("fas fa-home")),
                               menuItem("File", tabName = "file", icon = icon("fas fa-file")),
                               # menuItem("Time",
                               #          tabName = "helloWorld", icon = icon("fas fa-sliders-h"), # TODO rename
                               #          sliderTextInput(
                               #            inputId = "timepoint",
                               #            label = "Select time point:",
                               #            choices = "",
                               #            selected = "",
                               #            grid = TRUE,
                               #            animate = F
                               #          ), startExpanded = F
                               # ),
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
                   ))




  })
