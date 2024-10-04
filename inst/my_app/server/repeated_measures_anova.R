output$info_box_repeated_anova <- renderUI({
  #   HTML("<p align = 'justify'>
  # This section enables you to perform a mixed ANOVA analysis for a specific metabolite,
  # using a between-subjects factor (selected categorical variable) and a within-subjects factor (time).
  # The first table presents summary statistics for each measurement time point,
  # while the ANOVA results box provides information on the significance and
  # effect size (generalized eta squared) of the between factor, within factor, and their interaction.
  # <p></p>
  # In addition, a box plot is displayed in the upper left corner to illustrate the results for the between-within factors interactions.
  # The x-axis represents the measurement time point, and the color of the box plot indicates group membership.
  # Between-group pairwise comparisons are conducted using t-tests for dependent samples,
  # and multiple testing is corrected with Bonferroni.
  #   ")

  HTML("
    <p align='justify'>
      This section enables you to perform a repeated measures ANOVA analysis on a specific metabolite, utilizing a within-subjects factor (time).
    </p>
    <h4>Summary Statistics</h4>
    <p align='justify'>
      The first table presents summary statistics for each measurement time point.
    </p>
    <h4>Outlier Analysis</h4>
    <p align='justify'>
      The second table shows results from outlier analysis. Outliers are detected using the boxplot method and marked as is.outlier and is.extreme.
    </p>
    <h4>Normality Test</h4>
    <p align='justify'>
      The third table provides results from the Shapiro-Wilk test for normality for every time point. A p-value < 0.05 indicates a violation of the normality assumption.
    </p>
    <h4>Repeated Measures ANOVA Results</h4>
    <p align='justify'>
      The 'Repeated Measures ANOVA results' box displays the repeated measures ANOVA results, providing information on the significance and effect size (generalized eta squared) for the within-subjects factor (time).
    </p>
    <h4>Visual Representation</h4>
    <p align='justify'>
      A box plot is displayed to illustrate the trend over different time points of measurement. Each box plot represents a distribution for a given time point, with the x-axis representing the measurement time points and the y-axis representing the metabolite abundance levels.
    </p>
    <h4>Pairwise Comparisons</h4>
    <p align='justify'>
      Pairwise comparisons between different time points are conducted to determine significant differences in metabolite levels. To account for multiple testing, Bonferroni correction is applied.
    </p>
     <br><br>
    <u>When to Use:</u><br>
    <p align='justify'>
      Repeated measures ANOVA is suitable when:
    </p>
    <ul>
      <li>Measurements are taken on the same subjects or units over multiple time points or conditions.</li>
      <li>The focus is on detecting changes or differences within subjects over time or conditions.</li>
      <li>There's a need to control for individual differences or variability among subjects.</li>
    </ul>
    <br><br>
    <u>Used Packages and Additional Information:</u><br>
    <ul>
      <li>Kassambara A (2023). <em>rstatix: Pipe-Friendly Framework for Basic Statistical Tests</em>. R package version 0.7.2. Available at: <a href='https://CRAN.R-project.org/package=rstatix' target='_blank'>https://CRAN.R-project.org/package=rstatix</a></li>
      <li><a href='https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/' target='_blank'>Tutorial on Repeated Measures ANOVA</a></li>
    </ul>
")
})


observe({
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[, 3]) %>% as.list()

  updateMultiInput(session, "repeated_anova_metabolite", choices = metabolite.names, selected = metabolite.names)
})



observe({
  req(data())
  req(input$catVars)
  dat.nam <- data()
  a <- input$catVars
  dat.nam <- dat.nam[a]

  trt.names <- unique(dat.nam[, 1]) %>% as.list()

  choices <- trt.names[!is.na(trt.names)]

  updateSelectInput(session, "repeated_anova_category", choices = choices, selected = choices)
})


reactive_repeated_anova <- reactiveValues(
  computation_done = FALSE,
  data.filtered = NULL,
  summary_stats = NULL,
  res_mixed_anova = NULL,
  boxplot = NULL,
  pairwise = NULL,
  pairwise.filtered = NULL,
  outliers = NULL,
  shapiro = NULL,
)



observeEvent(input$act_repeated_anova, {
  data.filtered <- data() %>%
    filter(metabolites %in% input$repeated_anova_metabolite) %>%
    filter(!!sym(input$catVars) %in% input$repeated_anova_category)


  ##### Testing assumptions ######

  ## outliers

  a <- catv()

  vars <- c("id", "time", a, "metabolites", "values")

  outliers <- data.filtered %>%
    select(all_of(vars)) %>%
    group_by(time) %>%
    rstatix::identify_outliers(values) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))

  colnames(outliers) <- c("Time", "ID", "Group", "Metabolites", "Values", "is.outlier", "is.extreme")

  ## normality assumption
  shapiro <- data.filtered %>%
    group_by(time) %>%
    rstatix::shapiro_test(values) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))

  colnames(shapiro) <- c("Time", "Variable", "statistic", "p")


  summary_stats <- data.filtered %>%
    group_by(time) %>%
    rstatix::get_summary_stats(values, type = "mean_sd")

  summary_stats <- summary_stats %>%
    mutate(category = as.factor(input$catVars)) %>%
    select(time, category, variable, n, mean, sd)


  colnames(summary_stats) <- c("Time", "Category", "Variable", "N", "Mean", "SD")

  res_mixed_anova <- data.filtered %>%
    # group_by(!!sym(input$catVars)) %>%
    rstatix::anova_test(
      data = ., dv = values, wid = id,
      within = time # , detailed = TRUE
    ) %>%
    get_anova_table()

  form.test <- paste0("values ~ time")


  pairwise <- data.filtered %>%
    group_by(!!sym(input$catVars)) %>%
    pairwise_t_test(as.formula(form.test), p.adjust.method = "bonferroni")
  pairwise


  # Visualization: boxplots with p-values
  pairwise <- pairwise %>% add_xy_position(x = "category")

  pairwise.filtered <- pairwise %>% filter(input$catVars != as.character(pairwise[1, 1]))

  boxplot <- data.filtered %>%
    as.data.frame() %>%
    # mutate(catVars = as.factor(input$catVars)) %>%
    ggboxplot(.,
      x = "time", y = "values",
      color = "time", palette = "jco"
    ) +
    stat_pvalue_manual(pairwise.filtered, tip.length = 0, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(res_mixed_anova, detailed = TRUE),
      caption = get_pwc_label(pairwise)
    )

  reactive_repeated_anova$summary_stats <- summary_stats
  reactive_repeated_anova$res_mixed_anova <- res_mixed_anova
  reactive_repeated_anova$boxplot <- boxplot
  reactive_repeated_anova$data.filtered <- data.filtered
  reactive_repeated_anova$pairwise <- pairwise
  reactive_repeated_anova$pairwise.filtered <- pairwise.filtered
  reactive_repeated_anova$computation_done <- TRUE
  reactive_repeated_anova$outliers <- outliers
  reactive_repeated_anova$shapiro <- shapiro
})


output$summary_stats_repeated_anova <- renderDT(
  datatable(reactive_repeated_anova$summary_stats,
    caption = "Summary statistics",
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      paging = TRUE
    )
  )
)

output$outliers_repeated_anova <- renderDT(
  datatable(reactive_repeated_anova$outliers,
    caption = "Outlier detection (Univariate outlier detection using boxplot methods):",
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      paging = TRUE
    )
  )
)


output$normality_repeated_anova <- renderDT(
  datatable(reactive_repeated_anova$shapiro,
    caption = "Normality test (Shapiro-Wilk test for all time points.):",
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      paging = TRUE
    )
  )
)


output$res_repeated_anova <- renderPrint({
  reactive_repeated_anova$res_mixed_anova
})


output$report_repeated_measures_anova <- downloadHandler(
  filename = "report_repeated_measures_anova.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("~/PycharmProjects/meteor_github/inst/my_app/server/repeated_measures_anova_report.Rmd", tempReport, overwrite = TRUE)

    params <- list(reactive_repeated_anova_RMD = reactive_repeated_anova, input_RMD = input)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



### repeated measures ANOVA for feature selection

reactive_repeated_anova_selection <- reactiveValues(
  computation_done = FALSE,
  data.filtered = NULL,
  summary_stats = NULL,
  res_mixed_anova = NULL,
  boxplot = NULL,
  pairwise = NULL,
  pairwise.filtered = NULL
)
repeated.anova.feat.selection <- reactiveValues(ranking = NULL, ranking.sorted = NULL)


output$boxplot_repeated_anova <- renderPlot(
  {
    a <- 1
    # TODO check if there are more than onvalue for colors
    if (reactive_repeated_anova$computation_done) {
      boxplot <- reactive_repeated_anova$data.filtered %>%
        as.data.frame() %>%
        mutate(!!sym(input$catVars) := factor(!!sym(input$catVars))) %>%
        ggboxplot(.,
          x = "time", y = "values",
          color = "time", palette = "jco", fill = "#edeff4", size = 2
        ) +
        stat_pvalue_manual(reactive_repeated_anova$pairwise.filtered, tip.length = 0, hide.ns = TRUE) +
        labs(
          subtitle = get_test_label(reactive_repeated_anova$res_mixed_anova, detailed = TRUE),
          caption = get_pwc_label(reactive_repeated_anova$pairwise)
        ) + theme(
          text = element_text(size = 16),
          panel.background = element_rect(
            fill = "#edeff4",
            colour = NA_character_
          ), # necessary to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(
            fill = "#edeff4",
            colour = NA_character_
          ), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "#edeff4"),
          # legend.box.background = element_rect(fill = "#edeff4"),
          # legend.key = element_rect(fill = "#edeff4")
        )
      return(boxplot)
    }
  },
  bg = "#edeff4"
)

observeEvent(input$act_repeated_anova_selection, {
  # Get unique metabolites
  req(data())
  dat.nam <- data()
  metabolites <- unique(dat.nam[, 3]) %>% as.character()

  # Create an empty data frame for ranking
  ranking <- data.frame(metabolites = metabolites, time.p.value = NA)

  # Iterate over metabolites and calculate ANOVA p-value
  data_no_na <- data() %>%
    filter(!!sym(input$catVars) %in% input$repeated_category)
  data_no_na2 <- data_no_na[!is.na(data_no_na[input$catVars]), ]


  # Set up a parallel backend
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  # Load the required packages on each node
  clusterEvalQ(cl, {
    library(dplyr)
    library(magrittr)
    library(rstatix)
  })

  # Define the list of metabolites
  # metabolites <- c("metabolite1", "metabolite2", "metabolite3")

  # Define the function to be applied to each metabolite
  anova_func <- function(i, data, cat) {
    anova.res <- data %>%
      filter(metabolites == i) %>%
      anova_test(
        data = .,
        dv = values,
        wid = id,
        within = time # ,
        # detailed = TRUE
      ) %>%
      get_anova_table()
    return(c(
      i,
      anova.res[1, 5]
    ))
  }

  # Apply the function to each metabolite in parallel
  cat <- input$catVars
  results <- foreach(i = metabolites, .combine = rbind) %dopar% {
    anova_func(i, data_no_na2, cat)
  }
  # unique(data_no_na2["diagnosis"])
  # Convert the results to a data frame
  colnames(results) <- c("metabolites", "time.p.value")
  rownames(results) <- 1:nrow(results)
  ranking <- as.data.frame(results)

  # Stop the parallel backend
  stopCluster(cl)

  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
  }
  unregister()


  repeated.anova.feat.selection$ranking <- ranking
  result_anova_sorted <- repeated.anova.feat.selection$ranking %>%
    arrange(time.p.value)

  repeated.anova.feat.selection$ranking.sorted <- result_anova_sorted
})

observeEvent(input$load_top_features_repeated_anova,
  {
    # mobse
    req(data())
    dat.nam <- data()
    metabolite.names <- unique(dat.nam[, 3]) %>% as.list()

    ui_metabolites$selection <- c(ui_metabolites$selection, head(repeated.anova.feat.selection$ranking.sorted$metabolites, 10))

    updatePickerInput(session,
      inputId = "metabolite_Picker", choices = metabolite.names, selected = ui_metabolites$selection,
      choicesOpt = list(
        style = c(rep("color: black;", length(metabolite.names)))
      )
    )
  },
  ignoreInit = TRUE
)


output$repeated.ranking.sorted <- DT::renderDT(datatable(repeated.anova.feat.selection$ranking.sorted,
  caption = "Ranked metabolites by p-values",
  extensions = "Buttons",
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = TRUE,
    dom = "Bfrtip",
    buttons = c("copy", "print", "csv", "excel")
  )
))


observeEvent(input$catVars, {
  req(data())
  dat.nam <- data()
  a <- input$catVars
  dat.nam <- dat.nam[a]

  trt.names <- unique(dat.nam[, 1]) %>% as.list()

  choices <- trt.names[!is.na(trt.names)]

  updateSelectInput(session, "repeated_category", choices = choices, selected = choices)
})



output$info_box_repeated_anova_selection <- renderUI({
  HTML("
    <p align='justify'>
      This section enables you to perform a repeated measures ANOVA analysis for all metabolites, providing insights for selecting metabolites for further analysis. Metabolites are ranked based on their p-values, with lower p-values indicating a higher rank.
    </p>
    <p align='justify'>
      Users receive p-values for:
      <ul>
        <li>The within-subjects factor 'Time'</li>
      </ul>
    </p>
    <p align='justify'>
      <u>Used Packages and Additional Information:</u><br>
      <ul>
        <li>Kassambara A (2023). <em>rstatix: Pipe-Friendly Framework for Basic Statistical Tests</em>. R package version 0.7.2. Available at: <a href='https://CRAN.R-project.org/package=rstatix' target='_blank'>https://CRAN.R-project.org/package=rstatix</a></li>
        <li><a href='https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/' target='_blank'>Tutorial on Repeated Measures ANOVA</a></li>
      </ul>
    </p>
  ")
})

output$report_repeated_anova_selection <- downloadHandler(
  filename = "report_repeated_anova_selection.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("~/PycharmProjects/meteor_github/inst/my_app/server/repeated_measures_anova_report_feature_selection.Rmd", tempReport, overwrite = TRUE)

    params <- list(repeated.anova.feat.selection_RMD = repeated.anova.feat.selection, input_RMD = input)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)






