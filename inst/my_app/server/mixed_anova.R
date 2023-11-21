# Mixed Anova
# https://www.datanovia.com/en/lessons/mixed-anova-in-r/


output$info_box_mixed_anova <- renderUI({
  HTML("<p align = 'justify'>
This section enables you to perform a mixed ANOVA analysis for a specific metabolite,
using a between-subjects factor (selected categorical variable) and a within-subjects factor (time).
The first table presents summary statistics for each measurement time point,
while the ANOVA results box provides information on the significance and
effect size (generalized eta squared) of the between factor, within factor, and their interaction.
<p></p>
In addition, a box plot is displayed in the upper left corner to illustrate the results for the between-within factors interactions.
The x-axis represents the measurement time point, and the color of the box plot indicates group membership.
Between-group pairwise comparisons are conducted using t-tests for dependent samples,
and multiple testing is corrected with Bonferroni.
  ")
})


observe({
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[, 3]) %>% as.list()

  updateMultiInput(session, "id10", choices = metabolite.names, selected = metabolite.names)
})

reactive_mixed_anova <- reactiveValues(
  computation_done = FALSE,
  data.filtered = NULL,
  summary_stats = NULL,
  res_mixed_anova = NULL,
  boxplot = NULL,
  pairwise = NULL,
  pairwise.filtered = NULL
)



observeEvent(input$act_mixed_anova, {

  data.filtered <- data() %>%
    filter(metabolites %in% input$id10)



  summary_stats <- data.filtered %>%
    group_by(time, input$catVars) %>%
    rstatix::get_summary_stats(values, type = "mean_sd")


  colnames(summary_stats) <- c("Time", "Category", "Variable", "N", "Mean", "SD")





  res_mixed_anova <- data.filtered %>%
    rstatix::anova_test(
      data = ., dv = values, wid = id,
      between = input$catVars, within = time
    ) %>%
    get_anova_table()

  form.test <- paste0("values~ ", input$catVars)


  pairwise <- data.filtered %>%
    group_by(time) %>%
    pairwise_t_test(as.formula(form.test), p.adjust.method = "bonferroni")
  pairwise


  # Visualization: boxplots with p-values
  pairwise <- pairwise %>% add_xy_position(x = "time")

  pairwise.filtered <- pairwise %>% filter(time != as.character(pairwise[1, 1]))

  boxplot <- data.filtered %>%
    as.data.frame() %>%
    mutate(catVars = as.factor(input$catVars)) %>%
    ggboxplot(.,
      x = "time", y = "values",
      color = input$catVars, palette = "jco"
    ) +
    stat_pvalue_manual(pairwise.filtered, tip.length = 0, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(res_mixed_anova, detailed = TRUE),
      caption = get_pwc_label(pairwise)
    )

  reactive_mixed_anova$summary_stats <- summary_stats
  reactive_mixed_anova$res_mixed_anova <- res_mixed_anova
  reactive_mixed_anova$boxplot <- boxplot
  reactive_mixed_anova$data.filtered <- data.filtered
  reactive_mixed_anova$pairwise <- pairwise
  reactive_mixed_anova$pairwise.filtered <- pairwise.filtered
  reactive_mixed_anova$computation_done <- TRUE
})


output$summary_stats_mixed_anova <- renderDT(
datatable(reactive_mixed_anova$summary_stats,
  caption = "Summary statistics",
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = TRUE
  )
))



output$res_mixed_anova <- renderPrint({
  reactive_mixed_anova$res_mixed_anova
})

output$boxplot_mixed_anova <- renderPlot({

  # TODO check if there are more than onvalue for colors
  if (reactive_mixed_anova$computation_done){
      boxplot <- reactive_mixed_anova$data.filtered %>%
    as.data.frame() %>%
    mutate(!!sym(input$catVars) := factor(!!sym(input$catVars))) %>%
    ggboxplot(.,
      x = "time", y = "values",
      color = input$catVars, palette = "jco", fill="#edeff4", size=2
    ) +
    stat_pvalue_manual(reactive_mixed_anova$pairwise.filtered, tip.length = 0, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(reactive_mixed_anova$res_mixed_anova, detailed = TRUE),
      caption = get_pwc_label(reactive_mixed_anova$pairwise)
    ) + theme(
      text = element_text(size = 16),
      panel.background = element_rect(fill = "#edeff4",
                                      colour = NA_character_), # necessary to avoid drawing panel outline
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      plot.background = element_rect(fill = "#edeff4",
                                     colour = NA_character_), # necessary to avoid drawing plot outline
      legend.background = element_rect(fill = "#edeff4"),
      # legend.box.background = element_rect(fill = "#edeff4"),
      # legend.key = element_rect(fill = "#edeff4")
    )
  return(boxplot)
  }

}, bg = '#edeff4')


##### Mixed ANOVA for feature selection ######


mixed.anova.feat.selection <- reactiveValues(ranking = NULL, ranking.sorted = NULL)


observe({
  # Get unique metabolites
  req(data())
  dat.nam <- data()
  metabolites <- unique(dat.nam[, 3]) %>% as.character()
  m.length <- length(metabolites)
  choices <- seq(0, m.length, 10)
  updateSelectInput(session, "rank.p", choices = choices, selected = 10)
})



observeEvent(input$act_mixed_anova_selection, {
  paste0("running anova")




  # Get unique metabolites
  req(data())
  dat.nam <- data()
  metabolites <- unique(dat.nam[, 3]) %>% as.character()

  # Create an empty data frame for ranking
  ranking <- data.frame(metabolites = metabolites, time.p.value = NA, categorical.p.value = NA, timexcategorical.p.value = NA)

  # Iterate over metabolites and calculate ANOVA p-value


  data_no_na <- data()
  data_no_na2 <- data_no_na[!is.na(data_no_na[input$catVars]), ]

  # need to check if for the category there are at all time points at least two groups
  for (t in unique(data_no_na2$time)){
    temp <- data_no_na2 %>% filter(time %in% c(t))
    unique_values_in_cat <- unique(temp[input$catVars])

    if (nrow(unique_values_in_cat) < 2) {
      show_alert(
        title = NULL,
        text = tags$span(
          tags$h3("Error",
                  style = "color: steelblue;"),
          "not enough samples with this category at all time points"
        ),
        html = TRUE
      )
      return()
    }
  }


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
        between = cat,
        within = time
      ) %>%
      get_anova_table()
    return(c(i,
             anova.res[1, 5],
             anova.res[2, 5],
             anova.res[3, 5]))
  }

  # Apply the function to each metabolite in parallel
  cat <- input$catVars
  results <- foreach(i = metabolites, .combine = rbind) %dopar% {
    anova_func(i, data_no_na2, cat)
  }

  # Convert the results to a data frame
  colnames(results) <- c("metabolites", "categorical.p.value", "time.p.value", "timexcategorical.p.value")
  rownames(results) <- 1:nrow(results)
  ranking <- as.data.frame(results)

  # Stop the parallel backend
  stopCluster(cl)

  unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  unregister()



  mixed.anova.feat.selection$ranking <- ranking

  if (input$selectfactor == "Categorical") {

    ranking.sorted <- ranking %>%
                                 select(metabolites, categorical.p.value) %>%
                                 mutate(categorical.p.value = as.numeric(categorical.p.value)) %>%
                                 arrange(categorical.p.value)



  } else if (input$selectfactor == "Time") {

    ranking.sorted <- ranking %>%
                        select(metabolites, time.p.value) %>%
                        mutate(time.p.value = as.numeric(time.p.value)) %>%
                        arrange(time.p.value)


  } else if (input$selectfactor == "Time x Categorical") {

    ranking.sorted <- ranking %>%
                          select(metabolites, timexcategorical.p.value) %>%
                          mutate(timexcategorical.p.value = as.numeric(timexcategorical.p.value)) %>%
                          arrange(timexcategorical.p.value)

  }

  mixed.anova.feat.selection$ranking.sorted <- ranking.sorted



  })


output$ranking.sorted <- DT::renderDT(datatable(mixed.anova.feat.selection$ranking.sorted,
                                                   caption = "Ranked metabolites by p-values",
                                                   extensions = "Buttons",
                                                   options = list(
                                                     searching = FALSE,
                                                     lengthChange = FALSE,
                                                     paging = TRUE,
                                                     dom = 'Bfrtip',
                                                     buttons = c("copy", "print", "csv", "excel")
                                                   )
))

observeEvent(input$load_top_features_anova, {

  # mobse
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[,3]) %>% as.list()

  ui_metabolites$selection <- c(ui_metabolites$selection, head(mixed.anova.feat.selection$ranking.sorted$metabolites, 10))

  updatePickerInput(session, inputId = "metabolite_Picker", choices = metabolite.names, selected=ui_metabolites$selection,
                    choicesOpt = list(
                      style = c(rep("color: black;", length(metabolite.names))))
  )

}, ignoreInit = TRUE)


output$info_box_mixed_anova_selection <- renderUI({
  HTML("<p align = 'justify'>
      In this section, a mixed ANOVA is computed for all metabolites,
      providing results that can be used to select metabolites for further processing.
      Metabolites are ranked based on their p-values, with lower p-values indicating a higher rank.
      Users can choose to select p-values for the between factor 'Categorical',
      the within factor 'time', or the interaction 'Time x Categorical'.
      Additionally, the number of metabolites to be included in the ranking can be specified.
       ")
})





