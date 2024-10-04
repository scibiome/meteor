# Friedman test
# https://www.datanovia.com/en/lessons/friedman-test-in-r/


output$info_box_friedman <- renderUI({
  HTML("
    <p align='justify'>
      In this section, the Friedman test can be applied. This non-parametric test is suitable for the study of paired samples.
    </p>
    <h4>Summary Statistics</h4>
    <p align='justify'>
      The first table provides summary statistics for the individual time points.
    </p>
    <h4>Friedman Test Results</h4>
    <p align='justify'>
      The second table on the right side provides the results of the Friedman test.
    </p>
    <h4>Effect Size</h4>
    <p align='justify'>
      The third table on the left provides information about the effect size (Kendall's coefficient of concordance (W)).
    </p>
    <h4>Visual Representation</h4>
    <p align='justify'>
      A box plot is displayed to illustrate the trend over different time points of measurement. Each box plot represents a distribution for a given time point, with the x-axis representing the measurement time points and the y-axis representing the metabolite abundance levels.
    </p>
    <h4>Pairwise Comparisons</h4>
    <p align='justify'>
      Pairwise comparisons of the measurement times are carried out with the Wilcoxon test and, in the case of significant pairwise comparisons, are shown in the box plot.
    </p>
    <p align='justify'>
      The metabolite to be tested can be selected via the dropdown menu.
    </p>
    <br><br>
        <u>When to Use:</u><br>
    <p align='justify'>
      The Friedman test is suitable for:
    </p>
    <ul>
      <li>Comparing multiple treatments or conditions applied to the same subjects or units.</li>
      <li>When the data violates assumptions of parametric tests like ANOVA (e.g., normality, homogeneity of variances).</li>
      <li>Paired observations or repeated measures where the same subjects are measured under different conditions or at different time points.</li>
    </ul>
    <br><br>
    <u>Used Packages and Additional Information:</u><br>
    <ul>
      <li>Kassambara A (2023). <em>rstatix: Pipe-Friendly Framework for Basic Statistical Tests</em>. R package version 0.7.2. Available at: <a href='https://CRAN.R-project.org/package=rstatix' target='_blank'>https://CRAN.R-project.org/package=rstatix</a></li>
      <li><a href='https://www.datanovia.com/en/lessons/friedman-test-in-r/' target='_blank'>Tutorial on Friedman Test</a></li>
    </ul>
  ")
})





observe({
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[, 3]) %>% as.list()

  updateMultiInput(session, "id8", choices = metabolite.names, selected = metabolite.names)
})

reactive_friedman <- reactiveValues(summary_stat = NULL, res_fried = NULL, res_fried_eff = NULL, pair.ws = NULL)

observeEvent(input$act_fried, {
  data.filtered <- data() %>%
    filter(metabolites %in% input$id8)



  patients_with_all_timepoints <- data.filtered %>%
    group_by(id) %>%
    filter(all(unique(data.filtered$time) %in% time)) %>%
    distinct(id)

  data.filtered <- semi_join(data.filtered, patients_with_all_timepoints, by = "id")

  summary_stat <- data.filtered %>%
    group_by(time) %>%
    get_summary_stats(values, type = "common") %>%
    mutate(across(where(is.numeric), ~ round(., 2)))


  colnames(summary_stat) <- c("Time", "Variable", "N", "Min", "Max", "Median", "IQR", "Mean", "SD", "SE", "CI")


  res_fried <- data.filtered %>%
    friedman_test(values ~ time | id) %>%
    mutate(across(where(is.numeric), ~ round(., 2)))

  colnames(res_fried) <- c("Variable", "N", "Statistic", "df", "p", "Method")

  res_fried_eff <- data.filtered %>%
    friedman_effsize(values ~ time | id) %>%
    mutate(across(where(is.numeric), ~ round(., 2)))

  colnames(res_fried_eff) <- c("Variable", "N", "Effect size", "Method", "Magnitude")

  pair.ws <- data.filtered %>%
    wilcox_test(values ~ time, paired = TRUE, p.adjust.method = "bonferroni")

  reactive_friedman$summary_stat <- summary_stat
  reactive_friedman$res_fried <- res_fried
  reactive_friedman$res_fried_eff <- res_fried_eff
  reactive_friedman$pair.ws <- pair.ws
})

output$summary_stat <- renderDataTable(datatable(reactive_friedman$summary_stat,
  caption = "Summary statistics",
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = TRUE
  )
))

output$res_fried <- renderDataTable(datatable(reactive_friedman$res_fried,
  caption = "Friedman test",
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = FALSE
  )
))

output$res_fried_eff <- renderDataTable(datatable(reactive_friedman$res_fried_eff,
  caption = "Kendall's W",
  options = list(
    searching = FALSE,
    lengthChange = FALSE,
    paging = FALSE
  )
))


observeEvent(input$act_fried, {
  output$boxfried <- renderPlot({
    pair.ws <- reactive_friedman$pair.ws %>% add_xy_position(x = "time")

    boxfried <- data() %>%
      filter(metabolites %in% input$id8) %>%
      ggboxplot(., x = "time", y = "values", add = "point") +
      stat_pvalue_manual(pair.ws, hide.ns = TRUE) +
      labs(
        subtitle = get_test_label(reactive_friedman$res_fried, detailed = TRUE),
        caption = get_pwc_label(pair.ws)
      ) + theme(
        text = element_text(size = 16),
        panel.background = element_rect(fill = "#edeff4",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "#edeff4",
                                       colour = NA_character_), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "#edeff4"),
      )
    return(boxfried)
  })
})


output$report_friedman <- downloadHandler(
  filename = "report_friedman.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("~/PycharmProjects/meteor_github/inst/my_app/server/friedman_report.Rmd", tempReport, overwrite = TRUE)

    params <- list(reactive_friedman_RMD = reactive_friedman, input_RMD = input, data_RMD = data())

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



