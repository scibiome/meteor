# Friedman test
# https://www.datanovia.com/en/lessons/friedman-test-in-r/

output$info_box_friedman <- renderUI({
  HTML("<p align = 'justify'>
       In this section, the Friedman test can be applied. This non-parametric test is suitable for the study of paired samples.
       Kendall's coefficient of concordance (W) is given as the effect size.
       Pairwise comparisons of the measurement times are carried out with the Wilcoxon test and, in the case of significant pairwise comparisons, are shown in the box plot.
       <p></p>
       The metabolite to be tested can be selected via the dropdown menu.
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
