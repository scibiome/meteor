#### Ridge Plot ####

output$info_box_ridge1<- renderUI({
  HTML("<p align = 'justify'>
       This section presents ridge plots contrasting the distribution of selected metabolites (see: Select Metabolites:)
       at different measurement time points. The measurement times to be compared can be selected in the menu below this box.")
})





observe({
      req(data())
      dat.nam <- data()
      time.names <- unique(dat.nam[,2]) %>% as.numeric() %>% sort()

      updateSelectInput(session, "time.names", choices = time.names, selected = time.names)
})

# variable that stores the metabolites stores in the UI
ui_metabolites <<- reactiveValues()

observe({
      req(data())
      dat.nam <- data()
      metabolite.names <- unique(dat.nam[,3]) %>% as.list()
      delay(5000)
      updateMultiInput(session, "id", choices = metabolite.names, selected = ui_metabolites$selection)
})

observe({
      req(data())
      dat.nam <- data()
      metabolite.names <- unique(dat.nam[,3]) %>% as.list()
      delay(5000)
      updateMultiInput(session, "id2", choices = metabolite.names, selected = ui_metabolites$selection)
})

autoInvalidate <- reactiveTimer(500)

tab_switch_allowed <- TRUE

observe({
  # Invalidate and re-execute this reactive expression every time the
  # timer fires.
  autoInvalidate()
  tab_switch_allowed <<- TRUE
  # Do something each time this is invalidated.
  # The isolate() makes this observer _not_ get invalidated and re-executed
  # when input$n changes.
  # print(paste("hier oben", tab_switch_allowed))
})


last_action_id_stored <- "first_placeholder_id"

observeEvent(input$metabolite_Picker, {

  # mobse

  # req(input$changed)
  ui_metabolites$selection <- input$metabolite_Picker

}, ignoreInit = TRUE)

feature_importance_stored <- c()


ridgeplot_stored <- reactiveValues(computation_done_plot1 = FALSE, computed_data_plot1 = NULL,
                                   computation_done_plot2 = FALSE, computed_data_plot2 = NULL)

observeEvent(input$rp_compute, {

    ridgeplot_stored$computed_data_plot1 <- data() %>%
    dplyr::select(time, catv(), metabolites, values) %>%
    mutate(time = as.factor(time)) %>%
    filter(metabolites %in% ui_metabolites$selection) %>%
    filter(time %in% input$time.names) %>%
    filter(!!sym(catv()) %in% input$trtmt)


  ridgeplot_stored$computation_done_plot1 <- TRUE
})


output$ridgeplot1 <- renderPlot({
  if(ridgeplot_stored$computation_done_plot1)
  {
    ggplot(data= ridgeplot_stored$computed_data_plot1, aes(x = values, y = metabolites, fill = time)) +
    geom_density_ridges(alpha = 0.3, scale = 1) +
    theme_ridges() +
          theme(
            panel.background = element_rect(fill = "#edeff4",
                                            colour = NA_character_), # necessary to avoid drawing panel outline
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            plot.background = element_rect(fill = "#edeff4",
                                           colour = NA_character_), # necessary to avoid drawing plot outline
            legend.background = element_rect(fill = "#edeff4"),
            legend.box.background = element_rect(fill = "#edeff4"),
            legend.key = element_rect(fill = "#edeff4")
          )
  }}, bg = '#edeff4')

output$info_box_ridge2<- renderUI({
  HTML("<p align = 'justify'>
       This section also allows for the comparison of metabolites at different timepoints using ridge plots.
       The ridge plots illustrate the distribution of selected metabolites at different measurement time points,
       which can be selected in the menu provided below this section.
       This comparison enables a visual representation of changes in metabolite levels over time,
       which can provide valuable insights into the metabolic processes occurring in the system under study.")
})

observeEvent(input$rp_compute2, {


  ridgeplot_stored$computed_data_plot2 <- data() %>%
    dplyr::select(time, catv(), metabolites, values) %>%
    mutate(time = as.factor(time)) %>%
    filter(metabolites %in% ui_metabolites$selection) %>%
    filter(time %in% input$timepoint) %>%
    filter(!!sym(catv()) %in% input$trtmt) %>%
    mutate(!!sym(input$catVars) := factor(!!sym(input$catVars)))

  ridgeplot_stored$computation_done_plot2 <- TRUE
})


output$ridgeplot2 <- renderPlot({
  if(ridgeplot_stored$computation_done_plot2)
  {
      ggplot(data= ridgeplot_stored$computed_data_plot2, aes(x = values, y = metabolites, fill = !!sym(catv()))) +
      geom_density_ridges(alpha = 0.3, scale = 1) +
      theme_ridges() +
            theme(
              panel.background = element_rect(fill = "#edeff4",
                                              colour = NA_character_), # necessary to avoid drawing panel outline
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank(), # get rid of minor grid
              plot.background = element_rect(fill = "#edeff4",
                                             colour = NA_character_), # necessary to avoid drawing plot outline
              legend.background = element_rect(fill = "#edeff4"),
              legend.box.background = element_rect(fill = "#edeff4"),
              legend.key = element_rect(fill = "#edeff4")
            )
  }}, bg = '#edeff4')
