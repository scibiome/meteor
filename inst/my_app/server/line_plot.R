##### Line Plot ####

lineplot_stored <- reactiveValues(computation_done_plot2 = FALSE, computed_data_plot2 = NULL,
                                  computation_done_plot3 = FALSE, computed_data_plot3 = NULL,
                                  computation_done_plot4 = FALSE, computed_data_plot4 = NULL)



observeEvent(input$lp_compute2, {
  a <- catv()
  vars <- c("id", "time", a, "metabolites", "values")

  lineplot_stored$computed_data_plot2 <- data() %>%
    filter(metabolites %in% isolate(ui_metabolites$selection))

  lineplot_stored$computation_done_plot2 <- TRUE
})

output$info_box_il <- renderUI({
  HTML("<p align = 'justify'> Line plots can be useful for visualizing the distribution of a small set of data or
       showing patterns over time. On the x-axis are the time points, for each metabolite. The lines are colored by the selected category.  <p>")
})

output$info_box_gl <- renderUI({
  HTML("<p align = 'justify'> Line plots to visualize the mean metabolite course of a category group.
       On the x-axis are the time points, for each metabolite. The lines are colored by the selected category. <p>")
})

output$info_box_ml <- renderUI({
  HTML("<p align = 'justify'> Line plots to visualize the mean metabolite course.
       On the x-axis are the time points, for each metabolite. The lines are colored by the selected metabolite. <p>")
})

output$plot2 <- renderPlotly({
  if(lineplot_stored$computation_done_plot2)
  {
  p <- ggplot(data= lineplot_stored$computed_data_plot2, aes(x = as.factor(time), y = values, group = factor(id))) +
                  geom_line(aes(color = factor(!!sym(input$catVars))), alpha = 0.8) +
                  theme_apa() + labs(color = input$catVars) +
                  xlab("time") +
                  facet_wrap(~metabolites) +
                  theme(
                    panel.background = element_rect(fill = "transparent",
                                                    colour = NA_character_), # necessary to avoid drawing panel outline
                    panel.grid.major = element_blank(), # get rid of major grid
                    panel.grid.minor = element_blank(), # get rid of minor grid
                    plot.background = element_rect(fill = "transparent",
                                                   colour = NA_character_), # necessary to avoid drawing plot outline
                    legend.background = element_rect(fill = "transparent"),
                    legend.box.background = element_rect(fill = "transparent"),
                    legend.key = element_rect(fill = "transparent")
                  )

  ggplotly(p)

  }})

observeEvent(input$lp_compute3, {

  a <- catv()
  vars <- c("id", "time", a, "metabolites", "values")

  lineplot_stored$computed_data_plot3 <- data() %>%
    filter(metabolites %in% ui_metabolites$selection)

  lineplot_stored$computation_done_plot3 <- TRUE
})
output$plot3 <- renderPlotly({
  if(lineplot_stored$computation_done_plot3)
  {
  p <- ggplot(data= lineplot_stored$computed_data_plot3, aes(x = as.factor(time), y = values,color = factor(!!sym(input$catVars)))) +
                  stat_summary(aes(group= factor(!!sym(input$catVars))),fun=mean,geom="line") +
                  theme_apa() +
                  labs(color = input$catVars) +
                  xlab("time") +
                  facet_wrap(~metabolites) +
                  theme(
                    panel.background = element_rect(fill = "transparent",
                                                    colour = NA_character_), # necessary to avoid drawing panel outline
                    panel.grid.major = element_blank(), # get rid of major grid
                    panel.grid.minor = element_blank(), # get rid of minor grid
                    plot.background = element_rect(fill = "transparent",
                                                   colour = NA_character_), # necessary to avoid drawing plot outline
                    legend.background = element_rect(fill = "transparent"),
                    legend.box.background = element_rect(fill = "transparent"),
                    legend.key = element_rect(fill = "transparent")
                  )


  ggplotly(p)
  }

})

observeEvent(input$lp_compute4, {

  a <- catv()

  vars <- c("id", "time", a, "metabolites", "values")
  var  <- c(a)

  lineplot_stored$computed_data_plot4 <- data() %>%
    filter(metabolites %in% ui_metabolites$selection)

  lineplot_stored$computation_done_plot4 <- TRUE
})

output$plot4 <- renderPlotly({
  if(lineplot_stored$computation_done_plot4)
  {

  p <-  ggplot(data=lineplot_stored$computed_data_plot4, aes(x = as.factor(time), y = values, color = metabolites)) +
                stat_summary(aes(group=metabolites), fun=mean,geom="line") +
                theme_apa() +
                xlab("time") +
                #labs(color = metabolites) +
                theme(
                  panel.background = element_rect(fill = "transparent",
                                                  colour = NA_character_), # necessary to avoid drawing panel outline
                  panel.grid.major = element_blank(), # get rid of major grid
                  panel.grid.minor = element_blank(), # get rid of minor grid
                  plot.background = element_rect(fill = "transparent",
                                                 colour = NA_character_), # necessary to avoid drawing plot outline
                  legend.background = element_rect(fill = "transparent"),
                  legend.box.background = element_rect(fill = "transparent"),
                  legend.key = element_rect(fill = "transparent")
                )


  ggplotly(p)


}})
