##### Line Plot ####

lineplot_stored <- reactiveValues(
  computation_done_plot2 = FALSE, computed_data_plot2 = NULL,
  computation_done_plot3 = FALSE, computed_data_plot3 = NULL,
  computation_done_plot4 = FALSE, computed_data_plot4 = NULL
)



observeEvent(input$lp_compute2, {
  a <- catv()
  vars <- c("id", "time", a, "metabolites", "values")

  lineplot_stored$computed_data_plot2 <- data() %>%
    filter(metabolites %in% isolate(ui_metabolites$selection))

  lineplot_stored$computation_done_plot2 <- TRUE
})


output$info_box_il <- renderUI({
  HTML("<p align='justify'>
        Line plots can be useful for visualizing the distribution of a small set of data or showing patterns over time. On the x-axis are the time points for each metabolite. The lines are colored by the selected category.<br><br>

        <u>When to use:</u><br>
        This tool is ideal for analyzing and visualizing the temporal distribution and patterns of a small set of data points, with lines representing different categories.<br><br>

        <u>Additional Information:</u><br>
        Line plots help in understanding trends and changes in data over time, making it easier to identify significant patterns and anomalies within specific categories.
      </p>")
})

output$info_box_gl <- renderUI({
  HTML("<p align='justify'>
        Line plots to visualize the mean metabolite course of a category group. On the x-axis are the time points for each metabolite. The lines are colored by the selected category.<br><br>

        <u>When to use:</u><br>
        This tool is useful for visualizing the average trend of metabolite data within different category groups over time, helping to compare the overall behavior across categories.<br><br>

        <u>Additional Information:</u><br>
        By showing the mean metabolite course, these plots facilitate the comparison of group-level trends and patterns, aiding in the identification of group-specific behaviors.
      </p>")
})

output$info_box_ml <- renderUI({
  HTML("<p align='justify'>
        Line plots to visualize the mean metabolite course. On the x-axis are the time points for each metabolite. The lines are colored by the selected metabolite.<br><br>

        <u>When to use:</u><br>
        This tool is best for visualizing the overall average trend of individual metabolites over time, providing insights into the general course of each metabolite.<br><br>

        <u>Additional Information:</u><br>
        These plots help in understanding the average behavior of metabolites, making it easier to spot trends, patterns, and potential anomalies within the data set.
      </p>")
})


output$plot2 <- renderPlotly({
  if (lineplot_stored$computation_done_plot2) {
    if (nrow(lineplot_stored$computed_data_plot2) == 0) {
      show_alert(
        title = NULL,
        text = tags$span(
          tags$h3("Error",
            style = "color: steelblue;"
          ),
          "No metabolite selected!"
        ),
        html = TRUE
      )
      return()
    }


    p <- ggplot(data = lineplot_stored$computed_data_plot2, aes(x = as.factor(time), y = values, group = factor(id))) +
      geom_line(aes(color = factor(!!sym(input$catVars))), alpha = 0.8) +
      theme_apa() +
      labs(color = input$catVars) +
      xlab("time") +
      facet_wrap(~metabolites) +
      theme(
        panel.background = element_rect(
          fill = "transparent",
          colour = NA_character_
        ), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(
          fill = "transparent",
          colour = NA_character_
        ), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent")
      )

    ggplotly(p)
  }
})

output$report_lp_individuals <- downloadHandler(
  filename = "report_lp_individuals.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(file.path("server", "line_plot_individuals_report.Rmd"), tempReport, overwrite = TRUE)

    params <- list(lineplot_stored_RMD = lineplot_stored, input_RMD = input)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



observeEvent(input$lp_compute3, {
  a <- catv()
  vars <- c("id", "time", a, "metabolites", "values")

  lineplot_stored$computed_data_plot3 <- data() %>%
    filter(metabolites %in% ui_metabolites$selection)

  lineplot_stored$computation_done_plot3 <- TRUE
})
output$plot3 <- renderPlotly({
  if (lineplot_stored$computation_done_plot3) {
    if (nrow(lineplot_stored$computed_data_plot3) == 0) {
      show_alert(
        title = NULL,
        text = tags$span(
          tags$h3("Error",
            style = "color: steelblue;"
          ),
          "No metabolite selected!"
        ),
        html = TRUE
      )
      return()
    }



    p <- ggplot(data = lineplot_stored$computed_data_plot3, aes(x = as.factor(time), y = values, color = factor(!!sym(input$catVars)))) +
      stat_summary(aes(group = factor(!!sym(input$catVars))), fun = mean, geom = "line") +
      theme_apa() +
      labs(color = input$catVars) +
      xlab("time") +
      facet_wrap(~metabolites) +
      theme(
        panel.background = element_rect(
          fill = "transparent",
          colour = NA_character_
        ), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(
          fill = "transparent",
          colour = NA_character_
        ), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent")
      )


    ggplotly(p)
  }
})

output$report_lp_groups <- downloadHandler(
  filename = "report_lp_groups.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(file.path("server", "line_plot_groups_report.Rmd"), tempReport, overwrite = TRUE)

    params <- list(lineplot_stored_RMD = lineplot_stored, input_RMD = input)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)




observeEvent(input$lp_compute4, {
  a <- catv()

  vars <- c("id", "time", a, "metabolites", "values")
  var <- c(a)

  lineplot_stored$computed_data_plot4 <- data() %>%
    filter(metabolites %in% ui_metabolites$selection)

  lineplot_stored$computation_done_plot4 <- TRUE
})

output$plot4 <- renderPlotly({
  if (lineplot_stored$computation_done_plot4) {
    if (nrow(lineplot_stored$computed_data_plot4) == 0) {
      show_alert(
        title = NULL,
        text = tags$span(
          tags$h3("Error",
            style = "color: steelblue;"
          ),
          "No metabolite selected!"
        ),
        html = TRUE
      )
      return()
    }


    p <- ggplot(data = lineplot_stored$computed_data_plot4, aes(x = as.factor(time), y = values, color = metabolites)) +
      stat_summary(aes(group = metabolites), fun = mean, geom = "line") +
      theme_apa() +
      xlab("time") +
      # labs(color = metabolites) +
      theme(
        panel.background = element_rect(
          fill = "transparent",
          colour = NA_character_
        ), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(
          fill = "transparent",
          colour = NA_character_
        ), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent")
      )


    ggplotly(p)
  }
})


output$report_lp_mean <- downloadHandler(
  filename = "report_lp_mean.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(file.path("server", "line_plot_mean_report.Rmd"), tempReport, overwrite = TRUE)

    params <- list(lineplot_stored_RMD = lineplot_stored, input_RMD = input)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

