
output$info_box_linear_mixed_model <- renderUI({
  HTML("<p align = 'justify'>
This section allows you to estimate a linear mixed model (LMM) for a single metabolite,
using the subject ID as the random factor and the selected categorical variable and time as fixed factors.
Users can choose between three different model types (random intercept, random slope, and random slope + intercept)
based on their research question. The LMM equation in R is displayed in the first box,
while the box below presents a summary of the results for the LMM.
<p></p>
On the right side, a line plot displays the individual courses of the subjects,
colored according to the group to which they belong.
On the left side, the estimated model is shown with regression lines for each subject.
The bottom figure displays the fitted models for a random selection of individuals.
  ")
})


output$info_box_linear_mixed_model_comp <- renderUI({
  HTML("<p align = 'justify'>
This section enables you to compare different linear mixed models (LMMs),
including the random intercept, random slope, and random intercept + slope models.
The models are displayed in a comparison table on the right side of the section.
ANOVA tests are conducted to determine significant differences between the models.
A common question that arises is whether the complex model is superior to the parsimonious model,
in terms of model fit and explanatory power.
  ")
})



output$info_box_LMM <- renderUI({
  HTML("<p align = 'justify'> Text for LMM <p>")
})

output$info_box_comparison <- renderUI({
  HTML("<p align = 'justify'> Text comparison <p>")
})

observe({

  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[,3]) %>% as.list()

  updateMultiInput(session, "id9", choices = metabolite.names, selected = metabolite.names)
})


lmm.res <- reactiveValues(result = NULL,
                          vis_random = NULL,
                          vis_grid = NULL,
                          line_plot = NULL,
                          lmm_model = NULL,
                          model.code = NULL,
                          res.anova = NULL,
                          multi_tab = NULL,
                          check.mod = NULL)

stored_lm <- reactiveValues(lm1 = NULL, lm2=NULL, lm3=NULL)

observeEvent(input$act_lmm,{


  data.filtered <- data()%>% filter(metabolites %in% input$id9)  %>% filter(!is.na(!!sym(input$catVars)))

      if (input$lmm_select %in% "Random intercept + slope") {

            if(length(unique(data.filtered$id)) * 2 > nrow(data.filtered)) {
              # TODO add some output here that the model is not working, it has to many random effects
              show_alert(
                title = NULL,
                text = tags$span(
                  tags$h3("Error",
                          style = "color: steelblue;"),
                  "Not enough observations"
                ),
                html = TRUE
              )
              return()
            }

            formula_text <- paste("values ~ ","time"," + ",input$catVars," + ", "time:", input$catVars, " + (1 + time | id)")
            formula <- as.formula(formula_text)
            mixed.lm <- lmer(formula, data = data.filtered)
            data.filtered$fit <- predict(mixed.lm)

            lmm.res$check.mod <- mixed.lm

            lmm.res$model.code <- paste0("lmer(values ~ time + ",input$catVars," + time:",input$catVars," +  (1 + time | id), data = data.filtered)")


      } else if (input$lmm_select %in% "Random intercept" ) {

            formula_text <- paste("values ~ ","time"," + ",input$catVars," + ", "time:", input$catVars, " + (1 | id)")
            formula <- as.formula(formula_text)
            mixed.lm <- lmer(formula, data = data.filtered)
            data.filtered$fit <- predict(mixed.lm)

            lmm.res$check.mod <- mixed.lm

            lmm.res$model.code <- paste0("lmer(values ~ time + ",input$catVars," + time:",input$catVars," +  (1 | id), data = data.filtered)")

      } else if (input$lmm_select %in% "Random slope") {

            formula_text <- paste("values ~ ","time"," + ",input$catVars," + ", "time:", input$catVars, " + (0 + time | id)")
            formula <- as.formula(formula_text)
            mixed.lm <- lmer(formula, data = data.filtered)
            lmm.res$check.mod <- mixed.lm
            data.filtered$fit <- predict(mixed.lm)
            a <- predict(mixed.lm)
            lmm.res$model.code <- paste0("lmer(values ~ time + ",input$catVars," + time:",input$catVars," + (0 + time | id), data = data.filtered)")

      } else {print("Nothing selected!")}


  lmm.res$result <- HTML(tab_model(mixed.lm)$knitr)
  c <- 1
  # sub_samp <- sample(data.filtered$id, 10)
  #
  # lmm.res$vis_grid <- data.filtered %>%
  #                       filter(id %in% sub_samp) %>%
  #                            ggplot(.,aes(time, values, colour = factor(!!sym(input$catVars)))) +
  #                               labs(color = input$catVars) +
  #                               facet_wrap(~id) +
  #                               geom_line(aes(y=fit), size=0.8) +
  #                               geom_point(alpha = 0.3) +
  #                               geom_hline(yintercept=0, linetype="dashed") +
  #                                   theme_apa() +
  #                                   theme(
  #                                     panel.background = element_rect(fill = "transparent",
  #                                                                     colour = NA_character_), # necessary to avoid drawing panel outline
  #                                     panel.grid.major = element_blank(), # get rid of major grid
  #                                     panel.grid.minor = element_blank(), # get rid of minor grid
  #                                     plot.background = element_rect(fill = "transparent",
  #                                                                    colour = NA_character_), # necessary to avoid drawing plot outline
  #                                     legend.background = element_rect(fill = "transparent"),
  #                                     legend.box.background = element_rect(fill = "transparent"),
  #                                     legend.key = element_rect(fill = "transparent")
  #                                   )

  lmm.res$lmm_model <- ggplot(data.filtered, aes(x=time, y=values,  color = factor(!!sym(input$catVars)) ,group=factor(id)))+
                                labs(color = input$catVars) +
                                geom_point(size=1) +
                                geom_line(aes(y=fit, color = factor(!!sym(input$catVars)))) +
                                    theme_apa() +
                                    ggtitle("Fitted model") +
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

  lmm.res$line_plot <- ggplot(data.filtered, aes(x=time, y=values,  color = factor(!!sym(input$catVars)), group=factor(id))) +
                                labs(color = input$catVars) +
                                geom_point(size=1) +
                                geom_line() +
                                  theme_apa() +
                                  ggtitle("Line plot") +
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


  ##### Model comparison ####
  observeEvent(input$act_lmm2,{


    data.filtered <- data() %>%
                     filter(metabolites %in% input$id9)

    formula.lm1 <- paste("values ~ ","time"," + ",input$catVars," + ", "time:", input$catVars, " + (1 | id)")
    formula.lm2 <- paste("values ~ ","time"," + ",input$catVars," + ", "time:", input$catVars, " + (0 + time | id)")


    mixed.lm1 <- lmer(formula.lm1, data = data.filtered)
    mixed.lm2 <- lmer(formula.lm2, data = data.filtered)

    # need the check that we don't have too many random effects
    if(length(unique(data.filtered$id)) * 2 < nrow(data.filtered)) {
      formula.lm3 <- paste("values ~ ","time"," + ",input$catVars," + ", "time:", input$catVars, " + (1 + time | id)")
      mixed.lm3 <- lmer(formula.lm3, data = data.filtered)

      lmm.res$res.anova <- anova(mixed.lm1, mixed.lm2, mixed.lm3)
      lmm.res$multi_tab <- HTML(tab_model(mixed.lm1, mixed.lm2, mixed.lm3)$knitr)
    } else{
      lmm.res$res.anova <- anova(mixed.lm1, mixed.lm2)
      lmm.res$multi_tab <- HTML(tab_model(mixed.lm1, mixed.lm2)$knitr)
    }


  })





  output$model.code <- renderText({paste0("Model: ", lmm.res$model.code)})
  output$lmm_model <- renderPlotly({ggplotly(lmm.res$lmm_model)})
  output$line_plot <- renderPlotly({ggplotly(lmm.res$line_plot)})
  output$result.single <- renderUI({lmm.res$result})
  #output$vis_grid <- renderPlotly({ggplotly(lmm.res$vis_grid)})
  output$res.anova <- renderPrint({lmm.res$res.anova})
  output$multi_tab <- renderUI({lmm.res$multi_tab})
  output$check.mod.lmm <- renderPlot({
   # browser()
    check_model(lmm.res$check.mod, base_size = 13.5)})

})

