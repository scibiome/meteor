


output$info_box_linear_mixed_model <- renderUI({
  HTML("
    <p align='justify'>
      This section allows you to estimate a linear mixed model (LMM) using the <i>lme4</i> package for a single metabolite, utilizing the subject ID as the random factor and the selected categorical variable and time as fixed factors. Users can choose between three different model types: random intercept, random slope, and random slope + intercept, based on their research question.
    </p>
    <h4>Model Assumptions</h4>
    <p align='justify'>
      The <i>performance</i> package from the <i>easystats</i> R framework is used to check various assumptions for the selected linear mixed model:
    </p>
    <ul>
      <li>Posterior predictive checks: Evaluate whether the model fits the data well.</li>
      <li>Linearity: Assess whether predictors have a linear relationship with the outcome.</li>
      <li>Homogeneity of variance: Check the assumption of equal variance.</li>
      <li>Influential observations: Identify observations significantly affecting model parameter estimation.</li>
      <li>Multicollinearity: Detect potential collinearity among predictors.</li>
      <li>Normality of residuals: Ensure residuals are normally distributed.</li>
    </ul>
    <h4>LMM Equation</h4>
    <p align='justify'>
      The LMM equation in R is displayed in the second box.
    </p>
    <h4>Model Summary</h4>
    <p align='justify'>
      The box below presents a summary of the LMM results generated with the <i>sjPlot</i> package.
    </p>
    <h4>Visual Representation</h4>
    <p align='justify'>
      <ul>
        <li>A line plot displays the individual courses of the subjects, colored according to their group.</li>
        <li>The estimated model is shown with regression lines for each subject on the left side.</li>
      </ul>
    </p>
    <br><br>
    <u>When to Use:</u><br>
    <p align='justify'>
      Linear mixed models are suitable when:
    </p>
    <ul>
      <li>There is a need to account for both fixed and random effects.</li>
      <li>Data is hierarchical or nested (e.g., repeated measures for the same subjects).</li>
      <li>There is a need to control for individual differences or variability among subjects.</li>
    </ul>
    <br><br>
    <u>Used Packages and Additional Information:</u><br>
    <ul>
      <li>Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.</li>
      <li>Lüdecke et al. (2021). performance: An R Package for Assessment, Comparison and Testing of Statistical Models. Journal of Open Source Software, 6(60), 3139. <a href='https://doi.org/10.21105/joss.03139' target='_blank'>https://doi.org/10.21105/joss.03139</a></li>
      <li>Lüdecke D (2024). <em>sjPlot: Data Visualization for Statistics in Social Science</em>. R package version 2.8.16. Available at: <a href='https://CRAN.R-project.org/package=sjPlot' target='_blank'>https://CRAN.R-project.org/package=sjPlot</a></li>
      <li><a href='https://easystats.github.io/performance/articles/check_model.html' target='_blank'>Tutorial on checking model assumptions</a></li>
      <li><a href='https://strengejacke.github.io/sjPlot/articles/tab_mixed.html' target='_blank'>Tutorial on Summary of Mixed Models</a></li>
    </ul>
  ")
})



output$info_box_linear_mixed_model_comp <- renderUI({
  HTML("
    <p align='justify'>
      This section enables you to compare different linear mixed models (LMMs), including random intercept, random slope, and random intercept + slope models.
    </p>
    <h4>Model Comparison</h4>
    <p align='justify'>
      The models are displayed in a comparison table on the right side of the section, providing an overview of their respective fit statistics and performance metrics.
    </p>
    <h4>ANOVA Tests</h4>
    <p align='justify'>
      ANOVA tests are conducted to determine significant differences between the models. These tests help identify whether the more complex model provides a significantly better fit compared to a simpler one.
    </p>
    <h4>Model Selection</h4>
    <p align='justify'>
      A common question is whether the complex model is superior to the parsimonious model in terms of model fit and explanatory power. This section provides tools to assess the trade-off between model complexity and fit, guiding users in selecting the most appropriate model for their data.
    </p>
    <br><br>
    <u>When to Use:</u><br>
    <p align='justify'>
      Model comparison is suitable when:
    </p>
    <ul>
      <li>There are multiple candidate models for explaining the data.</li>
      <li>There's a need to balance model complexity with explanatory power.</li>
      <li>Determining the best-fitting model requires statistical tests and fit comparisons.</li>
    </ul>
    <br><br>
    <u>Used Packages and Additional Information:</u><br>
    <ul>
      <li>Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.</li>
      <li>Lüdecke D (2024). <em>sjPlot: Data Visualization for Statistics in Social Science</em>. R package version 2.8.16. Available at: <a href='https://CRAN.R-project.org/package=sjPlot' target='_blank'>https://CRAN.R-project.org/package=sjPlot</a></li>
      <li><a href='https://strengejacke.github.io/sjPlot/articles/tab_mixed.html' target='_blank'>Tutorial on Summary of Mixed Models</a></li>
    </ul>
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
                          check.mod = NULL,
                          data.filtered = NULL,
                          check_model_finished = NULL)

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

  lmm.res$data.filtered <- data.filtered
  lmm.res$check_model_finished <- check_model(lmm.res$check.mod, base_size = 13.5)


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
output$check.mod.lmm <- renderPlot({check_model(lmm.res$check.mod, base_size = 13.5)})
output$model.code <- renderText({paste0("Model: ", lmm.res$model.code)})
output$result.single <- renderUI({lmm.res$result})

output$report_linear_mixed_model <- downloadHandler(
  filename = "report_linear_mixed_model.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("~/PycharmProjects/meteor_github/inst/my_app/server/linear_mixed_model_report.Rmd", tempReport, overwrite = TRUE)

    params <- list(input_RMD = input, lmm.res_RMD = lmm.res)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
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






  output$lmm_model <- renderPlotly({ggplotly(lmm.res$lmm_model)})
  output$line_plot <- renderPlotly({ggplotly(lmm.res$line_plot)})

  #output$vis_grid <- renderPlotly({ggplotly(lmm.res$vis_grid)})
  output$res.anova <- renderPrint({lmm.res$res.anova})
  output$multi_tab <- renderUI({lmm.res$multi_tab})

})

