
output$info_box_volcano <- renderUI({
  HTML("<p align = 'justify'> The volcano plot is a tool for visualizing the differential occurrence of metabolites. By plotting the log-fold change on the x-axis and the statistical significance on the y-axis, represented as the negative log of the p-value, it effectively reveals the distribution of metabolites that are significantly upregulated or downregulated between two conditions. This enables to identify the most significant and biologically relevant changes. The cut-off for statistical significance and for the absolute log fold-change can be specified:<p>")
})

observeEvent(input$catVars, {
  req(data())
  dat.nam <- data()
  a <- input$catVars
  dat.nam <- dat.nam[a]

  trt.names <- unique(dat.nam[, 1]) %>% as.list()

  choices <- trt.names[!is.na(trt.names)]
  updateSelectInput(session, "feature1", choices = choices, selected = choices)
  updateSelectInput(session, "feature2", choices = choices, selected = NULL)
})


# output$text2 <- renderText({
#   if (input$feature1 == input$feature2) {
#     paste("Select two different features")
#   }
# })



volcano.dat <- reactiveValues(
  vars = NULL,
  data.pre = NULL,
  form.test = NULL,
  cate.name = NULL,
  res = NULL,
  volcano = NULL
)

#################################################
observeEvent(input$load_top_features_volcano, {

  # mobse
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[,3]) %>% as.list()

  # volcano.dat$res[order(volcano.dat$res$adj.P.Val),]



  ui_metabolites$selection <- c(ui_metabolites$selection, rownames(head(volcano.dat$res, 10)))

  updatePickerInput(session, inputId = "metabolite_Picker", choices = metabolite.names, selected=ui_metabolites$selection,
                    choicesOpt = list(
                      style = c(rep("color: black;", length(metabolite.names))))
  )

}, ignoreInit = TRUE)



observeEvent(input$act_volc, {
  # Process the data
  vars <- c("id", "time", "metabolites", "values", {{ input$catVars }})

  fl1 <- input$feature1
  fl2 <- input$feature2

  if(fl1 == fl2){
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Error",
                style = "color: steelblue;"),
        "Select two different features"
      ),
      html = TRUE
    )
    return()
  }

  levels <- c(fl1, fl2)


  data.pre <- data() %>%
    select(vars) %>%
    filter(time == input$timepoint) %>%
    filter(.data[[input$catVars]] %in% levels) %>%
    pivot_wider(
      id_cols = c("id", input$catVars),
      names_from = "metabolites",
      values_from = "values"
    ) %>%
    select(-c("id"))


  form.test <- paste0("~ ", input$catVars)
  cate.name <- input$catVars

  # Create a design matrix indicating the two groups
  design <- model.matrix(as.formula(form.test), data.pre)

  # Fit a linear model to the data
  fit <- lmFit(t(data.pre[, -1]), design)

  fit.res <- eBayes(fit)

  fit.res.tab <- topTable(fit.res, sort.by = "none", coef = cate.name, number = nrow(data.pre))

  fit.res.tab.show <- fit.res.tab %>%
    as.data.frame() %>%
    mutate_if(is.numeric, ~ round(., 4))

  # benjamini hochberg

  voc.plot <- EnhancedVolcano(fit.res.tab,
    lab = rownames(fit.res.tab),
    x = "logFC",
    y = "P.Value",
    pCutoff = input$pcut,
    FCcutoff = input$fccut,
    boxedLabels = TRUE,
    drawConnectors = TRUE,
    legendPosition = "right",
    max.overlaps = Inf,
    xlim = c(-2.5, 2.5),
    ylim=c(0,7)
  )+
    theme(
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
  volcano.dat$vars <- vars
  volcano.dat$data.pre <- data.pre
  volcano.dat$form.test <- form.test
  volcano.dat$cate.name <- cate.name
  volcano.dat$res <- fit.res.tab.show[order(fit.res.tab.show$P.Value),]
  volcano.dat$volcano <- voc.plot
})

output$res.voc.table <- DT::renderDataTable({
  datatable(volcano.dat$res)
})
output$volcano.plot <- renderPlot({
  volcano.dat$volcano
}, bg = '#edeff4')
output$printcatvar <- renderText({
  input$catVars
})
