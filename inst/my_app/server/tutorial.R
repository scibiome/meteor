
###### Tutorial main ------
helptext_main <- reactive(data.table::data.table(
  tab = c("tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main"),
  step = c(1, 2, 3,
           4, 5, 6,
           7, 8, 9,
           10,11,12,
           13,14,15,
           16,17,18,
           19),
  element = c("#tutorial_main",
              ".sidebar-menu",
              "#sidebarItemExpanded > ul > li:nth-child(2) > a",
              "#sidebarItemExpanded > ul > li:nth-child(3) > a",
              "#sidebarItemExpanded > ul > li:nth-child(4) > a",
              "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
              "#add_metabolites",
              "#remove_metabolites",
              "#sidebarItemExpanded > ul > li:nth-child(7) > a",
              "#sidebarItemExpanded > ul > li:nth-child(8) > a",
              "#sidebarItemExpanded > ul > li:nth-child(9) > a",
              "#sidebarItemExpanded > ul > li:nth-child(10) > a",
              "#sidebarItemExpanded > ul > li:nth-child(11) > a",
              "#sidebarItemExpanded > ul > li:nth-child(12) > a",
              "#sidebarItemExpanded > ul > li:nth-child(13) > a",
              "#sidebarItemExpanded > ul > li:nth-child(14) > a",
              "#sidebarItemExpanded > ul > li:nth-child(15) > a",
              "#sidebarItemExpanded > ul > li:nth-child(16) > a",
              "#tutorial_main"),
  intro = c("<b>Let's start a MeTEor tutorial!</b>
            <p>You can also navigate through the tutorial using the arrow keys.
            The right arrow key takes you to the next point, the left arrow key takes you to the previous point.</p>",
            "This is the navigation bar. Here you can select analysis and visualisation methods and have further configuration options.",
            "Here you can upload and analyse your data set. Example data sets are also available.",
            "As soon as your data is loaded, you can navigate between the different time points here.
             This has an impact on some analyses and visualisations. You can find more information on the respective page.",
            "Here you can select a categorical variable according to which you would like to group your analysis and visualisations.
             You may also only be interested in certain levels of your categorical variables.
             You can also select these. On which analysis and visualisation the selection of your
             categorical variable has an influence can be found on the respective page.",
            "In this menu you can select one or more metabolites.
             This has an influence on individual analyses or visualisations.
             You can find more information on the respective pages.",
            "If you click on this button, all metabolites are added to the selection.",
            "If you click on this button, all metabolites will be removed from the selection.",
            "Two dimensionality reduction approaches are available in this menu.
             Either the principal component analysis or modern tensor factorisation (TCAM).
             You can find more information on the respective pages.",
            "In this menu you have two options to display ridge plots for one or more metabolites.
             Either the metabolite distributions are displayed according to time points or according to grouping (categorical variable).
             You can find more information on the respective pages.",
            "Various binary prediction methods are available here.
             These include logistic regression, random forest and XGBoost.
             Further information can be found on the respective page.",
            "Various methods for statistical testing and modelling are available here.
             These include the Friedman test, mixed and repeated measures ANOVA and linear mixed models.
             Further information can be found on the respective pages. ",
            "Here you have two options for creating and visualisation of networks.
             One is the correlation network based on the Pearson correlation and the other is a Gaussian graphical model that visualises the partial correlations between the metabolites.
             Further information can be found on the respective pages.",
            "Here you have the option of displaying various line diagrams for the metabolite.
             The individual line diagrams show the individual course of observations for each selected metabolite.
             The observations are coloured according to the grouping variable (categorical variable).
             The group line plots show the average course of the groups. The 'mean lines' show the average course of the individual metabolites over time.
             Further information can be found on the respective pages.",
            "Here you have the option of creating cluster heatmaps.
             You can either create a heatmap for a single metabolite over the different time points.
             Information about the expression level of the observations and their grouping (categorical variables) is available.
             Or several metabolites with their observations for the selected time point are displayed and clustered.
             Further information can be found on the respective pages.",
            "Here you can perform a differential expression analysis of the metabolites and visualise the results using a volcano plot.
             The groupings are based on the selected categorical variable.
             The analysis is performed separately for each time point. Further information can be found on the respective page.",
            "Here you can query selected metabolites from a database.
             A query is sent to the MetaboAnalyst API to obtain different identifiers for each metabolite, e.g. HMDB, PubChem and KEGG.
             Using the HMDB identifier a pathway enrichment analysis using the Relational Database of Metabolomics Pathways (RaMP) is performed,
             which provides information on metabolic pathways involving the selected metabolites. Further information can be found on the respective page.",
            "Here you can find an overview of the packages used in MeTEor and the corresponding references.",
            "On the individual pages you will find additional tutorials on the individual analyses and visualizations."),
  position = c("auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto")
))

observeEvent(
  eventExpr = input$tutorial_main,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_main()[tab == "tutorial_main"]
      )
    )
  }
)



###### Tutorial file ------



helptext_file <- reactive(data.table::data.table(
  tab = c("tutorial_file"),
  step = c(1),
  element = c("#launch_modal"),
  intro = c("bla"),
  position = c("auto")
))


observeEvent(
  eventExpr = input$tutorial_file,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_file()[tab == "tutorial_file"]
      )
    )
  }
)
