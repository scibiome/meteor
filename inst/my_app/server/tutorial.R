###### Tutorial main ------
helptext_main <- reactive(data.table::data.table(
  tab = c(
    "tutorial_main", "tutorial_main", "tutorial_main",
    "tutorial_main", "tutorial_main", "tutorial_main",
    "tutorial_main", "tutorial_main", "tutorial_main",
    "tutorial_main", "tutorial_main", "tutorial_main",
    "tutorial_main", "tutorial_main", "tutorial_main",
    "tutorial_main", "tutorial_main", "tutorial_main",
    "tutorial_main"
  ),
  step = c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12,
    13, 14, 15,
    16, 17, 18,
    19
  ),
  element = c(
    "#tutorial_main",
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
    "#tutorial_main"
  ),
  intro = c(
    "<b>Let's start a MeTEor tutorial!</b>
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
    "On the individual pages (infobox) you will find additional tutorials on the individual analyses and visualizations."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto"
  )
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
  intro = c("Data can be imported here. To import your own data, click on <b>External file</b>.
            To import example data, click on <b>Environment</b> and select one of the two sample data sets and then click on <b>Import data</b>. "),
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

###### Tutorial PCA ------


helptext_pca <- reactive(data.table::data.table(
  tab = c("tutorial_pca"),
  step = c(
    1, 2, 3,
    4, 5, 6,
    7, 8
  ),
  element = c(
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#shiny-tab-pca > div:nth-child(2) > div > div > div.box-body",
    "#eigenPCA > div > div > svg:nth-child(1) > g.draglayer.cursor-crosshair > g > rect.nsewdrag.drag",
    "#indPCA > div > div > svg:nth-child(1) > g.draglayer.cursor-crosshair > g > rect.nsewdrag.drag",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#shiny-tab-pca > div:nth-child(4) > div:nth-child(1)",
    "#biplotPCA > img",
    "#add_graph_pc_cross_3d"
  ),
  intro = c(
    "The principal component analysis is calculated for the selected time points. If the time point is changed, the analysis is also recalculated. ",
    "Here you can select which principal components are to be displayed.",
    "Using a scree plot, you can heuristically determine the optimal number of principal components that explain most of the variance in your data.
             The x-axis displays the principal components in order,
             while the y-axis shows the eigenvalues, representing the proportion of variance each principal component explains.
             The scree plot helps identify an 'elbow' point, after which adding more principal components contributes minimally to the explained variance.",
    "The PCA scatter plot visualizes the results of Principal Component Analysis by projecting data onto a lower-dimensional space using the principal components.
             Each axis represents a principal component, and each point represents an observation in the dataset.
             This plot helps identify clusters, trends, and outliers, revealing patterns and structures within the data that are not easily seen in higher dimensions.
             The grouping is done based on the selected categorical variable.",
    "Here you can select the categorical variable for displaying the groups.",
    "Here you can see the contribution of the metabolites to the respective principal component. The metabolites are listed in descending order of their contribution.",
    "This biplot is used to visualise variables in principal component space. It displays the variables as vectors,
             with the length of each vector representing the significance of the respective variable in the principal component.
             Longer vectors indicate variables with greater influence. The angles between the vectors indicate the correlations between the variables:
             smaller angles indicate higher positive correlations, while angles close to 180 degrees indicate negative correlations.
             In addition, the angle of the variables to the axes indicates their correlation with each principal component axis.
             This visualisation helps to understand the relative contributions of the variables to the principal components and their correlations with the axes.",
    "If you click on this button, you can create a 3D PCA plot."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_pca,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_pca()[tab == "tutorial_pca"]
      )
    )
  }
)


###### Tutorial TCAM ------


helptext_tcam <- reactive(data.table::data.table(
  tab = c("tutorial_tcam"),
  step = c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
  ),
  element = c(
    "#ind_pca_long > div > div > svg:nth-child(1) > g.draglayer.cursor-crosshair > g > rect.nsewdrag.drag",
    "#ind_FC > div > div > svg:nth-child(1) > g.draglayer.cursor-crosshair > g > rect.nsewdrag.drag",
    "#shiny-tab-tcam > div:nth-child(2) > div:nth-child(1)",
    "#shiny-tab-tcam > div:nth-child(2) > div:nth-child(2)",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#pca_loadings_long > div > div",
    "#fc_loadings_long > div > div",
    "#add_graph_pc_3d",
    "#add_graph_fc_3d"
  ),
  intro = c(
    "Here, principal component analysis is performed on the data, including all time points of the measurement.",
    "Here TCAM is applied to the data including all time points of measurement.",
    "Here you can select which principal components are to be displayed. (PCA)",
    "Here you can select which factors are to be displayes. (TCAM)",
    "Here you can select the categorical variable for displaying the groups.",
    "Here you can see the contribution of the metabolites to the respective principal component. The metabolites are listed in descending order of their contribution.",
    "Here you can see the contribution of the metabolites to the respective factors. Positive and negative factor loadings are displayed for the most important metabolites.",
    "If you click on this button, you can create a 3D PCA plot.",
    "If you click on this button, you can create a 3D TCAM plot."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_tcam,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_tcam()[tab == "tutorial_tcam"]
      )
    )
  }
)


###### Tutorial Ridge plot (comparing time points) ------


helptext_rp <- reactive(data.table::data.table(
  tab = c("tutorial_rp"),
  step = c(1, 2, 3),
  element = c(
    "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
    "#shiny-tab-op1 > div:nth-child(2) > div > div > div.box-body > div > div > div",
    "#rp_compute"
  ),
  intro = c(
    "Here you can select one or more metabolites to visualize.",
    "Here you can select the time points you want to compare.",
    "Click here to generate a ridge plot showing the density distribution for the selected metabolites grouped by time points."
  ),
  position = c("auto", "auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_rp,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_rp()[tab == "tutorial_rp"]
      )
    )
  }
)

###### Tutorial Ridge plot (Compare groups) ------


helptext_rp2 <- reactive(data.table::data.table(
  tab = c("tutorial_rp2"),
  step = c(
    1, 2, 3,
    4
  ),
  element = c(
    "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#rp_compute2"
  ),
  intro = c(
    "Here you can select one or more metabolites to visualize.",
    "Here you can select the time point for which you want to create the ridge plot.",
    "Here you the categorical variable for which you want to compare the groups",
    "Click here to generate a ridge plot showing the density distribution for the selected metabolites grouped by the categorical variable."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_rp2,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_rp2()[tab == "tutorial_rp2"]
      )
    )
  }
)


###### Tutorial Binary Prediction ------



helptext_pred <- reactive(data.table::data.table(
  tab = c("tutorial_pred"),
  step = c(1, 2, 3,
           4, 5, 6,
           7, 8, 9,
           10, 11, 12),
  element = c(
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#prediction_method-label",
    "#prediction_method > div > div:nth-child(1) > label",
    "#prediction_method > div > div:nth-child(2) > label",
    "#prediction_method > div > div:nth-child(3) > label",
    "#valitation_method-label",
    "#valitation_method > div > div:nth-child(1) > label",
    "#valitation_method > div > div:nth-child(2) > label",
    "#shiny-tab-pred > div:nth-child(3) > div.col-sm-2 > div > div > div > div.selectize-input.items.full.has-options.has-items",
    "#rf_prediction",
    "#load_top_features"
  ),
  intro = c(
    "Here, you can select for which time point you want to perform the analysis.",
    "Here, you can select a categorical variable you want to predict. Note: For now only binary prediction is possible.",
    "Here, you can select between machine learning models.",
    "If you choose this option a random forest is used for prediction.",
    "If you choose this option a logistic regression is used for prediction.",
    "If you choose this option Extrem Gradient Boosting (XGBoost) is used fo prediciton.",
    "Here, you can select between two different validation methods.",
    "If you choose this option cross-validation is applied.",
    "If you choose this option leave-one-out cross-validation is applied.",
    "Here, you can choose the percentage of your data to use as test data. Ensure that your dataset is sufficiently large to allow for splitting the data.",
    "If you click on this button the model is computed.",
    "Here, you can add the most predictive features to the selection."
  ),
  position = c("auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto")
))


observeEvent(
  eventExpr = input$tutorial_pred,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_pred()[tab == "tutorial_pred"]
            )
    )
  }
)








###### Tutorial Friedman test ------


helptext_fried <- reactive(data.table::data.table(
  tab = c("tutorial_fried"),
  step = c(1, 2),
  element = c(
    "#id8-label",
    "#act_fried"
  ),
  intro = c(
    "Here you can select a metabolite for which you want to perform the Friedman test.",
    "Click here to compute the Friedman test."
  ),
  position = c("auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_fried,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_fried()[tab == "tutorial_fried"]
      )
    )
  }
)

###### Tutorial Mixed ANOVA ------


helptext_mixedanova <- reactive(data.table::data.table(
  tab = c("tutorial_mixanova"),
  step = c(
    1, 2, 3,
    4
  ),
  element = c(
    "#shiny-tab-mixedanova > div > ul > li.active",
    "#id10-label",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#act_mixed_anova"
  ),
  intro = c(
    "Here you can calculate a mixed ANOVA for a single selected metabolite.",
    "Here you can select the metabolite for which you want to perform the analysis.",
    "Here you can select a categorical variable to be used as the between factor in the analysis.",
    "If you click on this button, the mixed ANOVA is calculated."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_mixanova,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_mixedanova()[tab == "tutorial_mixanova"]
      )
    )
  }
)


###### Tutorial Mixed ANOVA for feature selection ------


helptext_mixedanova_feat <- reactive(data.table::data.table(
  tab = c("tutorial_mixanova_feat"),
  step = c(
    1, 2, 3,
    4, 5
  ),
  element = c(
    "#shiny-tab-mixedanova > div > ul > li.active",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#selectfactor-label",
    "#act_mixed_anova_selection",
    "#load_top_features_anova",
    "#sidebarItemExpanded > ul > div.form-group.shiny-input-container"
  ),
  intro = c(
    "Here, you can perform a mixed ANOVA for all metabolites. The metabolites will then be ranked based on their p-values for the given factor.",
    "Here you can select a categorical variable to be used as the between factor in the analysis.",
    "Here, you can select the within factor (time), the between factor (categorical variable), or the interaction between time and the categorical variable.",
    "If you click on this button, the mixed ANOVA is calculated.",
    "Here, you can select the top-ranked feature based on the lowest p-value for a given factor. The selected feature will be marked in the metabolite picker.",
    "Selected metabolites can be found here."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_mixanova_feat,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_mixedanova_feat()[tab == "tutorial_mixanova_feat"]
      )
    )
  }
)


###### Tutorial repeated measures ANOVA ------



helptext_repanova <- reactive(data.table::data.table(
  tab = c("tutorial_repanova"),
  step = c(
    1, 2, 3,
    4
  ),
  element = c(
    "#repeated_anova_metabolite-label",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#repeated_anova_category-label",
    "#act_repeated_anova"
  ),
  intro = c(
    "Here, you can select a single metabolite for the analysis.",
    "Here, you can select a categorical variable.
     In the next stept you can select a subgroup for which you want to perform the analysis.",
    "Here, you can select a subgroup for which you want to perform the analysis.",
    "If you click here the repeated measures ANOVA is performed."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_repanova,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_repanova()[tab == "tutorial_repanova"]
            )
    )
  }
)

###### Tutorial repeated measures ANOVA for feature selection ------

# TODO Infotext missing



helptext_repanova_sel <- reactive(data.table::data.table(
  tab = c("tutorial_repanova_sel"),
  step = c(
    1, 2, 3,
    4, 5
  ),
  element = c(
   "#sidebarItemExpanded > ul > li:nth-child(4) > a",
   "#repeated_category-label",
   "#act_repeated_anova_selection",
   "#load_top_features_repeated_anova",
   "#sidebarItemExpanded > ul > div.form-group.shiny-input-container"
  ),
  intro = c(
    "Here, you can select a categorical variable.
     In the next stept you can select a subgroup for which you want to perform the analysis.",
    "Here, you can select a subgroup for which you want to perform the analysis.",
    "If you click here the repeated measures ANOVA is performed.",
    "Here, you can select the top-ranked feature based on the lowest p-value for the time factor.
    The selected feature will be marked in the metabolite picker.",
    "Selected metabolites can be found here."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_repanova_sel,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_repanova_sel()[tab == "tutorial_repanova_sel"]
            )
    )
  }
)





###### Tutorial Linear mixed model ------


helptext_lmm <- reactive(data.table::data.table(
  tab = c("tutorial_lmm"),
  step = c(
    1, 2, 3,
    4, 5, 6,
    7
  ),
  element = c(
    "#shiny-tab-lmm > div > ul > li.active",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#id9-label",
    "#lmm_select-label",
    "#act_lmm",
    "#tab-2496-1 > div:nth-child(4) > div",
    "#tab-2496-1 > div:nth-child(5) > div > div",
    "#tab-2496-1 > div:nth-child(6) > div > div"
  ),
  intro = c(
    "Here you can calculate a mixed ANOVA for a single selected metabolite.",
    "Here you can select a categorical variable to be used as a fixed factor in the analysis.",
    "Here you can select the metabolite for which you want to perform the analysis.",
    "Here, you can choose between different types of models: random intercept, random slope, or random intercept and random slope.",
    "If you click on this button, the linear mixed model is calculated.",
    "This box shows you the testing of the model assumptions.",
    "This box shows you the model formula used for computing the linear mixed model.",
    "This box shows you the model summary including estimates for fixed and random effects."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_lmm,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_lmm()[tab == "tutorial_lmm"]
      )
    )
  }
)

###### Tutorial Linear mixed model (comparison) ------


helptext_lmm_comp <- reactive(data.table::data.table(
  tab = c("tutorial_lmm_comp"),
  step = c(
    1, 2, 3,
    4, 5
  ),
  element = c(
    "#shiny-tab-lmm > div > ul > li.active",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#act_lmm2",
    "#tab-2496-2 > div:nth-child(3) > div",
    "#tab-2496-2 > div:nth-child(4) > div"
  ),
  intro = c(
    "Here you can perform a model comparion between the different types of the linear mixed model.",
    "Here you can select a categorical variable to be used as a fixed factor in the analysis.",
    "If you click on this button, the different linear mixed modesl are calculated",
    "This box shows you the model summary including estimates for fixed and random effects for the different models.",
    "This box displays a model comparison using ANOVA: for linear mixed models (LMM), ANOVA evaluates whether adding fixed or random effects significantly improves model fit by comparing nested models and providing statistics such as AIC, BIC, log-likelihood, chi-square, and p-values."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_lmm_comp,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_lmm_comp()[tab == "tutorial_lmm_comp"]
      )
    )
  }
)

###### Tutorial Network Diagram (Pearson Correlation Network) ------


helptext_pcn <- reactive(data.table::data.table(
  tab = c("tutorial_pcn"),
  step = c(
    1, 2, 3,
    4, 5, 6,
    7, 8
  ),
  element = c(
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#abscorr-label",
    "#shiny-tab-pcn > div:nth-child(2) > div.col-sm-4",
    "#nodeSelectnetwork",
    "#selectedBynetwork",
    "#graphnetwork > div.vis-network > div.vis-edit-mode > button",
    "#shiny-tab-pcn > div:nth-child(4) > div",
    "#shiny-tab-pcn > div:nth-child(6) > div"
  ),
  intro = c(
    "Here you can select for which time point the correlation network should be calculated.",
    "To make it easier to explore the network, you can filter by absolute correlation values here. Metabolites that are no longer linked to other metabolites are filtered out.",
    "Here you can select between different network layouts.",
    "Here you can select a single metabolite in the network.",
    "Here you can select between detected communities in the network.",
    "Here you can perform modifications to the graph.",
    "To export the current network as an HTML file, first click on 'Store positions!' and then click on 'Download network'.",
    "If you select one or multiple metabolites from the network (by pressing Ctrl/Cmd),
             the table will display the correlations between the selected metabolites. Below the table, the associations between the metabolites will be shown in scatterplots."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_pcn,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_pcn()[tab == "tutorial_pcn"]
      )
    )
  }
)



###### Tutorial Network Diagram (Gaussian Graphical Model) ------


helptext_ggm <- reactive(data.table::data.table(
  tab = c("tutorial_ggm"),
  step = c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
  ),
  element = c(
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#abscorr_ggm-label",
    "#ggm_compute",
    "#shiny-tab-ggm > div:nth-child(2) > div.col-sm-4",
    "#nodeSelectnetwork_ggm",
    "#selectedBynetwork_ggm",
    "#graphnetwork_ggm > div.vis-network > div.vis-edit-mode > button",
    "#shiny-tab-ggm > div:nth-child(5) > div",
    "#shiny-tab-ggm > div:nth-child(7) > div"
  ),
  intro = c(
    "Here you can select for which time point the correlation network should be calculated.",
    "To make it easier to explore the network, you can filter by absolute partial correlation values here. Metabolites that are no longer linked to other metabolites are filtered out.",
    "If you click here the gaussian graphical model is computed. This can take a moment.",
    "Here you can select between different network layouts.",
    "Here you can select a single metabolite in the network.",
    "Here you can select between detected communities in the network.",
    "Here you can perform modifications to the graph.",
    "To export the current network as an HTML file, first click on 'Store positions!' and then click on 'Download network'.",
    "If you select one or multiple metabolites from the network (by pressing Ctrl/Cmd),
             the table will display the partial correlations between the selected metabolites. Below the table, the associations between the metabolites will be shown in scatterplots."
  ),
  position = c(
    "auto", "auto", "auto",
    "auto", "auto", "auto",
    "auto", "auto", "auto"
  )
))


observeEvent(
  eventExpr = input$tutorial_ggm,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_ggm()[tab == "tutorial_ggm"]
      )
    )
  }
)


###### Tutorial Line plot (by individuals) ------


helptext_il <- reactive(data.table::data.table(
  tab = c("tutorial_il"),
  step = c(1, 2, 3),
  element = c(
    "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#lp_compute2"
  ),
  intro = c(
    "Here, you can select the metabolites to be displayed in a line plot. The lines represent the trajectories of individuals, color-coded by the categorical variable.",
    "Here, you can select the categorical variable to color-code the individuals.",
    "If you click here the line plots are generated."
  ),
  position = c("auto", "auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_il,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_il()[tab == "tutorial_il"]
      )
    )
  }
)

###### Tutorial Line plot (by groups) ------


helptext_gl <- reactive(data.table::data.table(
  tab = c("tutorial_gl"),
  step = c(1, 2, 3),
  element = c(
    "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#lp_compute3"
  ),
  intro = c(
    "Here, you can select the metabolites to be displayed in a line plot. The lines represent the trajectories of group means, color-coded by the categorical variable.",
    "Here, you can select the categorical variable to color-code the individuals.",
    "If you click here the line plots are generated."
  ),
  position = c("auto", "auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_gl,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_gl()[tab == "tutorial_gl"]
      )
    )
  }
)

###### Tutorial mean line plot ------


helptext_ml <- reactive(data.table::data.table(
  tab = c("tutorial_ml"),
  step = c(1, 2),
  element = c(
    "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
    "#lp_compute4"
  ),
  intro = c(
    "Here, you can select the metabolites to be displayed in a line plot. The lines represent the mean trajectories of the individual metabolites.",
    "If you click here the line plots are generated."
  ),
  position = c("auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_ml,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext_ml()[tab == "tutorial_ml"]
      )
    )
  }
)


###### Tutorial single metabolite heatmap  ------


helptext_sm <- reactive(data.table::data.table(
  tab = c("tutorial_sm"),
  step = c(1, 2, 3),
  element = c(
    "#shiny-tab-sm > div.form-group.shiny-input-container",
    "#shiny-tab-sm > div:nth-child(3) > div:nth-child(1) > div",
    "#shiny-tab-sm > div:nth-child(3) > div:nth-child(2) > div"
  ),
  intro = c(
    "Here, you can select the metabolite for which you want to generate the heatmap.",
    "Here, you can separate individuals into distinct clusters using a hierarchical clustering solution.",
    "Here, you can separate time points into distinct clusters using a hierarchical clustering solution."
  ),
  position = c("auto", "auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_sm,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_sm()[tab == "tutorial_sm"]
            )
    )
  }
)


###### Tutorial by timepoint heatmap  ------


helptext_bt <- reactive(data.table::data.table(
  tab = c("tutorial_bt"),
  step = c(1, 2, 3,
           4, 5),
  element = c(
        "#sidebarItemExpanded > ul > li:nth-child(3) > a",
        "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
        "#heatmap_compute",
        "#shiny-tab-bt > div:nth-child(3) > div:nth-child(1)",
        "#shiny-tab-bt > div:nth-child(3) > div:nth-child(2)"
  ),
  intro = c(
    "Here, you can select a time point for which you want to generate the heatmap.",
    "Here, you can select multiple metabolites to be used in the heatmap.",
    "If you click here the heatmap generated.",
    "Here, you can separate individuals into distinct clusters using a hierarchical clustering solution.",
    "Here, you can separate metabolites into distinct clusters using a hierarchical clustering solution."
  ),
  position = c("auto", "auto", "auto",
               "auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_bt,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_bt()[tab == "tutorial_bt"]
            )
    )
  }
)

###### Tutorial volcano ------


helptext_volcano <- reactive(data.table::data.table(
  tab = c("tutorial_volcano"),
  step = c(1, 2, 3,
           4, 5, 6),
  element = c(
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#sidebarItemExpanded > ul > li:nth-child(4) > a",
    "#shiny-tab-volcano > div:nth-child(3) > div.col-sm-3 > div",
    "#shiny-tab-volcano > div:nth-child(3) > div.col-sm-6 > div",
    "#shiny-tab-volcano > div:nth-child(5) > div.col-sm-3 > div",
    "#shiny-tab-volcano > div:nth-child(5) > div.col-sm-6 > div",
    "#act_volc",
    "#load_top_features_volcano"),
  intro = c(
    "Here, you can select the time point of measurement for the analysis.",
    "Here, you can select the categorical variable for the analysis",
    "To generate the volcano plot, a differential abundance analysis is conducted by comparing two groups.
     If the selected categorical variable has more than two groups, one specific group can be chosen here for the comparison.",
    "Here, you can chose the second group.",
    "Here, you can select a cut-off for statistical significance.",
    "Here, you can select a cut-off for absolute log2 fold-change",
    "Click here to perform the differential abundance analysis and generate the volcano plot.",
    "Here, you can select the top 10 differentially abundant metabolites based on the p-value. They are added to the metabolite picker."
  ),
  position = c("auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto")
))


observeEvent(
  eventExpr = input$tutorial_volcano,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_volcano()[tab == "tutorial_volcano"]
            )
    )
  }
)


###### Tutorial Enrichment ------


helptext_enrichment <- reactive(data.table::data.table(
  tab = c("tutorial_enrichment"),
  step = c(1, 2, 3,
           4, 5),
  element = c(
    "#sidebarItemExpanded > ul > li:nth-child(3) > a",
    "#editableField",
    "#shiny-tab-export > div:nth-child(4) > div > a",
    "#jsonInput",
    "#shiny-tab-export > div:nth-child(6) > div"),
  intro = c(
    "Here, you can select metabolites to be included in the enrichment analysis.",
    "Here, you can copy the selected metabolites.",
    "Go to MetaboAnalyst and paste the copied metabolites.",
    "Paste the downloaded query results here.",
    "Click on the button 'Get enriched pathways' to receive a list of pathways where your selected metabolites are present."
  ),
  position = c("auto", "auto", "auto",
               "auto", "auto" )
))


observeEvent(
  eventExpr = input$tutorial_enrichment,
  handlerExpr = {
    introjs(session,
            options = list(
              "nextLabel" = "Next",
              "prevLabel" = "Back",
              "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
              steps = helptext_enrichment()[tab == "tutorial_enrichment"]
            )
    )
  }
)









