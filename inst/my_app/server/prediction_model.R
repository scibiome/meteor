##### Prediction models #####

##### logistic regression ----

output$info_box_pred <- renderUI({
  HTML("<p align='justify'>
        In this section, you can perform binary classification using logistic regression (LR), random forest (RF), or XGBoost (XGB).
        To prevent overfitting and test the generalizability of the learned model, stratified 5-fold cross-validation or leave-one-out cross-validation (LOOCV) are available.<br><br>

        <u>When to use</u>:<br>
        This tool is ideal if you need to classify between two groups of your categorical variable, such as different patient groups, and determine which metabolites are most predictive.<br><br>

        <u>Additional Information:</u><br>
        This tool is designed for users interested in binary classification scenarios, such as distinguishing between two classes using predictive algorithms and cross-validation methods.
        The ROC curve is computed on the test set. If the test set size is set to 0%, the application will not compute an ROC curve.
        It utilizes all available metabolites in the dataset for prediction and shows the feature importance of the most predictive ones.
        Additionally, the tool allows you to select the ten most predictive metabolites for further analysis.<br><br>

        To learn more about the algorithms used and their applications, refer to
        Kuhn, M. <a href='https://www.jstatsoft.org/article/view/v028i05' target='_blank'>Building predictive models in R using the caret package</a> 2008.<br>
        For detailed information on cross-validation (CV) and leave-one-out cross-validation (LOOCV), visit
        Ng, A. Y. <a href='https://ai.stanford.edu/~ang/papers/cv-final.pdf' target='_blank'>Preventing \"overfitting\" of cross-validation data</a> 1997.
      </p>")
})




# changes
prediction_stored <- reactiveValues(prediction1 = 0, computation_done = FALSE, rfFit = NULL, auc = NULL)

observeEvent(input$load_top_features, {

  # mobse
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[,3]) %>% as.list()

  ui_metabolites$selection <- c(ui_metabolites$selection, feature_importance_stored)

  updatePickerInput(session, inputId = "metabolite_Picker", choices = metabolite.names, selected=ui_metabolites$selection,
                        choicesOpt = list(
                        style = c(rep("color: black;", length(metabolite.names))))
                    )

}, ignoreInit = TRUE)


output$pred_scores <- renderDT(
  datatable(prediction_stored$scores,
    options = list(searching = FALSE, paging = FALSE, info = FALSE),
    extensions = "Scroller",
    style = "bootstrap"
  )
)

output$prediction_text <- renderText({
  "No testdata for to compute ROC curve"
})

output$prediction <- renderPlot(
  {
    if(input$testdata != 0){

    if (prediction_stored$computation_done) {
      labels2 <- as.numeric(factor(prediction_stored$prediction1$obs, levels = c("X1", "X0")))
      scores2 <- prediction_stored$prediction1[[1]]

      rocobj <- pROC::roc(labels2, scores2)

      prediction_stored$scores[, "AUC"] <- round(rocobj$auc, 3)

      prediction_stored$auc <- rocobj$auc
      # create ROC plot
      g <- ggroc(rocobj, legacy.axes = TRUE) + ggtitle("ROC Curve") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        theme(panel.background = element_rect(fill = "#edeff4", colour = "black")) +
        theme(plot.background = element_rect(fill = "#edeff4", colour = "#edeff4")) +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        )

      g
    }
    }
  },
  bg = "#edeff4"

)



output$feature_imp_plot <- renderPlotly({
  a <- 1
  if (prediction_stored$computation_done) {
    imps <- as.matrix(varImp(prediction_stored$rfFit)$importance)

    feature_imp <- head(imps[order(imps, decreasing = TRUE), ], 10)

    feature_imp <- as.data.frame(feature_imp)
    colnames(feature_imp) <- "importance"
    feature_imp$metabolite_name <- row.names(feature_imp)
    feature_importance_stored <<- feature_imp$metabolite_name
    p <- ggplot(feature_imp, aes(x = reorder(metabolite_name, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "#0072B2", ) +
      theme_apa() +
      xlab("metabolites") +
      coord_flip()


    ggplotly(p) %>% layout(
      plot_bgcolor = "#edeff4",
      paper_bgcolor = "#edeff4",
      fig_bgcolor = "#edeff4",
      legend = list(bgcolor = "#edeff4")
    )
  }
})
##### Prediction models #####

##### logistic regression ----



prediction_stored <- reactiveValues(prediction1 = 0, computation_done = FALSE, rfFit = NULL, auc = NULL)


observeEvent(input$rf_prediction, {
  set.seed(1)
  # input$trtmt gets only initialized when a categorial variable has been selected
  if (is.null(input$trtmt)) {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Error",
                style = "color: steelblue;"),
        "Select a categorial Variable"
      ),
      html = TRUE
    )
    return()
  }

  ed <- data() %>%
    filter(time %in% input$timepoint) %>%
    select(-time) %>%
    filter(!!sym(catv()) %in% input$trtmt) %>%
    pivot_wider(
      names_from = "metabolites",
      values_from = "values",
      id_cols = c("id", colnames(data()[, 5:ncol(data())]))
    )

  c <- 1
  # input$testdata
  # 12 because 10 fold cv plus 1 test sample
  if ((nrow(ed) < 12) & (input$testdata != 0)) {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Error",
                style = "color: steelblue;"),
        "dataset too small to use testdata"
      ),
      html = TRUE
    )
    return()
  }

  unique_values <- unique(ed[catv()])

  if (nrow(unique_values) != 2) {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Error",
                style = "color: steelblue;"),
        "Prediction works only for binary prediction. There could be too less sample of both categories."
      ),
      html = TRUE
    )
    return()
  }

  # in case the first variable would be equal to unique_values[1, 1]
  ed[ed[catv()] == as.numeric(unique_values[2, 1]), ][catv()] <-  999
  ed[ed[catv()] == as.numeric(unique_values[1, 1]), ][catv()] <- 1

  ed[ed[catv()] == 999, ][catv()] <- 0
  imbalance <- setNames(sum(ed[catv()]) / nrow(ed[catv()]), "imbalance")


  # amount of test data
  if (input$testdata == 0) {
    x_train <- ed %>% select(-c("id", colnames(data()[, 5:ncol(data())])))
    y_train <- ed[catv()]
  } else {
    target_cat <- as.integer(unlist(ed[catv()]))
    inTraining <- createDataPartition(as.factor(target_cat), p = 1-as.numeric(input$testdata), list = FALSE)
    x_train <- ed[inTraining, ] %>% select(-c("id", colnames(data()[, 5:ncol(data())])))
    y_train <- ed[inTraining, ][catv()]
    x_test <- ed[-inTraining, ] %>% select(-c("id", colnames(data()[, 5:ncol(data())])))
    y_test <- ed[-inTraining, ][catv()]
  }

  # create stratified folds for CV
  n_folds <- 10
  cvIndex <- createFolds(as.factor(pull(y_train)), n_folds, returnTrain = T, list=T)

  # CV method
  if (input$valitation_method == "CV") {
    train_control <- trainControl(
      method = "repeatedcv",
      number = n_folds,
      repeats = 10,
      index = cvIndex,
      summaryFunction = multiClassSummary,
      classProbs = T,
      savePredictions = T
    )
  } else {
    train_control <- trainControl(
      method = "LOOCV",
      number = 1,
      savePredictions = "final",
      classProbs = T,
      summaryFunction = multiClassSummary
    )
  }



  ##### model training ----

  #### prediction methods ====

  y_train_names <- make.names(as.integer(unlist(y_train)))
  if (input$prediction_method == "RF") {
    rfFit <- train(
      y = y_train_names, x = x_train,
      method = "rf",
      trControl = train_control,
      tuneLength = 1,
      metric = "AUC"
    )
  }

  if (input$prediction_method == "LR") {
    # train the model on training set
    rfFit <- train(
      y = y_train_names, x = x_train,
      trControl = train_control,
      method = "glm",
      family = binomial(),
      tuneLength = 1,
      metric = "AUC"
    )
  }
  if (input$prediction_method == "XGB") {
    mtry <- round(sqrt(18), 0)
    gbmGrid <- expand.grid(
      max_depth = c(3, 5, 7, 10),#, 5, 7, 10),
      nrounds = (1:10) * 50, # number of trees
      # default values below
      eta = 0.3,
      gamma = 0,
      subsample = 1,
      min_child_weight = 1,
      colsample_bytree = 0.6
    )

    rfFit <- train(
      y = y_train_names, x = x_train,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = gbmGrid,
      tuneLength = 1,
      verbose = FALSE,
      metric = "AUC"

    )
  }



  print(input$prediction_method)
  print(rfFit[["bestTune"]])
  rfFit[['results']]

  # if we have test data, the ROC is computed on it
  if (input$testdata == 0) {
    predition1 <- rfFit[["pred"]][, 3:4]
    predition1$obs <- as.factor(y_train_names)

    if (input$prediction_method == "XGB") {
      rfFit[["results"]] <- rfFit[["results"]][rownames(rfFit[["bestTune"]]), ]
    }

    prediction_stored$scores <- t(round(as.matrix(c(
      setNames(rfFit[["results"]]$AUC, "cv AUC"),
      setNames(rfFit[["results"]]$Sensitivity, "cv Sensitivity"),
      setNames(rfFit[["results"]]$Specificity, "cv Specificity"),
      setNames(nrow(y_train), "y_train"), # updated later
      imbalance
    )),3))

    prediction_stored$prediction1 <- predition1
    prediction_stored$rfFit <- rfFit
    prediction_stored$computation_done <- TRUE

  } else {
    predition1 <- predict(rfFit, x_test, type = "prob")


    train_tab <- table(predicted = predict(rfFit, x_test), actual = as.factor(make.names(as.integer(unlist(y_test)))))
    test_con_mat <- confusionMatrix(train_tab)

    # for XGB gridsearch was performed
    if (input$prediction_method == "XGB") {
      rfFit[["results"]] <- rfFit[["results"]][rownames(rfFit[["bestTune"]]), ]
    }

    prediction_stored$scores <- t(round(as.matrix(c(
       setNames(rfFit[["results"]]$AUC, 'cv AUC'),
        setNames(rfFit[["results"]]$Sensitivity, 'cv Sensitivity'),
       setNames(rfFit[["results"]]$Specificity, 'cv Specificity'),
       setNames(rfFit[["results"]]$F1, 'cv F1'),
       imbalance,
       setNames(0, "test AUC"), # updated later
       setNames(nrow(y_train), "y_train"), # updated later
       setNames(nrow(y_test), "y_test"), # updated later
      setNames(test_con_mat$overall["Accuracy"], "test Accuracy"),
      setNames(test_con_mat$byClass["Sensitivity"], "test Sensitivity"),
      setNames(test_con_mat$byClass["Specificity"], "test Specificity"),
      setNames(test_con_mat$byClass["F1"], "test F1"),
      setNames(test_con_mat$byClass["Balanced Accuracy"], "test Balanced Accuracy")

    )), 3))


    predition1$obs <- as.factor(make.names(as.integer(unlist(y_test))))
    predition1$obs_numeric <- y_test
    prediction_stored$prediction1 <- predition1
    prediction_stored$rfFit <- rfFit
    prediction_stored$computation_done <- TRUE
  }
})


output$pred_scores <- renderDT(
  datatable(prediction_stored$scores,
    options = list(searching = FALSE, paging = FALSE, info = FALSE),
    extensions = "Scroller",
    style = "bootstrap"
  )
)

output$prediction_text <- renderText({
  "No testdata for to compute ROC curve"
})

# output$color_pr <- renderPrint({ # für die Output box
#   req(input$select1)
#   input$select1
# })
#
# output$panelStatus <- reactive({ # wenn show == show, ist der wert True
#   input$select1=="show"
# })
# outputOptions(output, "panelStatus", suspendWhenHidden = FALSE) #set the


output$prediction <- renderPlot(
  {
    if(isolate(input$testdata) != 0){

    if (prediction_stored$computation_done) {
      labels2 <- as.numeric(factor(prediction_stored$prediction1$obs, levels = c("X1", "X0")))
      scores2 <- prediction_stored$prediction1[[1]]

      rocobj <- roc(labels2, scores2)

      prediction_stored$scores[, "test AUC"] <- round(rocobj$auc, 3)

      prediction_stored$auc <- rocobj$auc
      # create ROC plot
      g <- ggroc(rocobj, legacy.axes = TRUE) + ggtitle("ROC Curve") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        theme(panel.background = element_rect(fill = "#edeff4", colour = "black")) +
        theme(plot.background = element_rect(fill = "#edeff4", colour = "#edeff4")) +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        )

      g
    }
    } else{
      g <- plot(0, 0, type="n")
      g
    }
  },
  bg = "#edeff4"
)



output$feature_imp_plot <- renderPlotly({
  if (prediction_stored$computation_done) {
    if(input$prediction_method =='XGB'){
      # varImp is fine for the model, see https://stackoverflow.com/questions/59632899/does-the-caret-varimp-wrapper-for-xgboost-xgbtree-use-xgboost-gain
      imps <- as.matrix(varImp(prediction_stored$rfFit)$importance, scale=FALSE)
    } else {
      imps <- as.matrix(varImp(prediction_stored$rfFit)$importance)
    }
    imps_df <- data.frame(Overall = imps)
    feature_imp <- imps_df %>% filter(Overall > 0.1) %>% arrange(desc(Overall)) %>% head(10)


    # feature_imp <- imps[order(imps, decreasing = TRUE), ] %>% filter(Overall == 0)#  %>% head(10)

    # feature_imp <- as.data.frame(feature_imp)
    colnames(feature_imp) <- "importance"
    feature_imp$metabolite_name <- row.names(feature_imp)
    feature_importance_stored <<- feature_imp$metabolite_name
    p <- ggplot(feature_imp, aes(x = reorder(metabolite_name, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "#0072B2", ) +
      theme_apa() +
      xlab("metabolites") +
      coord_flip()


    ggplotly(p) %>% layout(
      plot_bgcolor = "#edeff4",
      paper_bgcolor = "#edeff4",
      fig_bgcolor = "#edeff4",
      legend = list(bgcolor = "#edeff4")
    )
  }
})



