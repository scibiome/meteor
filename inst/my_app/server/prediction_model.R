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
      # fig_bgcolor = "#edeff4",
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
  ed[ed[catv()] == as.numeric(unique_values[2, 1]), ][catv()] <- 999
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
  n_folds <- 5
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

  if (input$prediction_method == "LR") {
    # Train the model on the training set using glmnet for L1 regularization
    rfFit <- train(
      y = y_train_names,
      x = x_train,
      trControl = train_control,
      method = "glmnet",  # Change to glmnet for L1 and L2 regularization
      metric = input$metric,
      tuneGrid = expand.grid(
        alpha = 0.5,  # Elastic net mixing parameter (0 = L2, 1 = L1)
        lambda = seq(0.1, 0.7, 0.05)  # Range of lambda values for regularization
      )
    )
  }



  parse_param_grid <- function(param_grid_input) {
    # Split by space to separate each parameter (adjusted to handle the new format)
    param_pairs <- strsplit(param_grid_input, "\\s+")[[1]]
    param_list <- list()

    for (pair in param_pairs) {
      # Split the parameter name and values
      param_name_value <- strsplit(pair, "=")[[1]]
      if (length(param_name_value) == 2) {
        param_name <- trimws(param_name_value[1])  # Parameter name
        param_values <- gsub("[()]", "", trimws(param_name_value[2]))  # Remove parentheses
        param_values <- as.numeric(unlist(strsplit(param_values, ",")))  # Split by comma and convert to numeric

        # Assign to list
        param_list[[param_name]] <- param_values
      }
    }

    return(param_list)
  }
  param_grid <- parse_param_grid(input$param_grid)



  valid_params <- list(
    max_depth = 1,
    nrounds = 1,
    eta = 1,
    gamma = 1,
    subsample = 1,
    min_child_weight = 1,
    colsample_bytree = 1
  )

  # Function to validate the parameters in param_grid
  validate_params <- function(param_grid, prediction_method) {
    valid <- TRUE
    message <- "Validation successful."

    # Define valid parameters for XGBoost
    valid_params_xgb <- c("max_depth", "nrounds", "eta", "gamma", "subsample", "min_child_weight", "colsample_bytree")

    if (prediction_method == "XGB") {
      # List of valid parameter names for XGBoost
      valid_param_names <- valid_params_xgb

      # Check if all parameters in param_grid are valid
      invalid_params <- setdiff(names(param_grid), valid_param_names)

      if (length(invalid_params) > 0) {
        valid <- FALSE
        message <- paste("Invalid parameters found for XGB:", paste(invalid_params, collapse = ", "))
      }

      # Optional: Check if any valid parameters are missing in param_grid
      missing_params <- setdiff(valid_param_names, names(param_grid))
      if (length(missing_params) > 0) {
        valid <- FALSE
        message <- paste(message, "Missing parameters for XGB:", paste(missing_params, collapse = ", "))
      }
    }

    # Check for prediction method "RF"
    if (prediction_method == "RF") {
      # Ensure that only 'mtry' is in param_grid
      if (!("mtry" %in% names(param_grid))) {
        valid <- FALSE
        message <- "For RF method, 'mtry' must be specified."
      }

      # Check if there are any additional parameters besides 'mtry'
      if (length(names(param_grid)) > 1 || !("mtry" %in% names(param_grid))) {
        valid <- FALSE
        message <- "For RF method, only 'mtry' should be provided."
      }
    }

    return(list(valid = valid, message = message))
  }

  result <- validate_params(param_grid, input$prediction_method)
  print(result)

  if (!result$valid) {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Error",
                style = "color: steelblue;"),
        result$message
      ),
      html = TRUE
    )
    return()
  }

    # Convert the parsed parameters into a grid for grid search
  if (length(param_grid) > 0) {
    output$grid_output <- renderPrint({
      gbmGrid  # Show the generated grid
    })
    gbmGrid <- expand.grid(param_grid)
  } else {
    output$grid_output <- renderPrint({
      "Invalid parameter grid input, using base grid"
    })
    gbmGrid <- expand.grid(
      max_depth = c(3, 5, 7, 10),
      nrounds = (1:10) * 50,
      eta = 0.3,
      gamma = 0,
      subsample = 1,
      min_child_weight = 1,
      colsample_bytree = 0.6
    )
  }

  output$grid_output <- renderPrint({
    input$param_grid  # Display the parameter grid input
  })

  rfGrid <- expand.grid(
    mtry = c(3, 5, 7, 10)  # Number of variables randomly sampled as candidates at each split
  )

  if (input$prediction_method == "RF") {
      rfFit <- train(
        y = y_train_names,
        x = x_train,
        method = "rf",
        trControl = train_control,
        tuneGrid = rfGrid,  # Use gbmGrid if tuneLength is not specified
        metric = input$metric
      )
  }

  if (input$prediction_method == "XGB") {
    mtry <- round(sqrt(18), 0)


      rfFit <- train(
        y = y_train_names,
        x = x_train,
        method = "xgbTree",
        trControl = train_control,
        tuneGrid = gbmGrid,  # Use gbmGrid if tuneLength is not specified
        verbose = FALSE,
        metric = input$metric
      )

  }



  rfFit$bestTune

  print(input$prediction_method)
  print(rfFit[["bestTune"]])
  rfFit[['results']]

  # if we have test data, the ROC is computed on it
  if (input$testdata == 0) {
    predition1 <- rfFit[["pred"]][, 3:4]
    predition1$obs <- as.factor(y_train_names)


    rfFit[["results"]] <- rfFit[["results"]][rownames(rfFit[["bestTune"]]), ]


    prediction_stored$scores <- t(round(as.matrix(c(
      setNames(rfFit[["results"]]$AUC, "cv AUC"),
      setNames(rfFit[["results"]]$Sensitivity, "cv Sensitivity"),
      setNames(rfFit[["results"]]$Specificity, "cv Specificity"),
      setNames(rfFit[["results"]]$F1, 'cv F1'),
      setNames(rfFit[["results"]]$Accuracy, 'cv Accuracy'),
      setNames(nrow(y_train), "y_train"), # updated later
      imbalance
    )),3))

    # browser()

    prediction_stored$prediction1 <- predition1
    prediction_stored$rfFit <- rfFit
    prediction_stored$computation_done <- TRUE
    print(rfFit)

  } else {
    predition1 <- predict(rfFit, x_test, type = "prob")


    train_tab <- table(predicted = predict(rfFit, x_test), actual = as.factor(make.names(as.integer(unlist(y_test)))))
    test_con_mat <- confusionMatrix(train_tab)


    rfFit[["results"]] <- rfFit[["results"]][rownames(rfFit[["bestTune"]]), ]


    prediction_stored$scores <- t(round(as.matrix(c(
       setNames(rfFit[["results"]]$AUC, 'cv AUC'),
        setNames(rfFit[["results"]]$Sensitivity, 'cv Sensitivity'),
       setNames(rfFit[["results"]]$Specificity, 'cv Specificity'),
       setNames(rfFit[["results"]]$F1, 'cv F1'),
       setNames(rfFit[["results"]]$Accuracy, 'cv Accuracy'),
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

  output$best_params <- renderPrint({
    # Print the best tuning parameters found
    if (!is.null(rfFit$bestTune)) {
      cat("Best Parameters:\n")
      print(rfFit$bestTune)
    } else {
      cat("No best parameters found.")
    }
  })
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
      # fig_bgcolor = "#edeff4",
      legend = list(bgcolor = "#edeff4")
    )
  }
})

output$report_prediction <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report_prediction.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(file.path("server", "report_prediction.Rmd"), tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(prediction_stored_RMD = prediction_stored, input_RMD = input)

    # Knit the document, passing in the `params` list, and eval it in as
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

observe({
  # Determine the new value based on the selected prediction method
  new_value <- ""
  if (input$prediction_method == "XGB") {
    new_value <- "max_depth=(3,5,7,10) nrounds=(50,100,150,200,250,300,350,400,450,500) eta=(0.3) gamma=(0) subsample=(1) min_child_weight=(1) colsample_bytree=(0.6)"
  } else if (input$prediction_method == "RF") {
    new_value <- "mtry=(1,2,3,4,5)"  # Example value for RF method
  }

  # Update the textInput value
  updateTextInput(session, "param_grid", value = new_value)
})

