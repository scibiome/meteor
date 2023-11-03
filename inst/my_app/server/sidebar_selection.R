#### Sidebar selection ####


##### Show categorical variables ----

# observe({
#   req(data())
#   #browser()
#   dat.nam <- data()
#   cat.nam <- colnames(dat.nam[,5:ncol(dat.nam)]) %>% as.list()
#   updateSelectInput(session, "catVars", choices = cat.nam, selected = cat.nam[1])
#
# })
#
# catv <- reactive({ a <- input$catVars
# return(a)})


##### list metabolites ----

observeEvent(input$remove_metabolites, {

  # mobse
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[,3]) %>% as.list()

  print(paste0("You have chosen: ", feature_importance_stored))
  # req(input$changed)
  ui_metabolites$selection <- ''

  updatePickerInput(session, inputId = "metabolite_Picker", choices = metabolite.names, selected="",
                    choicesOpt = list(
                      style = c(
                        rep("color: black;", length(metabolite.names)),
                        ".selectize-dropdown-content .option:hover { color: green; }"
                      )
                      # might be an option https://stackoverflow.com/questions/57972351/customize-background-color-of-selectinput-in-shiny

                      )
  )

}, ignoreInit = TRUE)

observeEvent(input$add_metabolites, {

  # mobse
  req(data())
  dat.nam <- data()
  metabolite.names <- unique(dat.nam[,3]) %>% as.list()

  print(paste0("You have chosen: ", feature_importance_stored))
  # req(input$changed)
  ui_metabolites$selection <- metabolite.names

  updatePickerInput(session, inputId = "metabolite_Picker", choices = metabolite.names, selected=metabolite.names,
                    choicesOpt = list(
                      style = c(rep("color: black;", length(metabolite.names))))
  )

}, ignoreInit = TRUE)


observe({
    req(data())
    dat.nam <- data()
    metabolite.names <- unique(dat.nam[,3]) %>% as.list()

    updateMultiInput(session, "id", choices = metabolite.names, selected = metabolite.names[1])
})


##### update metabolite_Picker ----

observe({
    req(data())
    dat.nam <- data()
    metabolite.names <- unique(dat.nam[,3]) %>% as.list()

    updatePickerInput(session, inputId = "metabolite_Picker", choices = metabolite.names,
                      choicesOpt = list(
                        style = c(rep("color: black;", length(metabolite.names))))
                      )
})

# choicesOpt = list(
#   style = c("color: steelblue; font-size: 150%;",
#             "color: firebrick; text-align: right;", "font-weight: bold;",
#             "background: forestgreen; color: white;"))
# )

##### categorial variable levels ----

observeEvent(input$catVars, {
      req(data())
      dat.nam <- data()
      a <- input$catVars
      dat.nam <- dat.nam[a]

      trt.names <- unique(dat.nam[,1]) %>% as.list()

        choices <- trt.names[!is.na(trt.names)]

      updateSelectInput(session, "trtmt", choices = choices, selected = choices)
})

##### Names of time points ----

is_timepoint_loaded <- reactiveVal(0)

observe({
  req(data())
  dat.nam <- data()
  time.names <- unique(dat.nam[,2]) %>% as.numeric() %>% sort()

  updateSelectInput(session, "time.names", choices = time.names, selected = time.names)

})


##### List time points ----


observeEvent(data(), {
  req(data())
  dat.nam <- data()
  time.names <- unique(dat.nam[,2]) %>%  as.list()

  time.names.vec <- as.character(unlist(time.names))

  min.time = as.character(min(unlist(time.names)))
  max.time = as.character(max(unlist(time.names)))


  updateSliderTextInput(session, inputId = "timepoint", choices = time.names.vec,
                        selected = min.time)
  is_timepoint_loaded(1)
})

