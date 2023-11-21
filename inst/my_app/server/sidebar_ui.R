output$conditionalsidebarMenuOutput <- renderMenu({
  dashboardSidebar(
    conditionalPanel(
      condition = "input.menu1 !== 'tcam'",
      sidebarMenu(
        menuItem("Time",
          tabName = "helloWorld", icon = icon("fas fa-sliders-h"), # TODO rename
          sliderTextInput(
            inputId = "timepoint",
            label = "Select time point:",
            choices = "",
            selected = "",
            grid = TRUE,
            animate = F
          ), startExpanded = F
        ),
        menuItem("Categorical Variables",
          tabName = "categorical",
          icon = icon("fas fa-border-all"),
          selectInput(
            inputId = "catVars",
            label = "Choose categorical variable",
            choice = "",
            selected = "",
            multiple = FALSE
          ),
          selectInput(
            inputId = "trtmt",
            label = "Categorical variable Levels:",
            choices = "",
            selected = "",
            multiple = TRUE
          ), startExpanded = F
        )
      )
    ),


    #### Conditional Panel: time selection is hidden ====

    conditionalPanel(
      condition = "input.menu1 == 'tcam'",
      sidebarMenu(
        menuItem("Categorical Variables",
          tabName = "categorical",
          icon = icon("fas fa-border-all"),
          selectInput(
            inputId = "catVars",
            label = "Choose categorical variable",
            choice = "",
            selected = "",
            multiple = FALSE
          ),
          selectInput(
            inputId = "trtmt",
            label = "Categorical variable Levels:",
            choices = "",
            selected = "",
            multiple = TRUE
          ), startExpanded = F
        )
      )
    )
  )
})
