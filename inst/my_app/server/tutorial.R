
helptext <- reactive(data.table::data.table(
  tab = c("tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_main",
          "tutorial_main", "tutorial_main", "tutorial_file"),
  step = c(1, 2, 3,
           4, 5, 6,
           7, 8, 1),
  element = c("#tutorial_main",
              ".sidebar-menu",
              "#sidebarItemExpanded > ul > li:nth-child(2) > a",
              "#sidebarItemExpanded > ul > li:nth-child(3) > a",
              "#sidebarItemExpanded > ul > li:nth-child(4) > a",
              "#sidebarItemExpanded > ul > div.form-group.shiny-input-container",
              "#add_metabolites",
              "#remove_metabolites",
              "#launch_modal"),
  intro = c("Let's start a MeTEor tutorial",
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
            "bla"),
  position = c("auto", "auto", "auto",
               "auto", "auto", "auto",
               "auto", "auto", "auto")
))

observeEvent(
  eventExpr = input$tutorial_main,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext()[tab == "tutorial_main"]
      )
    )
  }
)

observeEvent(
  eventExpr = input$tutorial_file,
  handlerExpr = {
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Back",
        "skipLabel" = "<strong><i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>",
        steps = helptext()[tab == "tutorial_file"]
      )
    )
  }
)
