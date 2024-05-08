
helptext <- reactive(data.table::data.table(
  tab = c("tutorial_main", "tutorial_main", "tutorial_main","tutorial_file"),
  step = c(1, 2, 3, 1),
  element = c("#tutorial_main",
              ".sidebar-menu",
              "#sidebarItemExpanded > ul > li:nth-child(2) > a",
              "#launch_modal"),
  intro = c("Let's start a MeTEor tutorial",
            "This is the sidebar. Look how intro elements can nest",
            "File",
            "bla"),
  position = c("auto", "auto", "auto", "auto")
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
