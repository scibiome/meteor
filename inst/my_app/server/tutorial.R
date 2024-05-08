# start introjs when button is pressed with custom options and events
# observeEvent(
#   input$tutorial,
#   introjs(session,
#     options = list(
#       "nextLabel" = "Next",
#       "prevLabel" = "Back",
#       "skipLabel" = "<strong>
#       <i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>"
#     ),
#     events = list(onbeforechange = readCallback("switchTabs")))
# )

helptext <- reactive(data.table::data.table(
  tab = c("tutorial_main", "tutorial_main", "tutorial_file"),
  step = c(1, 2, 3),
  element = c("#tutorial_main", "#sidebar", "#launch_modal"),
  intro = c("Let's start a MeTEor tutorial", "This is the sidebar. Look how intro elements can nest", "File")
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
