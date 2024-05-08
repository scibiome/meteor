# start introjs when button is pressed with custom options and events
observeEvent(
  input$tutorial,
  introjs(session,
    options = list(
      "nextLabel" = "Next",
      "prevLabel" = "Back",
      "skipLabel" = "<strong>
      <i class='fa-regular fa-circle-xmark' style='color: red;'></i></strong>"
    ),
    # events = list("oncomplete"=I('alert("Glad that is over")'))
  )
)
