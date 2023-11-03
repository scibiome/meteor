#### Launch modal ####


#### Info box: ----

output$info_box_launch <- renderUI({
  HTML("<p align = 'justify'> With the import module you can easily load your data into MeTEor.
                                         Just press the button 'Click for data importing'. <p></p>
                                         Example data can be easily selected via
                                         <b> Environment > Select a data.frame > 'example_data'</b>.
                                         Under external file you can load your own data.</p> <p>")
})




observeEvent(input$launch_modal, {
  import_modal(
    id = "myid",
    from = c("env", "file"),
    title = "Import data to be used in application"
  )
})

imported <- import_server("myid", return_class = "data.frame")

data <- reactive({
  req(imported$data())
  imported$data()
  # a <- imported$data()[,5:dim(imported$data())[2]] %>%
  #
  # dim(imported$data())
})

catVars_global <- reactive({
  req(imported$data())
  dat.nam <- data()
  cat.nam <- colnames(dat.nam[, 5:ncol(dat.nam)]) %>% as.list()
  cat.nam
})

##### Datatable  ----

output$datatable <- DT::renderDataTable(imported$data(), server = T, caption = "Long Format Representation", options = list(
  scrollX = TRUE, # Enable horizontal scrolling
  scrollY = "400px", # Set a fixed height for vertical scrolling
  columnDefs = list(list(targets = "_all", width = "100px"))
))
output$datatable.wide <- DT::renderDataTable(data() %>%
  spread(metabolites, values), server = T, caption = "Wide Format Representation", options = list(
  scrollX = TRUE,
  scrollY = "400px",
  columnDefs = list(list(targets = "_all", width = "100px"))
))

observe({
  req(data())
  dat.nam <- data()
  cat.nam <- colnames(dat.nam[, 5:ncol(dat.nam)]) %>% as.list()

  updateSelectInput(session, "catVars", choices = cat.nam, selected = cat.nam[1])
})

catv <- reactive({
  a <- input$catVars
  return(a)
})
