##### Network #####

#### get time points for slider ----

output$info_box_pcn <- renderUI({
  HTML("<p align = 'justify'> A Pearson correlation analysis is performed to assess the relationships between different metabolites.
       The results are then visualized as a network, with positive correlations shown in blue and negative correlations shown in red.
       To facilitate exploration of the data, a slider tool is available to selectively display correlations based on the user's desired
       level of strength or significance.
       Different layouts can be choosen and subgroups of the graph can be selected. For a selected metabolite, the correlation with all connected metabolites is
       shown as a scatterplot. The network can be exported as html.<p>")
})

observe({
  req(data())
  dat.nam <- data()
  time.names <- unique(dat.nam[, 2]) %>% as.list()

  time.names.vec <- as.character(unlist(time.names))

  min.time <- as.character(min(unlist(time.names)))
  max.time <- as.character(max(unlist(time.names)))


  updateSliderTextInput(session,
    inputId = "timepoint4", choices = time.names.vec,
    selected = min.time
  )
})


#### Some JS code for styling datatable ----

rowCallback <- c(
  "function( row, data, index ) {

            if (data[3] > 0) {
              $(row).find('td').css('background-color', '#ADD8E6');
            } else {$(row).find('td').css('background-color', '#FF7276');};

          }"
)

stored_node_pcn <- reactiveValues(node.pcn = NULL, edge.pcn = NULL)

#### Correlation network ----

output$network <- renderVisNetwork({
  d <- data()

  ed <- df_pca()

  tidy_cors <- ed %>%
                correlate() %>%
                stretch()


  graph_cors <- tidy_cors %>%
    filter(between(abs(r), input$abscorr[1], input$abscorr[2]))

  colnames(graph_cors) <- c("from", "to", "value")


  # identify communities
  g <- graph.data.frame(graph_cors[, c("from", "to")], directed = FALSE)
  E(g)$weight <- abs(graph_cors$value)
  g <- igraph::simplify(g)
  c1 <- igraph::cluster_fast_greedy(g)

# TODO change to reactive values
  isolate(stored_node_pcn$edge.pcn <- graph_cors %>% as.data.frame())
  isolate(stored_node_pcn$node.pcn <- unique(graph_cors$from) %>% as.data.frame())
  isolate(colnames(stored_node_pcn$node.pcn) <- "id")
  isolate(stored_node_pcn$node.pcn$group <- membership(c1))
  isolate(stored_node_pcn$edge.pcn$color <- ifelse(stored_node_pcn$edge.pcn$value > 0, "blue", "red"))
  # isolate(stored_node_pcn$edge.pcn$abs_value <- abs(stored_node_pcn$edge.pcn$value))


  visNetwork(isolate(stored_node_pcn$node.pcn), isolate(stored_node_pcn$edge.pcn)) %>%
    visEdges(color = isolate(stored_node_pcn$edge.pcn$color)) %>%
    visIgraphLayout(layout = input$layoutselect.pcn) %>%
    visOptions(
      highlightNearest = list(enabled = T, degree = list(from = 0, to = 1)),
      nodesIdSelection = TRUE,
      manipulation = TRUE,
      selectedBy = "group"
    ) %>%
    visInteraction(multiselect = TRUE) %>%
    visIgraphLayout() %>%
    visEvents(select = "function(nodes) {
                        Shiny.onInputChange('current_node_selection', nodes.nodes);
                        ;}")
})


##### render data table restricted to selected nodes ====

output$tbl <- renderDT(
  table.dt <- stored_node_pcn$edge.pcn %>%
    filter(from %in% input$current_node_selection) %>%

    rowwise() %>%
    mutate(results = paste(sort(c(from, to)), collapse = ",")) %>%
    ungroup() %>%
    distinct(results, .keep_all = TRUE) %>%
    select(-results, -color) %>%

    mutate(value = as.numeric(value)) %>%
    mutate_at(vars(value), funs(round(., 2))),
  options = list(
    rowCallback = JS(rowCallback),
    lengthChange = FALSE
  )
)

#### Export Network: https://github.com/datastorm-open/visNetwork/issues/138 ====

# get position info
observeEvent(input$store_position.pcn, {
  visNetworkProxy("network") %>% visGetPositions()
})

# format positions
nodes_positions <- reactive({
  positions <- input$network_positions

  if (!is.null(positions)) {
    nodes_positions <- do.call("rbind", lapply(positions, function(x) {
      data.frame(x = x$x, y = x$y)
    }))
    nodes_positions$id <- names(positions)
    nodes_positions
  } else {
    NULL
  }
})

output$downloadNetwork.pcn <- downloadHandler(
  filename = function() {
    paste("network-", Sys.Date(), ".html", sep = "")
  },
  content = function(con) {
    nodes_positions <- nodes_positions()
    if (!is.null(nodes_positions)) {
      nodes_save <- merge(stored_node_pcn$node.pcn, nodes_positions, by = "id", all = T)
    } else {
      nodes_save <- stored_node_pcn$node.pcn
    }

    visNetwork(nodes_save, stored_node_pcn$edge.pcn, width = "100%") %>%
      visEdges(color = stored_node_pcn$edge.pcn$color) %>%
      visIgraphLayout(layout = input$layoutselect.ggm) %>%
      visOptions(
        highlightNearest = list(enabled = T, degree = list(from = 0, to = 1)),
        nodesIdSelection = TRUE,
        manipulation = TRUE,
        selectedBy = "group"
      ) %>%
      visInteraction(multiselect = TRUE) %>%
      visIgraphLayout() %>%
      visEvents(select = "function(nodes) {
                      Shiny.onInputChange('current_node_selection', nodes.nodes);
                      ;}") %>%
      visExport(type = "pdf") %>%
      visSave(con)
  }
)


compute_scatter_plots <- function(connections, data_timepoint_selected) {
  plot_list <- vector('list', nrow(connections))

  for (i in 1:nrow(connections)) {
    message(i)

    plot_list[[i]] <- local({
      i <- i
      p1 <- ggplot(data = data_timepoint_selected) +
        aes(x = .data[[connections$from[i]]], y = .data[[connections$to[i]]]) +
        geom_point() +
        geom_smooth(method=lm, color = ifelse(connections$value[i] > 0, "blue","red")) +
        ggtitle(i) +
        labs(x = connections$from[i], y = connections$to[i]) +
        theme_apa() +
        theme(
          panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # necessary to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")
        )
      print(p1)
    })
  }

  return(plot_list)
}


#### Scatter Correlation plot ====

output$scatterplot <- renderPlot({


          req(stored_node_pcn$edge.pcn)
          stored_node_pcn$distinct_connections <- stored_node_pcn$edge.pcn %>%
                filter((from %in% input$current_node_selection)) %>%
              rowwise() %>%
              mutate(results = paste(sort(c(from, to)), collapse = ",")) %>%
              ungroup() %>%
              distinct(results, .keep_all = TRUE) %>%
              select(-results, -color)

            # create scatter plots if stored_node_pcn$distinct_connections are selected
            if ((!is.null(input$current_node_selection)) & (nrow(stored_node_pcn$distinct_connections[0]) > 0)) {

              data_timepoint_selected <- df_pca()

              plot_list = compute_scatter_plots(stored_node_pcn$distinct_connections, data_timepoint_selected)
              ggarrange(plotlist = plot_list) + bgcolor("#edeff4")
            }
          })


##### gaussian graphical model #####

#### slider input ----

observe({
  req(data())
  dat.nam <- data()
  time.names <- unique(dat.nam[, 2]) %>% as.list()

  time.names.vec <- as.character(unlist(time.names))

  min.time <- as.character(min(unlist(time.names)))
  max.time <- as.character(max(unlist(time.names)))


  updateSliderTextInput(session,
    inputId = "timepoint4", choices = time.names.vec,
    selected = min.time
  )
})


#### Some JS code for styling datatable ----


rowCallback <- c(
  "function( row, data, index ) {

            if (data[3] > 0) {
              $(row).find('td').css('background-color', '#ADD8E6');
            } else {$(row).find('td').css('background-color', '#FF7276');};

          }"
)


#### mgm.fit ----


output$info_box_ggm <- renderUI({
  HTML("<p align = 'justify'> A partial correlation analysis is performed to assess the relationships between different metabolites.
The results are then visualized as a network, with positive correlations shown in blue and negative correlations shown in red.
To facilitate exploration of the data, a slider tool is available to selectively display correlations based on the user's desired
level of strength or significance.
Different layouts can be choosen and subgroups of the graph can be selected. For a selected metabolite, the correlation with all connected metabolites is
shown as a scatterplot. The network can be exported as html. <p>")
})


observeEvent(input$ggm_compute, {

      d  <- data()
      ed <- df_pca()

  node_lst <- data.frame(unique(d$metabolites), unique(d$metabolites))
  colnames(node_lst) <- c("id", "label")

  mydata_matrix <- as.matrix(ed)

  fit <- mgm(mydata_matrix,
    type = rep("g", length(ed)),
    lamda.sel = "EBIC", k = 2, scale = F
  )

  stretched <- fit$pairwise$wadj %>%
    as.data.frame()

      colnames(stretched) <- colnames(ed)
      row.names(stretched) <- colnames(ed)
      ggm_data_stored$mgm.fit <- stretched
      ggm_data_stored$computation_done <- TRUE
      # return(stretched)

})

#### display the network ====

ggm_data_stored <- reactiveValues(mgm.fit = NULL, computation_done = FALSE,
                                  edge.ggm=NULL, node.ggm = NULL)

observeEvent(input$ggm_compute, {
  if(ggm_data_stored$computation_done)
  {

            output$network_ggm <-  renderVisNetwork({


                    stretched <- ggm_data_stored$mgm.fit

    stretched <- stretched %>%
      corrr::as_cordf() %>%
      corrr::stretch() %>%
      drop_na() %>%
      filter(between(abs(r), input$abscorr_ggm[1], input$abscorr_ggm[2]))

    colnames(stretched) <- c("from", "to", "value")

    # identify communities
    g <- graph.data.frame(stretched[, c("from", "to")], directed = FALSE)
    E(g)$weight <- stretched$value
    g <- igraph::simplify(g)
    c1 <- igraph::cluster_fast_greedy(g)


    isolate(ggm_data_stored$edge.ggm <- stretched %>% as.data.frame())
    isolate(ggm_data_stored$node.ggm <- unique(stretched$from) %>% as.data.frame())
    isolate(colnames(ggm_data_stored$node.ggm) <- "id")
    isolate(ggm_data_stored$node.ggm$group <- membership(c1))
    isolate(ggm_data_stored$edge.ggm$color <- ifelse(ggm_data_stored$edge.ggm$value > 0, "blue", "red"))
    isolate(ggm_data_stored$edge.ggm$value <- abs(ggm_data_stored$edge.ggm$value))


    visNetwork(isolate(ggm_data_stored$node.ggm), isolate(ggm_data_stored$edge.ggm)) %>%
      visEdges(color = isolate(ggm_data_stored$edge.ggm$color)) %>%
      visIgraphLayout(layout = input$layoutselect.ggm) %>%
      visOptions(
        highlightNearest = list(enabled = T, degree = list(from = 0, to = 1)),
        nodesIdSelection = TRUE,
        manipulation = TRUE,
        selectedBy = "group"
      ) %>%
      visInteraction(multiselect = TRUE) %>%
      visIgraphLayout() %>%
      visEvents(select = "function(nodes) {
                                      Shiny.onInputChange('current_node_selection', nodes.nodes);
                                      ;}")



          })
  }

})

#### Export Network: https://github.com/datastorm-open/visNetwork/issues/138 ====

# get position info
observeEvent(input$store_position, {
  visNetworkProxy("network_ggm") %>% visGetPositions()
})

# format positions

nodes_positions <- reactive({
  positions <- input$network_ggm_positions

  if (!is.null(positions)) {
    nodes_positions <- do.call("rbind", lapply(positions, function(x) {
      data.frame(x = x$x, y = x$y)
    }))
    nodes_positions$id <- names(positions)
    nodes_positions
  } else {
    NULL
  }
})

output$downloadNetwork <- downloadHandler(
  filename = function() {
    paste("network-", Sys.Date(), ".html", sep = "")
  },
  content = function(con) {
    nodes_positions <- nodes_positions()
    if (!is.null(nodes_positions)) {
      nodes_save <- merge(ggm_data_stored$node.ggm, nodes_positions, by = "id", all = T)
    } else {
      nodes_save <- ggm_data_stored$node.ggm
    }

    visNetwork(nodes_save, ggm_data_stored$edge.ggm, width = "100%") %>%
      visEdges(color = ggm_data_stored$edge.ggm$color) %>%
      visIgraphLayout(layout = input$layoutselect.ggm) %>%
      visOptions(
        highlightNearest = list(enabled = T, degree = list(from = 0, to = 1)),
        nodesIdSelection = TRUE,
        manipulation = TRUE,
        selectedBy = "group"
      ) %>%
      visInteraction(multiselect = TRUE) %>%
      visIgraphLayout() %>%
      visEvents(select = "function(nodes) {
                                    Shiny.onInputChange('current_node_selection', nodes.nodes);
                                    ;}") %>%
      visExport(type = "pdf") %>%
      visSave(con)
  }
)


#render data table restricted to selected nodes

observeEvent(input$ggm_compute, {
  output$tbl_ggm <- renderDT(
    table.dt <- ggm_data_stored$distinct_connections %>%
      mutate(value = as.numeric(value)) %>%
      mutate_at(vars(value), funs(round(., 2))),
    options = list(
      rowCallback = JS(rowCallback),
      lengthChange = FALSE)

  )

})

# output$tbl <- renderDT(
#   table.dt <- stored_node_pcn$edge.pcn %>%
#     filter(from %in% input$current_node_selection) %>%
#     mutate(value = as.numeric(value)) %>%
#     mutate_at(vars(value), funs(round(., 2))),
#   options = list(
#     rowCallback = JS(rowCallback),
#     lengthChange = FALSE
#   )
# )


##### Scatter Correlation plot ----

output$scatterplot_ggm <- renderPlot({
  if(ggm_data_stored$computation_done)
  {
    req(ggm_data_stored$edge.ggm)
    ggm_data_stored$distinct_connections <- ggm_data_stored$edge.ggm %>%
      filter((from %in% input$current_node_selection)) %>%
      rowwise() %>%
      mutate(results = paste(sort(c(from, to)), collapse = ",")) %>%
      ungroup() %>%
      distinct(results, .keep_all = TRUE) %>%
      select(-results, -color)


    # create scatter plots if distinct connections are selected
    if ((!is.null(input$current_node_selection)) & (nrow(ggm_data_stored$distinct_connections[0]) > 0)) {

      data_timepoint_selected <- df_pca()

      plot_list = compute_scatter_plots(ggm_data_stored$distinct_connections, data_timepoint_selected)

      ggarrange(plotlist = plot_list) + bgcolor("#edeff4")
  }
  }
})
