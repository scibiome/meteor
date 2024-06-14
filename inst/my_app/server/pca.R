##### PCA ####

#### Calculate PCA: A ----


#### Info box: ----


output$info_box_pca <- renderUI({

  HTML(paste0("<p align='justify'>
       In this section, a Principal Component Analysis (PCA) is performed for the selected time point. PCA helps to reduce the dimensionality of the data, making it easier to visualize and analyze. It can also help identify important metabolites that contribute most to the variance in the data.
       <br><br>
       <u>Visualizations provided:</u>
       <ul>
         <li>The <b>eigenvalues</b> of the principal components are displayed in the diagram in the upper left corner.</li>
         <li>A <b>scatter plot</b> with observations colored by the selected grouping variable is shown in the upper right corner.</li>
         <li>The <b>contributions of the variables</b> to the principal components and a <b>biplot</b> are presented at the bottom.</li>
         <li>A <b>3D scatter plot</b> can also be generated in the bottom panel.</li>
       </ul>
       <br>
       Principal components can be selected for different dimensions using the dropdown menu.
       <br><br>
       <u>When to Use:</u><br>
       Use PCA when you need to:
       <ul>
         <li>Reduce the dimensionality of your data to simplify visualization and analysis.</li>
         <li>Identify which variables (e.g., metabolites) contribute most to the variance in the data.</li>
         <li>Uncover underlying patterns in the data that might not be immediately apparent.</li>
       </ul>
       <u>Additional information:</u><br>
       <li>Tutorial <a href = https://rpkgs.datanovia.com/factoextra/index.html>factoextra</li>
       <li>Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18. <a href='https://doi.org/10.18637/jss.v025.i01'>10.18637/jss.v025.i01</a>
       </li>
      <li>Kassambara A, Mundt F (2020). _factoextra: Extract and Visualize the Results of Multivariate Data Analyses_. R package version 1.0.7,
  <https://CRAN.R-project.org/package=factoextra></li>"))
})







df_pca <- reactiveVal()

observe({
  req(data())

  a <- data() %>% select(time) %>% unique()

  # this was done since at the initialization, input$timepoint is not NULL
  place_holder_timepoint <- NULL
  if(!(input$timepoint %in% a[[1]])){
    place_holder_timepoint <- min(unlist(a))
  }
  else{
    place_holder_timepoint <- input$timepoint
  }
  # req(is_timepoint_loaded())
  # validate(as.character(need(is_timepoint_loaded(), "Waiting for computation...")))
  df_pca1 <- data() %>%
    select(-unlist(catVars_global())) %>%
    filter(time %in% place_holder_timepoint) %>%
    select(-time) %>%
    pivot_wider(names_from = "metabolites",
                values_from = "values", id_cols = "id") %>%
    select(-id)# %>% prcomp()
  df_pca(df_pca1)

})


observeEvent(df_pca(), {
  data()
  req(df_pca())

  res.pca <- prcomp(df_pca())

  n <- 1:ncol(res.pca$rotation)
  # print(res.pca)

  updateSelectInput(session, "pcA", choices = n, selected = n[1])

})

#### Calculate PCA: B ----

observeEvent(df_pca(), {

  req(data())

  res.pca <- prcomp(df_pca())

  n <- 1:ncol(res.pca$rotation)

  updateSelectInput(session, "pcB", choices = n, selected = n[2])

})

#### Calculate PCA: C ----

observeEvent(df_pca(), {

  res.pca <- prcomp(df_pca())

  n <- 1:ncol(res.pca$rotation)

  updateSelectInput(session, "pcC", choices = n, selected = n[3])
})

#### Eigenvalues ----

output$eigenPCA <- renderPlotly({

  res.pca <- prcomp(df_pca())

  # Visualize eigenvalues/variances
  k <- fviz_screeplot(res.pca, addlabels = TRUE) +
                      theme_apa()

  ggplotly(k) %>% layout(plot_bgcolor  = "#edeff4",
                         paper_bgcolor = "#edeff4",
                         fig_bgcolor   = "#edeff4")

})


#### PCA plot (individuals) ----

output$indPCA <- renderPlotly({

          a <- catv()

          vars <- c("id", "time", a, "metabolites", "values")


           df <- data() %>%
             select(all_of(vars)) %>%
             filter(time %in% input$timepoint) %>%
            select(-time) %>%
             pivot_wider(names_from = "metabolites",
                         values_from = "values", id_cols = all_of(c("id", a))) %>%
             select(-id)

  #        df <- df_pca()
#           TODO why was this selection ever done?
#           c <- df[,45:50]
#
#           c <- apply(c, 2, as.numeric)
          # df.pca <- prcomp(c)

           # df[is.na(df)] <- 0

          df.pca <- df %>%select(-a) %>% prcomp()

          # df.pca <- cbind(df[a], df.pca)

          # Visualize
          # Use habillage to specify groups for coloring

          dim1 <- input$pcA %>% as.integer()
          dim2 <- input$pcB %>% as.integer()

          if (length(unique(as.factor(df[[1]]))) < 2) {
            habillage = "none"
          }else
          {
            habillage = as.factor(df[[1]])
          }


          # TODO add the selction of axes and the habillage again
          k <- fviz_pca_ind(df.pca,
                            label = "none", # hide individual labels
                            axes = c(dim1, dim2),
                            habillage = habillage, # color by groups
                            #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                            addEllipses = TRUE # TODO: The Ellipse is hiding the data points. when settint to false its working again.
          ) + theme_apa()


          k <- ggplotly(k)

          # Fix für Labels

          for (i in 1:length(k$x$data)){
            if (!is.null(k$x$data[[i]]$name)){
              k$x$data[[i]]$name =  gsub("\\(","",str_split(k$x$data[[i]]$name,",")[[1]][1])
            }
          }

          k %>% layout(plot_bgcolor  = "#edeff4",
                       paper_bgcolor = "#edeff4",
                       fig_bgcolor   = "#edeff4",
                       legend= list(bgcolor = "#edeff4" ))


})


#### Biplot ----


output$biplotPCA <- renderPlot({

  # df <- data() %>%
  #         select(-c("treatment")) %>%
  #         filter(time %in% input$timepoint) %>%
  #         select(-time) %>%
  #         pivot_wider(names_from = "metabolites",
  #                     values_from = "values", id_cols = "id") %>%
  #         select(-id)


  res.pca <- prcomp(df_pca())

  dim1 <- input$pcA %>% as.integer()
  dim2 <- input$pcB %>% as.integer()

  # Control the transparency of variables using their contributions
  k <- fviz_pca_var(res.pca, col.var="contrib",
                    alpha.var = "contrib",
                    axes = c(dim1, dim2),
                    repel = T ) +
            theme_apa() +
            theme(
              panel.background = element_rect(fill = "#edeff4",
                                              colour = NA_character_), # necessary to avoid drawing panel outline
              panel.grid.major = element_blank(), # get rid of major grid
              panel.grid.minor = element_blank(), # get rid of minor grid
              plot.background = element_rect(fill = "#edeff4",
                                             colour = NA_character_), # necessary to avoid drawing plot outline
              legend.background = element_rect(fill = "#edeff4"),
              legend.box.background = element_rect(fill = "#edeff4"),
              legend.key = element_rect(fill = "#edeff4")
            )

        plot(k)


})

#### 3D PCA ----

output$biplot3D <- renderPlotly(NULL)

observeEvent(input$add_graph_pc_cross_3d, {

    output$biplot3D <-  renderPlotly({

    a <- catv()

    vars <- c("id", "time", a, "metabolites", "values")


    df <- data() %>% select(all_of(vars)) %>%
      filter(time %in% input$timepoint) %>%
      select(-time) %>%
      pivot_wider(names_from = "metabolites",
                  values_from = "values", id_cols = all_of(c("id", a))) %>%
      select(-id)

    pca <- prcomp(df[,-1])


    # Scores
    scores <- pca$x


    dim1 <- input$pcA %>% as.integer()
    dim2 <- input$pcB %>% as.integer()
    dim3 <- input$pcC %>% as.integer()

    x <- scores[,dim1]
    y <- scores[,dim2]
    z <- scores[,dim3]


    dt.pltly <- cbind(x,y,z,as.factor(df[[a]])) %>% as.data.frame()
    colnames(dt.pltly) <- c("x","y","z",a)

    legend.titel <- colnames(dt.pltly[a])

    # 3D plot
    library(plotly)
    p <- plot_ly(dt.pltly, x = ~x, y = ~y, z = ~z, color =~ifelse(is.na(dt.pltly[[a]]), "NA", as.character(dt.pltly[[a]])),
                 colors = c('#BF382A', '#0C4B8E'), type = "scatter3d" , mode = "markers") %>%
            layout(scene = list(xaxis = list(title = paste0("PC", dim1[1])),
                                yaxis = list(title = paste0("PC", dim2[1])),
                                zaxis = list(title = paste0("PC", dim3[1])),
                                showlegend=TRUE,
                                legend = list(title=list(text= legend.titel))),
                   plot_bgcolor  = "#edeff4",
                   paper_bgcolor = "#edeff4",
                   fig_bgcolor   = "#edeff4")

    print(p)

  })
})


#### Contribution PCA: A ----


output$contribPCa <- renderPlotly({

  res.pca <- prcomp(df_pca())

  dim1 <- input$pcA %>% as.integer()

  # Contributions of variables to PC1
  k <- fviz_contrib(res.pca, choice = "var", axes = dim1, top = 10)
  ggplotly(k) %>% layout(plot_bgcolor  = "#edeff4",
                         paper_bgcolor = "#edeff4",
                         fig_bgcolor   = "#edeff4")


})


#### Contribution PCA: B ----

output$contribPCb <- renderPlotly({

  # PCA
  res.pca <- prcomp(df_pca())

  dim2 <- input$pcB %>% as.integer()

  # Contributions of variables to PC2
  k <- fviz_contrib(res.pca, choice = "var", axes = dim2, top = 10)
  ggplotly(k) %>% layout(plot_bgcolor  = "#edeff4",
                         paper_bgcolor = "#edeff4",
                         fig_bgcolor   = "#edeff4")


})
