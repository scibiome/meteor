#### PCA + TCAM ####

#### PCA ####

#### categorial variable selection ----

# output$info_box_tcam <- renderUI({
#   HTML("<p align = 'justify'>
#        In this section, principal component analysis is contrasted with a modern tensor factorization method (TCAM).
#        TCAM is an unsupervised tensor factorization method for the analysis of multiway data such as longitudinal omics data.
#        Both methods are applied to all measurement time points.
#
#        For more information on TCAM, see:
#        <p> Mor U, Cohen Y, Valdés-Mas R, Kviatcovsky D, Elinav E, Avron H.
#        Dimensionality reduction of longitudinal'omics data using modern tensor factorizations. PLoS Computational Biology. 2022 Jul 15;18(7):e1010212.</p>")
# })

output$info_box_tcam <- renderUI({
  HTML("
    <p align='justify'>
      This section contrasts principal component analysis (PCA) with a modern tensor factorization method known as TCAM. TCAM is an unsupervised tensor factorization method designed for analyzing multiway data, such as longitudinal omics data. Both methods are applied across all measurement time points to facilitate comprehensive data analysis.
    </p>
    <h4>Principal Component Analysis (PCA)</h4>
    <p align='justify'>
      PCA is a widely used technique for dimensionality reduction that transforms the data into a set of orthogonal components, capturing the most variance in the data.
    </p>
    <h4>Tensor Component Analysis (TCAM)</h4>
    <p align='justify'>
      TCAM provides an advanced approach for dimensionality reduction, specifically tailored for multiway data structures. It decomposes the data tensor into factors that can reveal underlying patterns and trends.
    </p>
    <h4>Application to Longitudinal Data</h4>
    <p align='justify'>
      Both PCA and TCAM are applied to all measurement time points to compare their effectiveness in capturing and reducing data dimensionality.
    </p>
  <h4>When to Use</h4>
    <p align='justify'>
      Use TCAM when:
    </p>
    <ul>
      <li>You have multiway data structures, such as longitudinal or multi-modal data.</li>
      <li>You need to uncover complex underlying patterns and interactions within the data.</li>
      <li>Traditional methods like PCA do not adequately capture the variance or structure of your data.</li>
    </ul>
    <p align='justify'>
    <u>Used Packages and Additional Information:</u><br>
    </p>
    <ul>
     <li>Tutorial: <a href='https://rpkgs.datanovia.com/factoextra/index.html' target='_blank'>factoextra</a></li>
          <li>Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18. <a href='https://doi.org/10.18637/jss.v025.i01' target='_blank'>10.18637/jss.v025.i01</a></li>
          <li>Kassambara A, Mundt F (2020). <em>factoextra: Extract and Visualize the Results of Multivariate Data Analyses</em>. R package version 1.0.7. Available at: <a href='https://CRAN.R-project.org/package=factoextra' target='_blank'>https://CRAN.R-project.org/package=factoextra</a></li>
      <li>Mor U, Cohen Y, Valdés-Mas R, Kviatcovsky D, Elinav E, Avron H. (2022). Dimensionality reduction of longitudinal omics data using modern tensor factorizations. PLoS Computational Biology, 18(7):e1010212. <a href='https://doi.org/10.1371/journal.pcbi.1010212' target='_blank'>https://doi.org/10.1371/journal.pcbi.1010212</a></li>
    </ul>
  ")
})


#### pcAlong ----

stored_pca_long <- reactiveVal()


observe({
  req(data())

  d <- data()

  df <- d %>%
    pivot_wider(names_from = c("metabolites", "time"),
                values_from = "values", id_cols = "id")

  complete_rows <- complete.cases(df)

  df <- df[complete_rows, ]

  stored_pca_long(df[,-1])
})

observeEvent(stored_pca_long(),{
    #
    res.pca <- prcomp(stored_pca_long()[,-1])

    n <- 1:ncol(res.pca$rotation)

    updateSelectInput(session, inputId = "pcAlong", choices = n, selected = n[1])
})

#### pcBlong ----

observeEvent(stored_pca_long(),{

  res.pca <- prcomp(stored_pca_long()[,-1])

  n <- 1:ncol(res.pca$rotation)

  updateSelectInput(session, inputId = "pcBlong", choices = n, selected = n[2])

})


#### pcClong ----


observeEvent(stored_pca_long(),{

  res.pca <- prcomp(stored_pca_long()[,-1])

  n <- 1:ncol(res.pca$rotation)

  updateSelectInput(session, inputId = "pcClong", choices = n, selected = n[3])


})

#### PCA plot individuals ----


output$ind_pca_long <- renderPlotly({

      a <- catv()

      vars <- c("id", "time", a, "metabolites", "values")


      c <- colnames(df)
      df <- data() %>%
        select(all_of(vars)) %>%
        pivot_wider(names_from = c("metabolites", "time"),
                    values_from = "values")

      # for patients
      df <- data() %>%
                select(all_of(vars)) %>%
                pivot_wider(names_from = c("metabolites", "time"),
                            values_from = "values", id_cols = all_of(c("id", a))) %>%
                select(-id)

      complete_rows <- complete.cases(df[, -1])

      df <- df[complete_rows, ]

      res.pca <- prcomp(df[,-1])

      dim1 <- input$pcAlong %>% as.integer()
      dim2 <- input$pcBlong %>% as.integer()

      if (length(unique(as.factor(df[[1]]))) < 2) {
        habillage = "none"
      }else
      {
        habillage = as.factor(df[[1]])
      }

      a <-  1

      k <- fviz_pca_ind(res.pca,
                        label = "none", # hide individual labels
                        axes = c(dim1, dim2),
                        habillage = habillage, # color by groups
                        #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                        addEllipses = TRUE, # Concentration ellipses
                        legend.title = catv()
      ) + theme_apa()

      k <- ggplotly(k, tooltip = c("x", "y", "color"))

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

#### PCA loadings ----


output$pca_loadings_long <- renderPlotly({

        a <- catv()

        vars <- c("id", "time", a, "metabolites", "values")


        df <- data() %>% select(all_of(vars)) %>%
          pivot_wider(names_from = c("metabolites", "time"),
                      values_from = "values", id_cols = all_of(c("id", a))) %>%
          select(-id)



        complete_rows <- complete.cases(df)

        df <- df[complete_rows, ]




        res.pca <- PCA(df[,-1], graph = FALSE)

        dim1 <- input$pcAlong %>% as.integer()

        k <- fviz_contrib(res.pca, choice = "var", axes = dim1, top = 10)
        ggplotly(k) %>% layout(plot_bgcolor  = "#edeff4",
                               paper_bgcolor = "#edeff4",
                               fig_bgcolor   = "#edeff4",
                               legend= list(bgcolor = "#edeff4" ))


})


#### 3D PCA plot ----


observeEvent(input$add_graph_pc_3d,{

  output$ind_pca_long_3D <- renderPlotly({

        a <- catv()

        vars <- c("id", "time", a, "metabolites", "values")


        df <- data() %>% select(all_of(vars)) %>%
          pivot_wider(names_from = c("metabolites", "time"),
                      values_from = "values", id_cols = all_of(c("id", a))) %>%
          select(-id)

        complete_rows <- complete.cases(df[, -1])

        df <- df[complete_rows, ]





        pca <- prcomp(df[,-1])

        # Scores
        scores <- pca$x


        dim1 <- input$pcAlong %>% as.integer()
        dim2 <- input$pcBlong %>% as.integer()
        dim3 <- input$pcClong %>% as.integer()

        x <- scores[,dim1]
        y <- scores[,dim2]
        z <- scores[,dim3]

        dt.pltly <- cbind(x,y,z,as.factor(df[[a]])) %>% as.data.frame()
        colnames(dt.pltly) <- c("x","y","z",a)

        legend.titel <- colnames(dt.pltly[a])


        # 3D plot
        p <- plot_ly(dt.pltly, x = ~x, y = ~y, z = ~z, color = ~ifelse(is.na(dt.pltly[[a]]), "NA", as.character(dt.pltly[[a]])),
                     colors = c('#BF382A', '#0C4B8E'),
                     type = "scatter3d" , mode = "markers")        %>%
          layout(scene = list(xaxis = list(title = paste0("PC", dim1[1])),
                              yaxis = list(title = paste0("PC", dim2[1])),
                              zaxis = list(title = paste0("PC", dim3[1])),
                              showlegend=TRUE,
                              legend = list(title=list(text= legend.titel))),
                 plot_bgcolor  = "#edeff4",
                 paper_bgcolor = "#edeff4"#,
                 # fig_bgcolor   = "#edeff4"
                 )
        print(p)

  })
})



#### TCAM ####

#### fcAlong ----

# stored_tcam_long <- reactiveVal()
stored_tcam <- reactiveValues(data.tca = NULL, tca.loadings =  NULL, var.explan = NULL, col.fac=NULL)


observe({
  req(data())
  a <- catv()

  vars <- c("id", "time", a, "metabolites", "values")

  d <- data()
  d2 <- data()
  ids <- c(colnames(d)[5:ncol(d)])
  d <- d %>% select(-c(ids))


  d <- d %>%
    pivot_wider(names_from = "metabolites",
                values_from = "values", id_cols = c("id", "time"))

  # we only use patients where all datapoint are available
  patients_with_all_timepoints <- d %>%
    group_by(id) %>%
    filter(all(unique(d$time) %in% time)) %>%
    distinct(id)

  d <- semi_join(d, patients_with_all_timepoints, by = "id")

  colnames(d)

  tca_all_results <- tca_all(d)

  data.tca <- tca_all_results[[1]]
  tca.loadings <- tca_all_results[[2]] %>% as.data.frame()
  var.explan<- tca_all_results[[3]] %>% as.data.frame()

  col.fac <- c()

  for (i in 1:ncol(data.tca)) {

    col.fac[i] <- paste0("FC_", i)

  }

  colnames(data.tca) <- col.fac

  # stored_tcam_long(c)

  stored_tcam$data.tca <- data.tca
  stored_tcam$tca.loadings <- tca.loadings
  stored_tcam$var.explan <- var.explan
  stored_tcam$col.fac <- col.fac

})

observeEvent(stored_tcam$col.fac,{
  updateSelectInput(session, inputId = "pcAfc", choices = stored_tcam$col.fac, selected = stored_tcam$col.fac[1])
})

observeEvent(stored_tcam$col.fac,{
  updateSelectInput(session, inputId = "pcBfc", choices = stored_tcam$col.fac, selected = stored_tcam$col.fac[2])
})

observeEvent(stored_tcam$col.fac,{
  updateSelectInput(session, inputId = "pcCfc", choices = stored_tcam$col.fac, selected = stored_tcam$col.fac[3])
})



#### TCAM individual plot ----

output$ind_FC <- renderPlotly({

          a <- catv()
          vars <- c("id", "time", a, "metabolites", "values")

          d <- data()
          d2 <- data()
          ids <- c(colnames(d)[5:ncol(d)])

          patients_with_all_timepoints <- d %>%
            group_by(id) %>%
            filter(all(unique(d$time) %in% time)) %>%
            distinct(id)

          ids <- c("id", "time", colnames(d2[5:ncol(d2)]) )

          d.t1 <- d2 %>% pivot_wider(names_from = "metabolites",
                                        values_from = "values", id_cols = ids) %>%
                            select(c(ids))

          d.t1 <- semi_join(d.t1, patients_with_all_timepoints, by = "id")
          d.t1 <- d.t1 %>% filter(time==unique(d$time)[1])


          data.tca <- cbind(stored_tcam$data.tca, d.t1) %>% as.data.frame()

          req(input$pcAfc)
          req(input$pcBfc)


          dim1 <- input$pcAfc
          dim2 <- input$pcBfc

          vars.fc <- c(dim1, dim2, a, "id")

          data.tca <- data.tca %>%
                          select(all_of(vars.fc))

          data.tca[,a] <- as.factor(data.tca[,a])
          # a <- "gender"

          color <- "black"
          if (length(unique(data.tca[[a]])) < 2) {
            color <- "black"
          }else
          {
            color <- a
          }
          ~as.factor(dt.pltly[[a]])

          tca.plot <- ggscatter(data.tca, x = dim1, y = dim2,
                                fill  = color,
                                shape = color,
                                # label = "id",
                                # repel = FALSE,
                                # size= 5,
                                #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                                ellipse = TRUE, mean.point = TRUE,
                                star.plot = F) +
                      geom_hline(yintercept = 0, lty = 2) +
                      geom_vline(xintercept = 0, lty = 2) +
                      theme_apa() + ggtitle("Individuals - TCAM")

          tca.plot <- ggplotly(tca.plot)

          # TODO add the labels when hovering over the points

          # Fix für Labels

          for (i in 1:length(tca.plot$x$data)){
            if (!is.null(tca.plot$x$data[[i]]$name)){
              tca.plot$x$data[[i]]$name =  gsub("\\(","",str_split(tca.plot$x$data[[i]]$name,",")[[1]][1])
            }
          }

          tca.plot %>% layout(plot_bgcolor  = "#edeff4",
                              paper_bgcolor = "#edeff4",
                              fig_bgcolor   = "#edeff4",
                              legend= list(bgcolor = "#edeff4" ))
})




#### TCAM 3D plot ----

observeEvent(input$add_graph_fc_3d,{

        output$ind_FC_3D <- renderPlotly({

                a <- catv()
                d <- data()
                d2 <- data()

                ids <- c(colnames(d)[5:ncol(d)])

                patients_with_all_timepoints <- d %>%
                  group_by(id) %>%
                  filter(all(unique(d$time) %in% time)) %>%
                  distinct(id)

                ids <- c("id", "time", colnames(d2[5:ncol(d2)]) )

                d.t1 <- d2 %>% pivot_wider(names_from = "metabolites",
                                           values_from = "values", id_cols = ids) %>%
                  select(c(ids))

                d.t1 <- semi_join(d.t1, patients_with_all_timepoints, by = "id")
                d.t1 <- d.t1 %>% filter(time==unique(d$time)[1])


                data.tca <- cbind(stored_tcam$data.tca, d.t1) %>% as.data.frame()


                dim1 <- input$pcAfc
                dim2 <- input$pcBfc
                dim3 <- input$pcCfc

                x <- data.tca[[dim1]]
                y <- data.tca[[dim2]]
                z <- data.tca[[dim3]]

                cat <- as.factor(data.tca[[a]])


                dt.pltly <- cbind(x,y,z, cat) %>% as.data.frame()

                colnames(dt.pltly) <- c("x","y","z",a)

                legend.titel <- colnames(dt.pltly[a])

                # 3D plot

                scene = list(xaxis = list(title = paste0("PC", dim1[1])),
                             yaxis = list(title = paste0("PC", dim2[1])),
                             zaxis = list(title = paste0("PC", dim3[1])),
                             showlegend=TRUE,
                             legend = list(title=list(text= legend.titel)))

                color_factors <- ifelse(is.na(dt.pltly[[a]]), "NA", as.character(dt.pltly[[a]]))

                p <- plot_ly(dt.pltly, x = ~x, y = ~y, z = ~z, color = ~color_factors,
                             colors = c('#BF382A', '#0C4B8E'), type = "scatter3d" , mode = "markers") %>%
                              layout(scene = list(xaxis = list(title = paste0(dim1)),
                                                  yaxis = list(title = paste0(dim2)),
                                                  zaxis = list(title = paste0(dim3)),
                                                  showlegend=TRUE,
                                                  legend = list(title=list(text= legend.titel))),
                                                       plot_bgcolor  = "#edeff4",
                                                       paper_bgcolor = "#edeff4",
                                                       fig_bgcolor   = "#edeff4")

                print(p)



        })

})


#### TCAM loadings ----


output$fc_loadings_long <- renderPlotly({

        a <- catv()
        d <- data()
        d2 <- data()
        ids <- c(colnames(d)[5:ncol(d)])
        d <- d %>% select(-c(ids))
        metaname <- unique(d$metabolites)

        # data.tca <- tca(d)
        tca.loadings <- stored_tcam$tca.loadings %>% as.data.frame()
        tca.loadings <- cbind(tca.loadings, metaname)
        tca.loadings <- tca.loadings %>% column_to_rownames("metaname")

        col.fac <- c()

        for (i in 1:ncol(tca.loadings)) {

          col.fac[i] <- paste0("FC_", i)

        }

        colnames(tca.loadings) <- stored_tcam$col.fac

        req(input$pcAfc)

        dim = input$pcAfc

        tca.loadings.shrt <- tca.loadings %>%
                                select(all_of(dim)) %>%
                                arrange(desc(abs(.))) %>%
                                dplyr::slice(1:15)

         metabolite <-rownames(tca.loadings.shrt)

         tca.loadings.shrt <- cbind(metabolite, tca.loadings.shrt)

         barplot <- ggplot(data=tca.loadings.shrt) +
                            geom_bar(aes(x=.data[[dim]],
                                         y=reorder(metabolite,.data[[dim]])),
                                     fill = "#0072B2",
                                     stat="identity") + theme_apa() +
                            labs(size= "",
                                 x = paste0("Factor loadings: ", dim),
                                 y = "",
                                 title = "")
                            ggplotly(barplot) %>%
                                layout(plot_bgcolor  = "#edeff4",
                                       paper_bgcolor = "#edeff4",
                                       fig_bgcolor   = "#edeff4",
                                       legend= list(bgcolor = "#edeff4" ))
      })



output$report_tcam <- downloadHandler(
  filename = "report_tcam.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(file.path("server", "report_tcam.Rmd"), tempReport, overwrite = TRUE)

    params <- list(df_tcam_RMD = stored_tcam, input_RMD = input, catv_RMD = catv(), data_RMD = data())

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

