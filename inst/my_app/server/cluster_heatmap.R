###### Cluster Heatmap #####

output$info_box_sm <- renderUI({
  HTML("<p align='justify'>
        The Cluster Heatmap tool allows you to visualize and cluster metabolite data across different time points for a single metabolite.
        The tool generates a heatmap plot where each row represents a single observation,
        and each column represents the selected metabolite at a specific time point. The data can be clustered based on the metabolite and observation axis.<br><br>

        <u>When to use:</u><br>
        This tool is ideal if you need to analyze and visualize the temporal variation of a single metabolite across multiple time points,
        allowing for the identification of patterns and clusters over time.<br><br>

        <u>Additional Information:</u><br>
        The plot can be customized by changing the number of clusters for both rows and columns and can be downloaded as a PNG image by right-clicking on it.
        This customization helps in fine-tuning the clustering results to better understand the data structure and relationships.<br><br>

        To learn more about cluster heatmaps,
        refer to Zhao, S. et al.
        <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4124803/' target='_blank'>Advanced Heat Map and Clustering Analysis Using Heatmap3</a> (2014).
        Additionally, you can explore this
        <a href='https://davetang.org/muse/2018/05/15/making-a-heatmap-in-r-with-the-pheatmap-package/' target='_blank'>tutorial on creating heatmaps with the pheatmap package</a>.


      </p>")
})

output$info_box_tb <- renderUI({
  HTML("<p align='justify'>
        The Cluster Heatmap tool allows you to visualize and cluster metabolite data across different time points for multiple metabolites.
        The tool generates a heatmap plot where each row represents a single observation,
        and each column represents the metabolites at the selected timepoint.<br><br>

        <u>When to use:</u><br>
        This tool is ideal if you need to analyze and visualize the variation of multiple metabolites across a single time point,
        providing insights into the relationships and patterns among different metabolites at that time.<br><br>

        <u>Additional Information:</u><br>
        The plot can be customized by changing the number of clusters for both rows and columns and can be downloaded as a PNG image by right-clicking on it.
        This feature allows for a detailed exploration of the metabolite data and helps in identifying significant clusters and patterns.

                To learn more about cluster heatmaps,
        refer to Zhao, S. et al.
        <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4124803/' target='_blank'>Advanced Heat Map and Clustering Analysis Using Heatmap3</a> (2014).
        Additionally, you can explore this
        <a href='https://davetang.org/muse/2018/05/15/making-a-heatmap-in-r-with-the-pheatmap-package/' target='_blank'>tutorial on creating heatmaps with the pheatmap package</a>.

      </p>")
})


observe({

    req(data())
    dat.nam <- data()
    metabolite.names <- unique(dat.nam[,3]) %>% as.list()

    updateMultiInput(session, "id6", choices = metabolite.names, selected = metabolite.names)

})

heatmap_plot_stored <- reactiveValues(computation_done_plot = FALSE, computed_data_plot = NULL)


observeEvent(input$clustrow1, {

  output$clustheatmap <- renderImage({

                    a <- catv()

                    vars <- c("id", "time", a, "metabolites", "values")
                    var <- c(a)


                    ids <- c("id", a)

                    data() %>% #filter(metabolites %in% c("meso_erythritol")) %>%
                      filter(id == 24)


                    data1 <- data() %>%
                              as.data.frame() %>%
                              dplyr::select(all_of(vars)) %>%
                              # mutate(.[[3]] <- as.factor(.[[3]])) %>%
                              filter(metabolites %in% input$id6) %>%
                              select(-metabolites) %>%
                              pivot_wider( names_from = "time",
                                           values_from = "values",
                                           id_cols = ids) %>%
                              column_to_rownames(var="id")

                    cases_not_all_timepoints <- (data1[!complete.cases(data1), ])
                    print("39 cases do not have all data") # TODO noch besser umsetzen?
                    nrow(cases_not_all_timepoints)
                    print(cases_not_all_timepoints)
                    data1 <- data1[complete.cases(data1), ]

                    data2 <- data1[,-1]

                    req(input$clustrow1)
                    req(input$clustcol1)

                    res <- pheatmap(data2)

                    my_pat_row <- cutree(res$tree_row, k = input$clustrow1)

                    clust.name = c()

                    for(i in 1:length(my_pat_row)) {
                      clust.name[i] <- paste0("Cluster ", my_pat_row[i])
                    }

                    clust.name <- cbind(my_pat_row, clust.name, data1[,1])
                    clust.name <- clust.name[,-1] %>% as.data.frame()
                    colnames(clust.name) <- c("cluster rows", a)

                    clust.name <- clust.name %>% mutate_all(~(as.factor(.)))

                    my_pat_col <- cutree(res$tree_col, k = input$clustcol)

                    clust.name.col = c()
                    for(i in 1:length(my_pat_col)) {
                      clust.name.col[i] <- paste0("Cluster ", my_pat_col[i])
                    }
                    clust.name.col <- as.data.frame(clust.name.col)
                    colnames(clust.name.col) <- c("cluster cols")

                    clust.name.col <- clust.name.col %>% mutate_all(~(as.factor(.)))


                    map <- pheatmap(data2, annotation_row = clust.name,
                                    annotation_col = clust.name.col,
                                    fontsize_row	= 3,
                                    cutree_rows = input$clustrow1,
                                    cutree_cols = input$clustcol1)

                    # A temp file to save the output.
                    # This file will be removed later by renderImage
                    outfile <- tempfile(fileext = '.png')

                    # Generate the PNG
                    png(outfile,  width = 30, height = 25, res = 600, units = "cm")
                    grid.draw(rectGrob(gp=gpar(fill="#edeff4", lwd=0)))
                    grid.draw(map)
                    dev.off()

                    # Return a list containing the filename
                    list(src = outfile,
                         contentType = 'image/png',
                         width = 1400,
                         height = 1000,
                         alt = "This is alternate text")
                  }, deleteFile = TRUE)

})

output$report_clusterheatmap1 <- downloadHandler(
  filename = "report_clusterheatmap1.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(file.path("server", "cluster_heatmap1_report.Rmd"), tempReport, overwrite = TRUE)

    params <- list(data_RMD = data(), input_RMD = input, catv_RMD = catv())


    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


observe({
    req(data())
    dat.nam <- data()
    metabolite.names <- unique(dat.nam[,3]) %>% as.list()

    updateMultiInput(session, "id7", choices = metabolite.names, selected = metabolite.names[1:4])
})


observeEvent(input$heatmap_compute, {
  a <- catv()

  vars <- c("id", "time", "metabolites", "values")

  ids <- c("id")

  heatmap_plot_stored$computed_data_plot <- data() %>%
    as.data.frame() %>%
    dplyr::select(all_of(vars)) %>%
    mutate(.[[3]] <- as.factor(.[[3]])) %>%
    filter(metabolites %in% ui_metabolites$selection) %>%
    filter(time %in% input$timepoint) %>%
    select(-time) %>%
    pivot_wider( names_from = "metabolites",
                 values_from = "values",
                 id_cols = ids) %>%
    column_to_rownames(var="id")


  heatmap_plot_stored$computation_done_plot <- TRUE
})



observeEvent(input$timepoint, {

  output$clustheatmap2 <- renderPlotly({
    if(heatmap_plot_stored$computation_done_plot)
    {
      if (nrow(heatmap_plot_stored$computed_data_plot) == 0) {
        show_alert(
          title = NULL,
          text = tags$span(
            tags$h3("Error",
                    style = "color: steelblue;"
            ),
            "No metabolites selected!"
          ),
          html = TRUE
        )
        return()
      }


          gg_back_box <- theme(
                          panel.background = element_rect(fill = "#edeff4"),
                          plot.background = element_rect(fill = "#edeff4"),
                          legend.background = element_rect(fill = "#edeff4")
          )
          heatmaply(
            heatmap_plot_stored$computed_data_plot,
            k_col = input$clustcol,
            k_row = input$clustrow,
            heatmap_layers = gg_back_box )


  }})
})


output$report_clusterheatmap2 <- downloadHandler(
  filename = "report_clusterheatmap2.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("~/PycharmProjects/meteor_github/inst/my_app/server/cluster_heatmap2_report.Rmd", tempReport, overwrite = TRUE)

    params <- list(heatmap_plot_stored_RMD = heatmap_plot_stored, input_RMD = input)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)
