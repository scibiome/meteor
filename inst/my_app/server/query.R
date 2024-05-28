#### Metabolite query ####
library(KEGGREST)
output$info_box_enrichment <- renderUI({

  HTML("<p align = 'justify'>
        This section enables users to perform database queries for the selected metabolites.
        The top table displays the names of the chosen metabolites,
        which can be edited directly within the table.
        To initiate the analysis, users can send a query to the MetaboAnalyst API,
        which will generate a list of different identifiers for each metabolite,
        including HMDB, PubChem, ChEBI, KEGG, METLIN, and SMILES.
        <p></p>
        Next, users can leverage the HMDB identifier to
        perform pathway enrichment analysis using the Relational Database of Metabolomics Pathways (RaMP) [1].
        This analysis will return information about the pathways that involve the selected metabolites.
        <p></p>
        [1] Zhang, B., Hu, S., Baskin, E., Patt, A., Siddiqui, J. K., & Mathé, E. A. (2018).
              RaMP: A Comprehensive Relational Database of Metabolomics Pathways for Pathway Enrichment Analysis of Genes and Metabolites.
              Metabolites, 8(1), 16. https://doi.org/10.3390/metabo8010016
       ")
})





raw_list <- reactiveValues(data = NULL)

observeEvent(input$metabolite_Picker, {
  raw_list$data <- data.frame(metabolites = input$metabolite_Picker, row =row_number(input$metabolite_Picker)  )
})



# here
observeEvent(input$metabolite_Picker, {

  output$raw_list.dt <- renderDT({

    d <- data.frame(metabolites = raw_list$data$metabolites)

    d <- datatable(d, editable = TRUE)



  })   })


observeEvent(input$raw_list.dt_cell_edit, {
  d <- raw_list$data
  tmp <- d
  row <- input$raw_list.dt_cell_edit$row
  raw_list$data[row, input$raw_list.dt_cell_edit$col] <- input$raw_list.dt_cell_edit$value
})


raw_list.event <- eventReactive(input$api.go, {
  raw_list <- data.frame(metabolites = input$jsonInput)
  })

DT_pathways <- reactiveVal()

observeEvent(input$metabolite_Picker, {
    updateTextAreaInput(session, "editableField", value = paste(raw_list$data$metabolites, collapse = "\n"))
  })



output$export.dt <-   renderDT({

              raw_list <-  raw_list.event()

              query_results.df <- as.vector(raw_list$metabolites)


              data_string <- gsub("\" \"", "\"\n\"", query_results.df)
              # browser()
              query_results.df <- read.csv(textConnection(data_string), stringsAsFactors = FALSE)

              add_kegg_prefix <- function(metabolite) {
                if (!is.na(metabolite)) {
                  return(paste0("kegg:", metabolite))
                } else {
                  return(NA)
                }
              }

              # Apply the function to each element in the list
              prefixed_metabolites <- sapply(query_results.df$KEGG, add_kegg_prefix)
              browser()

              metabolites <- query_results.df$KEGG
              requests <- keggGet(sapply(query_results.df$KEGG, add_kegg_prefix))


              # Initialize empty lists to store data
              pathway_list <- c()
              pathway_name_list <- c()
              name_list <- c()
              entry_list <- c()

              # Iterate through each request
              for (req in requests) {
                # Iterate through each entry in PATHWAY of the current request
                for (i in seq_along(req$PATHWAY)) {
                  # Extract pathway and pathway name
                  pathway <- names(req$PATHWAY)[i]
                  pathway_name <- req$PATHWAY[[i]]

                  # Append to lists
                  pathway_list <- c(pathway_list, pathway)
                  pathway_name_list <- c(pathway_name_list, pathway_name)

                  # Append Name and Entry from the current request
                  name_list <- c(name_list, gsub(";", "", req$NAME[1]))
                  entry_list <- c(entry_list, req$ENTRY[1])
                }
              }

              # Create a data frame
              retrieved_pathways_df <- data.frame(
                pathwayId = pathway_list,
                pathwayName = pathway_name_list,
                commonName = name_list,
                inputId = entry_list
              )

              # Group the data by pathway name and input ID, and count the number of hits
              query_results_table <- retrieved_pathways_df %>%
                group_by(pathwayId, pathwayName, inputId) %>%
                summarize(num_hits = n())

              # Group the data by pathway name and count the number of input IDs
              query_results_table_summary <- query_results_table %>%
                group_by(pathwayId, pathwayName) %>%
                summarize(num_hits = n(), inputIds = paste(inputId, collapse = ", "))


              # Order the pathways by the number of hits
              enriched_kegg_pathways <- query_results_table_summary[order(-query_results_table_summary$num_hits),]

              generate_kegg_url <- function(pathwayId, inputIds) {
                # Remove "kegg:" prefix and split compound IDs
                compound_ids <- unlist(strsplit(gsub("kegg:", "", inputIds), ", "))

                # Generate color-coded compound IDs
                colored_compound_ids <- paste0(compound_ids, "%20red%20red")

                # Construct the URL template with colored compound IDs
                url_template <- paste0("https://www.kegg.jp/kegg-bin/show_pathway?", pathwayId, "/", paste(colored_compound_ids, collapse = "/"))

                return(url_template)
              }

              urls <- mapply(generate_kegg_url, enriched_kegg_pathways$pathwayId, enriched_kegg_pathways$inputIds)
              pathwayIDs <- gsub(" .*", "", names(urls))
              urls <- unname(urls)

              urls_df <- data.frame(pathwayId = pathwayIDs, url = urls)

              enriched_kegg_pathways <- enriched_kegg_pathways %>%
                mutate(inputIds = gsub("kegg:", "", inputIds))

              dict1 <- setNames(retrieved_pathways_df$commonName, retrieved_pathways_df$inputId)

              # loop through each row of the table
              for (i in seq_len(nrow(enriched_kegg_pathways))) {

                # split the inputIds column by comma
                inputIds <- strsplit(enriched_kegg_pathways$inputIds[i], ", ")[[1]]

                # replace each HMDB code with the corresponding value from the dictionary
                inputIds <- sapply(inputIds, function(x) ifelse(x %in% names(dict1), dict1[x], x))

                # combine the modified inputIds back into a comma-separated string
                enriched_kegg_pathways$inputIds[i] <- paste(inputIds, collapse = ", ")
              }

              merged_df <- merge(enriched_kegg_pathways, urls_df, by = "pathwayId", all.x = TRUE)
              enriched_kegg_pathways <- merged_df
              colnames(enriched_kegg_pathways) <- c('pathwayID', 'pathwayName', 'num_hits', 'Matches', 'url')

              enriched_kegg_pathways$url <- sprintf('<a href="%s" target="_blank">%s</a>', enriched_kegg_pathways$url, "KEGG")

              DT_pathways(datatable(enriched_kegg_pathways, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        ), escape = FALSE
                        ))






              # Function to generate URLs
              generate_url <- function(kegg_id) {
                if (!is.na(kegg_id)) {
                  return(sprintf('<a href="https://www.genome.jp/dbget-bin/www_bget?cpd:%s" target="_blank">%s</a>', kegg_id, kegg_id))
                } else {
                  return("-")
                }
              }
              query_results.df$url <- sapply(query_results.df$KEGG, generate_url)


              datatable(query_results.df, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        ), escape = FALSE)


})

output$pathways.dt <-   renderDT({DT_pathways()
  })
