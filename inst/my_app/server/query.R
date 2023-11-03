#### Metabolite query ####

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




observeEvent(input$metabolite_Picker, {

  output$raw_list.dt <- renderDT({

    d <- data.frame(metabolites = raw_list$data$metabolites)

    d <- datatable(d, editable = TRUE)

  })   })


observeEvent(input$raw_list.dt_cell_edit, {
  d <- raw_list$data
  tmp <- d
  row <- tmp$row[input$raw_list.dt_cell_edit$row]
  raw_list$data[row, input$raw_list.dt_cell_edit$col] <- input$raw_list.dt_cell_edit$value
})


raw_list.event <- eventReactive(input$api.go, {

  raw_list <- data.frame(metabolites = raw_list$data$metabolites)})

DT_pathways <- reactiveVal()

output$export.dt <-   renderDT({

              # First create a list containing a vector of the compounds to be queried (separated by a semi-colon)
              # and another character vector containing the compound id type.
              # The items in the list MUST be queryList and inputType
              # Valid input types are: "name", "hmdb", "kegg", "pubchem", "chebi", "metlin"

              raw_list <-  raw_list.event()

              name.vec <- as.vector(raw_list$metabolites)

              toSend = list(queryList = paste(name.vec, collapse = ";"), inputType = "name")

              # toSend =   list(queryList = "1,3-Diaminopropane;2-Ketobutyric acid;2-Hydroxybutyric acid;2-Methoxyestrone;",
              #   inputType= "name")

              # The MetaboAnalyst API url
              call <- "https://www.xialab.ca/api/mapcompounds"

              # Use httr::POST to send the request to the MetaboAnalyst API
              # The response will be saved in query_results
              query_results <- httr::POST(call, body = toSend, encode = "json")

              # Check if response is ok (TRUE)
              # 200 is ok! 401 means an error has occured on the user's end.
              query_results$status_code==200

              # Parse the response into a table
              # Will show mapping to "hmdb_id", "kegg_id", "pubchem_id", "chebi_id", "metlin_id", "smiles"
              query_results_text <- httr::content(query_results, "text", encoding = "UTF-8")
              query_results_json <- jsonlite::fromJSON(query_results_text, flatten = TRUE)



              query_results.df <- t(do.call(rbind.data.frame, query_results_json)) %>%
                as.data.frame()

              rownames(query_results.df) <- NULL
              colnames(query_results.df) <- names(query_results_json)

              body <- list(analytes = paste0("hmdb:", query_results.df$HMDB))
              # Define the body
              # body <- list(analytes = c("hmdb:HMDB0000641", "hmdb:HMDB0000067", "hmdb:HMDB0000161"))

              # The MetaboAnalyst API url
              call <- "https://rampdb.nih.gov/api/pathways-from-analytes"

              # Use httr::POST to send the request to the MetaboAnalyst API
              # The response will be saved in query_results
              query_results <- httr::POST(call, body = body, encode = "json")

              # Check if response is ok (TRUE)
              # 200 is ok! 401 means an error has occurred on the user's end.
              query_results$status_code == 200

              # Parse the response into a table
              # Will show mapping to "hmdb_id", "kegg_id", "pubchem_id", "chebi_id", "metlin_id", "smiles"
              query_results_json <- httr::content(query_results, "text", encoding = "UTF-8")
              query_results_parsed <- jsonlite::fromJSON(query_results_json, flatten = TRUE)

              # Group the data by pathway name and input ID, and count the number of hits
              query_results_table <- query_results_parsed$data %>%
                group_by(pathwayId, pathwayName, inputId) %>%
                summarize(num_hits = n())

              # Group the data by pathway name and count the number of input IDs
              query_results_table_summary <- query_results_table %>%
                group_by(pathwayId, pathwayName) %>%
                summarize(num_hits = n(), inputIds = paste(inputId, collapse = ", "))

              # Order the pathways by the number of hits
              query_results_table_sorted <- query_results_table_summary[order(-query_results_table_summary$num_hits),]
              # browser()
              # Print the top 10 pathways with the most hits, along with the number of input IDs and the input IDs themselves
              # head(query_results_table_sorted %>%
              #        mutate(inputIds = str_remove(inputIds, "hmdb:")), n = 10)
              query_results_table_sorted <- query_results_table_sorted %>%
                mutate(inputIds = gsub("hmdb:", "", inputIds))




              # dict <- setNames(as.list(query_results.df$Match), query_results.df$HMDB)

              dict <- setNames(query_results.df$Match, query_results.df$HMDB)

              # loop through each row of the table
              for (i in seq_len(nrow(query_results_table_sorted))) {

                # split the inputIds column by comma
                inputIds <- strsplit(query_results_table_sorted$inputIds[i], ", ")[[1]]

                # replace each HMDB code with the corresponding value from the dictionary
                inputIds <- sapply(inputIds, function(x) ifelse(x %in% names(dict), dict[x], x))

                # combine the modified inputIds back into a comma-separated string
                query_results_table_sorted$inputIds[i] <- paste(inputIds, collapse = ", ")
              }


              colnames(query_results_table_sorted) <- c('pathwayID', 'pathwayName', 'num_hits', 'Matches')

              DT_pathways(datatable(query_results_table_sorted, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        )))



              datatable(query_results.df, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        ))


})

output$pathways.dt <-   renderDT({DT_pathways()})
