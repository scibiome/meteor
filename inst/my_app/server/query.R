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

  raw_list <- data.frame(metabolites = raw_list$data$metabolites)})

DT_pathways <- reactiveVal()

output$export.dt <-   renderDT({

              # First create a list containing a vector of the compounds to be queried (separated by a semi-colon)
              # and another character vector containing the compound id type.
              # The items in the list MUST be queryList and inputType
              # Valid input types are: "name", "hmdb", "kegg", "pubchem", "chebi", "metlin"

              # raw_list <-  raw_list.event()
              # browser()
              # name.vec <- as.vector(raw_list$metabolites)
              #
              #
              #
              # toSend = list(queryList = paste(name.vec, collapse = ";"), inputType = "name")

              # toSend =   list(queryList = "1,3-Diaminopropane;2-Ketobutyric acid;2-Hydroxybutyric acid;2-Methoxyestrone;",
              #   inputType= "name")
              #
              #
              # # The MetaboAnalyst API url
              # call <- "https://rest.xialab.ca/api/mapcompounds"
              #
              # # Use httr::POST to send the request to the MetaboAnalyst API
              # # The response will be saved in query_results
              # query_results <- httr::POST(call, body = toSend, encode = "json")
              #
              # # Parse the response into a table
              # # Will show mapping to "hmdb_id", "kegg_id", "pubchem_id", "chebi_id", "metlin_id", "smiles"
              # query_results_text <- httr::content(query_results, "text", encoding = "UTF-8")
              # query_results_json <- jsonlite::fromJSON(query_results_text, flatten = TRUE)
              # #
              # #
              # query_results.df <- t(do.call(rbind.data.frame, query_results_json)) %>%
              #   as.data.frame()
              #
              # # dummy_data <- data.frame(
              # #   Query = c("1,3-Diaminopropane", "2-Ketobutyric acid", "2-Hydroxybutyric acid",
              # #             "2-Methoxyestrone", "(R)-3-Hydroxybutyric acid", "Deoxyuridine",
              # #             "Cortexolone", "Deoxycorticosteron", "Ketoisovaleric acid", "No Match"),
              # #   Match = c("1,3-Diaminopropane", "2-Ketobutyric acid", "2-Hydroxybutyric acid",
              # #             "2-Methoxyestrone", "3-Hydroxybutyric acid", "Deoxyuridine",
              # #             "Cortexolone", NA, NA, NA),
              # #   HMDB = c("HMDB0000002", "HMDB0000005", "HMDB0000008", "HMDB0000010",
              # #            "HMDB0000011", "HMDB0000012", "HMDB0000015", NA, NA, NA),
              # #   PubChem = c("428", "58", "440864", "440624", "441", "13712", "440707", NA, NA, NA),
              # #   ChEBI = c("15725", "30831", "50613", "1189", "17066", "16450", "28324", NA, NA, NA),
              # #   KEGG = c("C00986", "C00109", "C05984", "C05299", "C01089", "C00526", "C05488", NA, NA, NA),
              # #   METLIN = c("5081", NA, "3783", "2578", NA, "5086", "5088", NA, NA, NA),
              # #   SMILES = c("NCCCN", "CCC(=O)C(O)=O", "CC[C@H](O)C(O)=O",
              # #              "[H][C@@]12CCC(=O)[C@@]1(C)CC[C@]1([H])C3=C(CC[C@@]21[H])C=C(O)C(OC)=C3",
              # #              "C[C@@H](O)CC(O)=O", "OC[C@H]1O[C@H](C[C@@H]1O)N1C=CC(=O)NC1=O",
              # #              "[H][C@@]12CC[C@](O)(C(=O)CO)[C@@]1(C)CC[C@@]1([H])[C@@]2([H])CCC2=CC(=O)CC[C@]12C",
              # #              NA, NA, NA),
              # #   Comment = c("1", "1", "1", "1", "1", "1", "1", "0", "0", "0")
              # # )
              # # query_results.df <- dummy_data
              #
              # ### testing the pathview
              #
              # # dummy_data$KEGG
              # # https://www.genome.jp/kegg/webapp/color_url.html
              #
              #
              # rownames(query_results.df) <- NULL
              # colnames(query_results.df) <- names(query_results_json)
              #
              # body <- list(analytes = paste0("hmdb:", query_results.df$HMDB))
              # # Define the body
              # body <- list(analytes = c("hmdb:HMDB0000641", "hmdb:HMDB0000067", "hmdb:HMDB0000161"))
              # browser()
              # body <- list(analytes = c("kegg:C00986", "kegg:C00109", "kegg:C05984","kegg:C05299", "kegg:C01089", "kegg:C00526", "kegg:C00083"))
              metabolites <- c("kegg:C00986", "kegg:C00109", "kegg:C05984", "kegg:C05299", "kegg:C01089", "kegg:C00526", "kegg:C00083")

              requests <- keggGet(metabolites)


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

              # Print the data frame
              # print(result_df)

              #
              # # browser()
              # # body <- list(analytes = c("kegg:C01089", "kegg:C00526", "kegg:C00083"))
              # # The MetaboAnalyst API url
              # call <- "https://rampdb.nih.gov/api/pathways-from-analytes"
              #
              # # Use httr::POST to send the request to the MetaboAnalyst API
              # # The response will be saved in query_results
              # query_results <- httr::POST(call, body = body, encode = "json")
              # #
              # # # Check if response is ok (TRUE)
              # # # 200 is ok! 401 means an error has occurred on the user's end.
              # query_results$status_code == 200
              # #
              # # # Parse the response into a table
              # # # Will show mapping to "hmdb_id", "kegg_id", "pubchem_id", "chebi_id", "metlin_id", "smiles"
              # query_results_json <- httr::content(query_results, "text", encoding = "UTF-8")
              # query_results_parsed <- jsonlite::fromJSON(query_results_json, flatten = TRUE)
              #
              #
              #
              # if (length(query_results_parsed$data) == 0) {
              #   show_alert(
              #     title = NULL,
              #     text = tags$span(
              #       tags$h3("Error",
              #               style = "color: steelblue;"),
              #       "No matches found. Please edit metabolite names in the datatable."
              #     ),
              #     html = TRUE
              #   )
              #   return()
#               }
              # library(dplyr)
              # query_results_parsed$data <- subset(query_results_parsed$data, grepl("kegg", pathwaySource, ignore.case = TRUE))

              # Group the data by pathway name and input ID, and count the number of hits
              query_results_table <- retrieved_pathways_df %>%
                group_by(pathwayId, pathwayName, inputId) %>%
                summarize(num_hits = n())

              # Group the data by pathway name and count the number of input IDs
              query_results_table_summary <- query_results_table %>%
                group_by(pathwayId, pathwayName) %>%
                summarize(num_hits = n(), inputIds = paste(inputId, collapse = ", "))

              browser()
              # Order the pathways by the number of hits
              query_results_table_sorted <- query_results_table_summary[order(-query_results_table_summary$num_hits),]



              generate_kegg_url <- function(pathwayId, inputIds) {
                # Remove "kegg:" prefix and split compound IDs
                compound_ids <- unlist(strsplit(gsub("kegg:", "", inputIds), ", "))

                # Generate color-coded compound IDs
                colored_compound_ids <- paste0(compound_ids, "%20red%20red")

                # Construct the URL template with colored compound IDs
                url_template <- paste0("https://www.kegg.jp/kegg-bin/show_pathway?", pathwayId, "/", paste(colored_compound_ids, collapse = "/"))

                return(url_template)
              }

              # Apply the function to each row in the dataframe
              urls <- mapply(generate_kegg_url, query_results_table_sorted$pathwayId, query_results_table_sorted$inputIds)
              pathwayIDs <- gsub(" .*", "", names(urls))
              urls <- unname(urls)

              # Create a dataframe
              urls_df <- data.frame(pathwayId = pathwayIDs, url = urls)
              # Print the generated URLs
              # print(urls)

              # browser()
              # Print the top 10 pathways with the most hits, along with the number of input IDs and the input IDs themselves
              # head(query_results_table_sorted %>%
              #        mutate(inputIds = str_remove(inputIds, "hmdb:")), n = 10)
              query_results_table_sorted <- query_results_table_sorted %>%
                mutate(inputIds = gsub("kegg:", "", inputIds))



              dict1 <- setNames(retrieved_pathways_df$commonName, retrieved_pathways_df$inputId)

              # loop through each row of the table
              for (i in seq_len(nrow(query_results_table_sorted))) {

                # split the inputIds column by comma
                inputIds <- strsplit(query_results_table_sorted$inputIds[i], ", ")[[1]]

                # replace each HMDB code with the corresponding value from the dictionary
                inputIds <- sapply(inputIds, function(x) ifelse(x %in% names(dict1), dict1[x], x))

                # combine the modified inputIds back into a comma-separated string
                query_results_table_sorted$inputIds[i] <- paste(inputIds, collapse = ", ")
              }
              # browser()

              merged_df <- merge(query_results_table_sorted, urls_df, by = "pathwayId", all.x = TRUE)
              query_results_table_sorted <- merged_df
              colnames(query_results_table_sorted) <- c('pathwayID', 'pathwayName', 'num_hits', 'Matches', 'url')

              query_results_table_sorted$url <- sprintf('<a href="%s" target="_blank">%s</a>', query_results_table_sorted$url, "KEGG")

              DT_pathways(datatable(query_results_table_sorted, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        ), escape = FALSE
                        ))



              datatable(retrieved_pathways_df, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        ))


})

output$pathways.dt <-   renderDT({DT_pathways()
  })
