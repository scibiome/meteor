#### Metabolite query ####
library(KEGGREST)
output$info_box_enrichment <- renderUI({
  HTML("<p align='justify'>
        This section enables users to conduct a deeper analysis of selected metabolites by getting their comon IDs and identifying pathways where these metabolites are abundant.<br><br>

        <u>When to use</u>:<br>
        If you want to obtain common IDs, identify pathways with high abundance, perform functional analysis using the KEGG website, and save your results.<br><br>

        <u>Additional Information:</u><br>
        Disclaimer: The API of MetaboAnalyst is currently not available, so we used this workaround.
        For a step-by-step tutorial on the enrichment analysis, refer to <a href='https://scibiome.github.io/meteor/articles/Tutorial.html' target='_blank'>Tutorial</a>.<br><br>

        In step 1, the currently selected metabolites are displayed. Copy these metabolites and visit
        <a href='https://www.metaboanalyst.ca/MetaboAnalyst/upload/ConvertView.xhtml' target='_blank'>MetaboAnalyst</a>. Paste the metabolites into the provided form and click 'Submit' to perform the metabolite ID conversion.
        The 'Compound Name/ID Standardization' will be shown. At the bottom of the page, click the 'here' button to download the results in CSV format:<br><br>

        <pre>\"Query\",\"Match\",\"HMDB\",\"PubChem\",\"ChEBI\",\"KEGG\",\"METLIN\",\"SMILES\",\"Comment\"<br>\"1,3-Diaminopropane\",\"1,3-Diaminopropane\",\"HMDB0000002\",\"428\",\"15725\",\"C00986\",\"5081\",\"NCCCN\",\"1\"<br>\"2-Ketobutyric acid\",\"2-Ketobutyric acid\",\"HMDB0000005\",\"58\",\"30831\",\"C00109\",NA,\"CCC(=O)C(O)=O\",\"1\"</pre><br><br>

        Copy the content and return to MeTEor to paste it into the form of step 2.

        In the 3 step, click 'Get enriched pathways' to receive a list of pathways where your selected metabolites are present, ranked by occurrence..<br><br>

        Each pathway includes outlinks to the interactive KEGG pathway with highlighted metabolites in red,
        facilitating deeper analysis and functional understanding of the measured metabolites.
      </p>")
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
stored_enriched_kegg_pathways <- reactiveValues(data = NULL)


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

              # browser()




              body <- list(analytes = paste0("hmdb:", query_results.df$HMDB))
              # Define the body
              body <- list(analytes = c("hmdb:HMDB0000641", "hmdb:HMDB0000067", "hmdb:HMDB0000161"))

              # The MetaboAnalyst API url
              call <- "https://rampdb.nih.gov/api/pathways-from-analytes"

              # Use httr::POST to send the request to the MetaboAnalyst API
              # The response will be saved in query_results
              query_results <- httr::POST(call, body = body, encode = "json")

              # Parse the response into a table
              # Will show mapping to "hmdb_id", "kegg_id", "pubchem_id", "chebi_id", "metlin_id", "smiles"
              query_results_json <- httr::content(query_results, "text", encoding = "UTF-8")
              query_results_parsed <- jsonlite::fromJSON(query_results_json, flatten = TRUE)

              query_results_table <- query_results_parsed$data %>%
                group_by(pathwayId, pathwayName, inputId) %>%
                summarize(num_hits = n())



              enriched_kegg_pathways$url <- sprintf('<a href="%s" target="_blank">%s</a>', enriched_kegg_pathways$url, "KEGG")
              enriched_kegg_pathways <- enriched_kegg_pathways[order(enriched_kegg_pathways$num_hits, decreasing = TRUE), ]
              DT_pathways(datatable(enriched_kegg_pathways, editable = TRUE,
                        extensions = "Buttons",
                        options = list(paging = TRUE,
                                       scrollX=TRUE,
                                       searching = TRUE,
                                       # ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c("copy", "print", "csv", "excel")
                        ), escape = FALSE
                        ))

              # enriched kegg pathways
              stored_enriched_kegg_pathways$data <- enriched_kegg_pathways



              # Function to generate URLs
              generate_url <- function(kegg_id) {
                if (!is.na(kegg_id)) {
                  return(sprintf('<a href="https://www.genome.jp/dbget-bin/www_bget?cpd:%s" target="_blank">%s</a>', kegg_id, kegg_id))
                } else {
                  return("-")
                }
              }
              query_results.df$KEGG <- sapply(query_results.df$KEGG, generate_url)

              stored_enriched_kegg_pathways$query_results.df <- query_results.df

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

output$pathways.dt <-   renderDT({DT_pathways()})

output$report_query <- downloadHandler(
  filename = "report_query.html",
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("~/PycharmProjects/meteor_github/inst/my_app/server/query_report.Rmd", tempReport, overwrite = TRUE)

    params <- list(DT_pathways_RMD = DT_pathways(), input_RMD = input, stored_enriched_kegg_pathways_RMD = stored_enriched_kegg_pathways, raw_list_RMD = raw_list)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)
