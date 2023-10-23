server <- function(input, output, session) {
  
  output$cluster <- renderUI({
    path <- "./www/umap/"
    path_sep <- "/"
    cluster_files_glob <- Sys.glob(paste0(path, "*UMAP.txt"))
    
    do.call(tabsetPanel, lapply(cluster_files_glob, function(fname){
      cluster_file <- fread(fname, sep="\t")
      cluster_file <- as.data.frame(cluster_file)

      tiss_chip <- unlist(str_split(fname, path_sep))
      tiss_chip <- unlist(str_split(tiss_chip[length(tiss_chip)], "__"))[1]
      
      coordinates <- cluster_file[, c("col0", "col1")]

      nodes <- data.frame(
        id=cluster_file$V1,
        shape=cluster_file$type,
        x=cluster_file$col0 * 1000,
        y=cluster_file$col1 * 1000,
        size=unlist(lapply(cluster_file$type, function(x) if(x=="circle") {return(50)} else {return(30)})),
        font.size=30,
        title=apply(cluster_file, 1, function(cluster_file_row){
          paste0("<b>", cluster_file_row["chemical"], " (", cluster_file_row["mode"], ")</b><br>dose: ", cluster_file_row["dose"], "<br>duration: ", cluster_file_row["duration"], "<br>(", round(as.numeric(cluster_file_row["col0"]), 4), ", ", round(as.numeric(cluster_file_row["col1"]), 4), ")")
        }),
        label="",
        group=cluster_file$chemical
      )
      edges <- data.frame()

      groups <- lapply(unique(cluster_file$chemical), function(cluster_chemical){
        list(
          label=cluster_chemical,
          shape="square"
        )
      })
      
      options <- . %>%
        visOptions(autoResize=TRUE, highlightNearest=list(enabled=TRUE, hover=FALSE)) %>%
        visInteraction(navigationButtons=TRUE, dragNodes=FALSE, dragView=TRUE, zoomView=TRUE, tooltipDelay=0, selectable=FALSE) %>%
        visPhysics(stabilization=FALSE) %>%
        visExport()
      
      g <- visNetwork(nodes, edges) %>% 
        options %>%
        visNodes(fixed=TRUE)
        
      
      res_filter_cluster <- callModule(
        module = selectizeGroupServer,
        id=paste0(tiss_chip, "__selectize"),
        data = cluster_file,
        vars = c("chemical", "dose", "duration", "mode")
      )
      
      reac_res_filter_cluster <- reactive({res_filter_cluster()})

      tp <- tabPanel(tiss_chip, div(
        fluidRow(
          column(12,
            selectizeGroupUI(
              id=paste0(tiss_chip, "__selectize"),
              inline = TRUE,
              params = list(
                var_chemical = list(inputId = "chemical", title = "Chemical", placeholder = 'select chemical'),
                var_dose = list(inputId = "dose", title = "Dose", placeholder = 'select dose'),
                var_time = list(inputId = "duration", title = "Time", placeholder = 'select time'),
                var_mp = list(inputId = "mode", title = "Measured/Predicted", placeholder = 'select measured/predicted')
              ),
              label="Filter nodes",
            )
          )
        ),
        fluidRow(
          column(12,
            visNetworkOutput(outputId=paste0(tiss_chip, "__network_proxy"), width="100%", height="1000px") %>% withSpinner()
          )
        )
      ))
      
      output[[paste0(tiss_chip, "__network_proxy")]] <- renderVisNetwork(g)
      
      # Observers
      observe({
        selected_chemicals <- input[[paste0(tiss_chip, "__selectize-chemical")]]
        selected_doses <- input[[paste0(tiss_chip, "__selectize-dose")]]
        selected_durations <- input[[paste0(tiss_chip, "__selectize-duration")]]
        selected_mp <- input[[paste0(tiss_chip, "__selectize-mode")]]
        
        if(is.null(selected_chemicals)){
          selected_chemicals <- cluster_file$chemical
        }
        
        if(is.null(selected_doses)){
          selected_doses <- cluster_file$dose
        }
        
        if(is.null(selected_durations)){
          selected_durations <- cluster_file$duration
        }
        
        if(is.null(selected_mp)){
          selected_mp <- cluster_file$mode
        }
        selected_nodes <- cluster_file[
          cluster_file$chemical %in% selected_chemicals &
          cluster_file$dose %in% selected_doses &
          cluster_file$duration %in% selected_durations &
          cluster_file$mode %in% selected_mp
          , "V1"
        ]
        visNetworkProxy(paste0(tiss_chip, "__network_proxy")) %>% visSelectNodes(id=selected_nodes)
      })
      return(tp)
    }))
  })
  
  # Define reusable functions
  access_enrichr_api <- function(genes=c()){
    # 1) Check if Enrichr is accessible
    websiteLive <- getOption("enrichR.live")
    if(websiteLive){
      setEnrichrSite("Enrichr")
      dbs <- c("KEGG_2021_Human", "BioPlanet_2019", "Reactome_2022", "GO_Biological_Process_2023")
      # 2) Send gene list to server
      enriched <- enrichr(genes, dbs)
      return(enriched)
    } else {
      showNotification("Error: unable to connect to the Enrichr API.", type="error")
      return(FALSE)
    }
  }
  
  observeEvent(input$datatype, {
    if(input$datatype ==  "measured"){
      shinyjs::show("dl_gene_expression_measured_affy")
      shinyjs::show("dl_histopathology_measured_affy")
      shinyjs::show("dl_clinical_chemistry_measured_affy")
      shinyjs::show("dl_hematology_measured_affy")
      shinyjs::show("dl_gene_expression_measured_affy")
      shinyjs::show("dl_histopathology_measured_affy")
      shinyjs::show("dl_clinical_chemistry_measured_affy")
      shinyjs::show("dl_hematology_measured_affy")
      
      shinyjs::show("dl_gene_expression_measured_codelink")
      shinyjs::show("dl_histopathology_measured_codelink")
      shinyjs::show("dl_clinical_chemistry_measured_codelink")
      shinyjs::show("dl_hematology_measured_codelink")
      shinyjs::show("dl_gene_expression_measured_codelink")
      shinyjs::show("dl_histopathology_measured_codelink")
      shinyjs::show("dl_clinical_chemistry_measured_codelink")
      shinyjs::show("dl_hematology_measured_codelink")
      
      shinyjs::hide("dl_gene_expression_predicted_affy")
      shinyjs::hide("dl_histopathology_predicted_affy")
      shinyjs::hide("dl_clinical_chemistry_predicted_affy")
      shinyjs::hide("dl_hematology_predicted_affy")
      shinyjs::hide("dl_gene_expression_predicted_affy")
      shinyjs::hide("dl_histopathology_predicted_affy")
      shinyjs::hide("dl_clinical_chemistry_predicted_affy")
      shinyjs::hide("dl_hematology_predicted_affy")
      
      shinyjs::hide("dl_gene_expression_predicted_codelink")
      shinyjs::hide("dl_histopathology_predicted_codelink")
      shinyjs::hide("dl_clinical_chemistry_predicted_codelink")
      shinyjs::hide("dl_hematology_predicted_codelink")
      shinyjs::hide("dl_gene_expression_predicted_codelink")
      shinyjs::hide("dl_histopathology_predicted_codelink")
      shinyjs::hide("dl_clinical_chemistry_predicted_codelink")
      shinyjs::hide("dl_hematology_predicted_codelink")
      
      shinyjs::disable("predictedonly")
    } else if(input$datatype ==  "predicted"){
      shinyjs::show("dl_gene_expression_predicted_affy")
      shinyjs::show("dl_histopathology_predicted_affy")
      shinyjs::show("dl_clinical_chemistry_predicted_affy")
      shinyjs::show("dl_hematology_predicted_affy")
      shinyjs::show("dl_gene_expression_predicted_affy")
      shinyjs::show("dl_histopathology_predicted_affy")
      shinyjs::show("dl_clinical_chemistry_predicted_affy")
      shinyjs::show("dl_hematology_predicted_affy")
      
      shinyjs::show("dl_gene_expression_predicted_codelink")
      shinyjs::show("dl_histopathology_predicted_codelink")
      shinyjs::show("dl_clinical_chemistry_predicted_codelink")
      shinyjs::show("dl_hematology_predicted_codelink")
      shinyjs::show("dl_gene_expression_predicted_codelink")
      shinyjs::show("dl_histopathology_predicted_codelink")
      shinyjs::show("dl_clinical_chemistry_predicted_codelink")
      shinyjs::show("dl_hematology_predicted_codelink")
      
      shinyjs::hide("dl_gene_expression_measured_affy")
      shinyjs::hide("dl_histopathology_measured_affy")
      shinyjs::hide("dl_clinical_chemistry_measured_affy")
      shinyjs::hide("dl_hematology_measured_affy")
      shinyjs::hide("dl_gene_expression_measured_affy")
      shinyjs::hide("dl_histopathology_measured_affy")
      shinyjs::hide("dl_clinical_chemistry_measured_affy")
      shinyjs::hide("dl_hematology_measured_affy")
      
      shinyjs::hide("dl_gene_expression_measured_codelink")
      shinyjs::hide("dl_histopathology_measured_codelink")
      shinyjs::hide("dl_clinical_chemistry_measured_codelink")
      shinyjs::hide("dl_hematology_measured_codelink")
      shinyjs::hide("dl_gene_expression_measured_codelink")
      shinyjs::hide("dl_histopathology_measured_codelink")
      shinyjs::hide("dl_clinical_chemistry_measured_codelink")
      shinyjs::hide("dl_hematology_measured_codelink")
      
      shinyjs::enable("predictedonly")
    } else if(input$datatype =="both"){
      shinyjs::show("dl_gene_expression_measured_affy")
      shinyjs::show("dl_histopathology_measured_affy")
      shinyjs::show("dl_clinical_chemistry_measured_affy")
      shinyjs::show("dl_hematology_measured_affy")
      shinyjs::show("dl_gene_expression_measured_affy")
      shinyjs::show("dl_histopathology_measured_affy")
      shinyjs::show("dl_clinical_chemistry_measured_affy")
      shinyjs::show("dl_hematology_measured_affy")
      
      shinyjs::show("dl_gene_expression_measured_codelink")
      shinyjs::show("dl_histopathology_measured_codelink")
      shinyjs::show("dl_clinical_chemistry_measured_codelink")
      shinyjs::show("dl_hematology_measured_codelink")
      shinyjs::show("dl_gene_expression_measured_codelink")
      shinyjs::show("dl_histopathology_measured_codelink")
      shinyjs::show("dl_clinical_chemistry_measured_codelink")
      shinyjs::show("dl_hematology_measured_codelink")
      
      shinyjs::show("dl_gene_expression_predicted_affy")
      shinyjs::show("dl_histopathology_predicted_affy")
      shinyjs::show("dl_clinical_chemistry_predicted_affy")
      shinyjs::show("dl_hematology_predicted_affy")
      shinyjs::show("dl_gene_expression_predicted_affy")
      shinyjs::show("dl_histopathology_predicted_affy")
      shinyjs::show("dl_clinical_chemistry_predicted_affy")
      shinyjs::show("dl_hematology_predicted_affy")
      
      shinyjs::show("dl_gene_expression_predicted_codelink")
      shinyjs::show("dl_histopathology_predicted_codelink")
      shinyjs::show("dl_clinical_chemistry_predicted_codelink")
      shinyjs::show("dl_hematology_predicted_codelink")
      shinyjs::show("dl_gene_expression_predicted_codelink")
      shinyjs::show("dl_histopathology_predicted_codelink")
      shinyjs::show("dl_clinical_chemistry_predicted_codelink")
      shinyjs::show("dl_hematology_predicted_codelink")
      
      shinyjs::enable("predictedonly")
    }
  })
  
  observeEvent(input$valuerange_low, {
    if(is.na(input$valuerange_low)){
      updateNumericInput(session, inputId="valuerange_low", value=0)
      return()
    }
    if(input$valuerange_low > input$valuerange_high){
      updateNumericInput(session, inputId="valuerange_low", value=input$valuerange_high)
    }
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  observeEvent(input$valuerange_high, {
    if(is.na(input$valuerange_high)){
      updateNumericInput(session, inputId="valuerange_high", value=0)
      return()
    }
    if(input$valuerange_high < input$valuerange_low){
      updateNumericInput(session, inputId="valuerange_high", value=input$valuerange_low)
    }
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  # Define options for sidebar menu
  res_chip <- callModule(
    module = selectizeGroupServer,
    id = "my-chip",
    data = chips,
    vars = "chip_name"
  )
  res_chemicals <- callModule(
    module = selectizeGroupServer,
    id = "my-chems",
    data = chemicals,
    vars = c("chemical_name", "dose", "time")
  )
  res_tissue <- callModule(
    module = selectizeGroupServer,
    id = "my-tissue",
    data = tissues,
    vars = "tissue"
  )
  
  res_gene_affy <- callModule(
    module = selectizeGroupServer,
    id = "my-gene_affy",
    data = genes_affy,
    vars = "gene_name"
  )
  res_gene_codelink <- callModule(
    module = selectizeGroupServer,
    id = "my-gene_codelink",
    data = genes_codelink,
    vars = "gene_name"
  )
  
  # Define reactive values to hold the values for the above selectors
  chemical_chosen <- reactive({res_chemicals()})
  chip_chosen <- reactive({res_chip()})
  tissue_chosen <- reactive({res_tissue()})
  gene_chosen_affy <- reactive({res_gene_affy()})
  gene_chosen_codelink <- reactive({res_gene_codelink()})
  
  # initialize tables so they don't show up on app startup
  output$table_gene_expression_measured_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_gene_expression_measured_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_gene_expression_predicted_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_gene_expression_predicted_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  
  output$table_histopathology_measured_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_histopathology_measured_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_histopathology_predicted_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_histopathology_predicted_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  
  output$table_clinical_chemistry_measured_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_clinical_chemistry_measured_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_clinical_chemistry_predicted_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_clinical_chemistry_predicted_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  
  output$table_hematology_measured_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_hematology_measured_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_hematology_predicted_affy <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_hematology_predicted_codelink <- renderDT(data.frame(), options=list(scrollX=TRUE))
  
  output$table_loaded_genes_affy_measured <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_loaded_genes_affy_predicted <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_loaded_genes_codelink_measured <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$table_loaded_genes_codelink_predicted <- renderDT(data.frame(), options=list(scrollX=TRUE))
  output$enriched_plots_affy_measured <- renderUI(p())
  output$enriched_plots_affy_predicted <- renderUI(p())
  output$enriched_plots_codelink_measured <- renderUI(p())
  output$enriched_plots_codelink_predicted <- renderUI(p())
  output$enriched_tables_affy_measured <- renderUI(p())
  output$enriched_tables_affy_predicted <- renderUI(p())
  output$enriched_tables_codelink_measured <- renderUI(p())
  output$enriched_tables_codelink_predicted <- renderUI(p())
  
  final_annotation_table_measured_affy <- reactiveValues(table=data.frame())
  final_annotation_table_measured_codelink <- reactiveValues(table=data.frame())
  final_annotation_table_predicted_affy <- reactiveValues(table=data.frame())
  final_annotation_table_predicted_codelink <- reactiveValues(table=data.frame())
  
  load_genes <- function(){
    removeModal(session)
    tmp_chemical <- chemical_chosen()
    tmp_chip <- chip_chosen()
    tmp_tissue <- tissue_chosen()
    tmp_gene_affy <- gene_chosen_affy()
    
    if(nrow(tmp_gene_affy) == nrow(genes_affy)) {
      tmp_gene_affy <- data.frame(probeset_name=c())
    }
    
    tmp_gene_codelink <- gene_chosen_codelink()
    
    if(nrow(tmp_gene_codelink) == nrow(genes_codelink)) {
      tmp_gene_codelink <- data.frame(probeset_name=c())
    }

    # reset reactive values
    # reset tables upon reload
    output$table_gene_expression_measured_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_gene_expression_measured_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_histopathology_measured_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_histopathology_measured_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_clinical_chemistry_measured_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_clinical_chemistry_measured_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_hematology_measured_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_hematology_measured_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_gene_expression_predicted_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_gene_expression_predicted_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_histopathology_predicted_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_histopathology_predicted_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_clinical_chemistry_predicted_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_clinical_chemistry_predicted_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_hematology_predicted_affy <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    output$table_hematology_predicted_codelink <- renderDT({data.frame()}, options=list(scrollX=TRUE))
    
    # reset Enrichr
    output$table_loaded_genes_affy_measured <- renderDT(data.frame(), options=list(scrollX=TRUE))
    output$table_loaded_genes_affy_predicted <- renderDT(data.frame(), options=list(scrollX=TRUE))
    output$table_loaded_genes_codelink_measured <- renderDT(data.frame(), options=list(scrollX=TRUE))
    output$table_loaded_genes_codelink_predicted <- renderDT(data.frame(), options=list(scrollX=TRUE))
    output$enriched_plots_affy_measured <- renderUI(p())
    output$enriched_plots_affy_predicted <- renderUI(p())
    output$enriched_plots_codelink_measured <- renderUI(p())
    output$enriched_plots_codelink_predicted <- renderUI(p())
    output$enriched_tables_affy_measured <- renderUI(p())
    output$enriched_tables_affy_predicted <- renderUI(p())
    output$enriched_tables_codelink_measured <- renderUI(p())
    output$enriched_tables_codelink_predicted <- renderUI(p())
    
    # reset reactive values
    final_annotation_table_measured_affy$table <- data.frame()
    final_annotation_table_measured_codelink$table <- data.frame()
    final_annotation_table_predicted_affy$table <- data.frame()
    final_annotation_table_predicted_codelink$table <- data.frame()
    
    showNotification("Searching for genes: this may take several seconds...", type="message")
    
    tmp_expressions_measured_affy <- data.frame()
    tmp_expressions_measured_codelink <- data.frame()
    tmp_expressions_predicted_affy <- data.frame()
    tmp_expressions_predicted_codelink <- data.frame()

    if(input$datatype =="both" | input$datatype ==  "measured"){
      if("RG230" %in% tmp_chip$chip_name){
        tmp_expressions_measured_affy <- run_search(mode="measured", predicted_only=FALSE, chip="affy", low=input$valuerange_low, high=input$valuerange_high, chemicals=tmp_chemical$chem_id, tissues=tmp_tissue$tiss_id, probes=tmp_gene_affy$probeset_name)
      } 
      if("RU1" %in% tmp_chip$chip_name){
        tmp_expressions_measured_codelink <- run_search(mode="measured", predicted_only=FALSE, chip="codelink", low=input$valuerange_low, high=input$valuerange_high, chemicals=tmp_chemical$chem_id, tissues=tmp_tissue$tiss_id, probes=tmp_gene_codelink$probeset_name)
      }
    }
    if(input$datatype =="both" | input$datatype ==  "predicted"){
      if(input$predictedonly == FALSE){
        if("RG230" %in% tmp_chip$chip_name){
          tmp_expressions_predicted_affy <- run_search(mode="predicted", predicted_only=FALSE, chip="affy", low=input$valuerange_low, high=input$valuerange_high, chemicals=tmp_chemical$chem_id, tissues=tmp_tissue$tiss_id, probes=tmp_gene_affy$probeset_name)
        }
        if("RU1" %in% tmp_chip$chip_name){
          tmp_expressions_predicted_codelink <- run_search(mode="predicted", predicted_only=FALSE, chip="codelink", low=input$valuerange_low, high=input$valuerange_high, chemicals=tmp_chemical$chem_id, tissues=tmp_tissue$tiss_id, probes=tmp_gene_codelink$probeset_name)
        }
      } else {
        # predicted only
        if("RG230" %in% tmp_chip$chip_name){
          tmp_expressions_predicted_affy <- run_search(mode="predicted", predicted_only=TRUE, chip="affy", low=input$valuerange_low, high=input$valuerange_high, chemicals=tmp_chemical$chem_id, tissues=tmp_tissue$tiss_id, probes=tmp_gene_affy$probeset_name)
        }
        if("RU1" %in% tmp_chip$chip_name){
          tmp_expressions_predicted_codelink <- run_search(mode="predicted", predicted_only=TRUE, chip="codelink", low=input$valuerange_low, high=input$valuerange_high, chemicals=tmp_chemical$chem_id, tissues=tmp_tissue$tiss_id, probes=tmp_gene_codelink$probeset_name)
        }
      }
    }
    
    # Render outputs to UI
    if(input$datatype =="both" | input$datatype ==  "measured"){
      if("RG230" %in% tmp_chip$chip_name){
        output$table_gene_expression_measured_affy <- renderDT({
          if(nrow(tmp_expressions_measured_affy) > 0){
            tmp_expressions_measured_affy[, c("probeset_name", "human_gene", "rat_gene", "chip_name", "tissue", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value")]
          } else{
            data.frame()
          }
        },
          selection="none",
          rownames=FALSE,
          class="row-border stripe compact", options=list(scrollX=TRUE)
        )
        output$dl_gene_expression_measured_affy <- downloadHandler(
          filename=function(){
            paste0("geneexpression_measured_affy_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(tmp_expressions_measured_affy, file)
          }
        )
      }
      
      if("RU1" %in% tmp_chip$chip_name){
        output$table_gene_expression_measured_codelink <- renderDT({
          if(nrow(tmp_expressions_measured_codelink) > 0){
            tmp_expressions_measured_codelink[, c("probeset_name", "human_gene", "rat_gene", "chip_name", "tissue", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value")]
          } else{
            data.frame()
          }
        },
          selection="none",
          rownames=FALSE,
          class="row-border stripe compact", options=list(scrollX=TRUE)
        )
        output$dl_gene_expression_measured_codelink <- downloadHandler(
          filename=function(){
            paste0("geneexpression_measured_codelink_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(tmp_expressions_measured_codelink, file)
          }
        )
      }
    }
      
    if(input$datatype =="both" | input$datatype ==  "predicted"){
      if("RG230" %in% tmp_chip$chip_name){
        output$table_gene_expression_predicted_affy <- renderDT({
          if(nrow(tmp_expressions_predicted_affy) > 0){
            tmp_expressions_predicted_affy[, c("probeset_name", "human_gene", "rat_gene", "chip_name", "tissue", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence")]
          } else{
            data.frame()
          }
        },
          selection="none",
          rownames=FALSE,
          class="row-border stripe compact", options=list(scrollX=TRUE)
        )
        
        output$dl_gene_expression_predicted_affy <- downloadHandler(
          filename=function(){
            paste0("geneexpression_predicted_affy_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(tmp_expressions_predicted_affy, file)
          }
        )
      }
      
      if("RU1" %in% tmp_chip$chip_name){
        output$table_gene_expression_predicted_codelink <- renderDT({
          if(nrow(tmp_expressions_predicted_codelink) > 0){
            tmp_expressions_predicted_codelink[, c("probeset_name", "human_gene", "rat_gene", "chip_name", "tissue", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence")]
          } else{
            data.frame()
          }
        },
        selection="none",
        rownames=FALSE,
        class="row-border stripe compact", options=list(scrollX=TRUE)
        )
        
        output$dl_gene_expression_predicted_codelink <- downloadHandler(
          filename=function(){
            paste0("geneexpression_predicted_codelink_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(tmp_expressions_predicted_codelink, file)
          }
        )
      }
    }
    
    # Save reactive tables for later use
    if(input$datatype =="both" | input$datatype ==  "measured"){
      if("RG230" %in% tmp_chip$chip_name){
        final_annotation_table_measured_affy$table <- tmp_expressions_measured_affy
      }
      if("RU1" %in% tmp_chip$chip_name){
        final_annotation_table_measured_codelink$table <- tmp_expressions_measured_codelink
      }
    }
    if(input$datatype =="both" | input$datatype ==  "predicted"){
      if("RG230" %in% tmp_chip$chip_name){
        final_annotation_table_predicted_affy$table <- tmp_expressions_predicted_affy
      }
      if("RU1" %in% tmp_chip$chip_name){
        final_annotation_table_predicted_codelink$table <- tmp_expressions_predicted_codelink
      }
    }
    
    tmp_chm_measured_affy <- data.frame()
    tmp_chm_measured_codelink <- data.frame()
    tmp_chm_predicted_affy <- data.frame()
    tmp_chm_predicted_codelink <- data.frame()
    
    if(length(tmp_chemical$chem_id) > 0){ # Only get this if chemicals are specified
        if(input$datatype =="both" | input$datatype ==  "measured"){
          tmp_chm_measured_affy <- run_query(query=paste0("
            SELECT DISTINCT
                mc.*
            FROM
                mv_chm_measured mc
            WHERE
                mc.chem_id IN (", paste0(lapply(seq_len(length(tmp_chemical$chem_id)), function(num) paste0("$", num)), collapse=","), ")
          "), args=tmp_chemical$chem_id)
          tmp_chm_measured_affy <- split(tmp_chm_measured_affy, tmp_chm_measured_affy$test_type)
          tmp_chm_measured_codelink <- tmp_chm_measured_affy
        }
        if(input$datatype =="both" | input$datatype ==  "predicted"){
          tmp_chm_predicted_affy <- run_query(query=paste0("
            SELECT DISTINCT
                mc.*
            FROM
                mv_chm mc
            WHERE
                mc.chem_id IN (", paste0(lapply(seq_len(length(tmp_chemical$chem_id)), function(num) paste0("$", num)), collapse=","), ")
          "), args=tmp_chemical$chem_id)
          tmp_chm_predicted_affy <- split(tmp_chm_predicted_affy, tmp_chm_predicted_affy$test_type)
          tmp_chm_predicted_codelink <- tmp_chm_predicted_affy
        }
        
        if(input$datatype =="both" | input$datatype ==  "measured"){
          if("RG230" %in% tmp_chip$chip_name){
            if("C" %in% names(tmp_chm_measured_affy)){
              output$table_clinical_chemistry_measured_affy <- renderDT({
                if(nrow(tmp_chm_measured_affy[["C"]]) > 0){
                  tmp_chm_measured_affy[["C"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
                selection="none",
                rownames=FALSE,
                class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_clinical_chemistry_measured_affy <- downloadHandler(
                filename=function(){
                  paste0("clinicalchemistry_measured_affy_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_measured_affy[["C"]], file)
                }
              )
            }
            
            if("H" %in% names(tmp_chm_measured_affy)){
              output$table_hematology_measured_affy <- renderDT({
                if(nrow(tmp_chm_measured_affy[["H"]]) > 0){
                  tmp_chm_measured_affy[["H"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
                selection="none",
                rownames=FALSE,
                class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_hematology_measured_affy <- downloadHandler(
                filename=function(){
                  paste0("hematology_measured_affy_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_measured_affy[["H"]], file)
                }
              )
            }
            
            if("M" %in% names(tmp_chm_measured_affy)){
              output$table_histopathology_measured_affy <- renderDT({
                if(nrow(tmp_chm_measured_affy[["M"]]) > 0){
                  tmp_chm_measured_affy[["M"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
                selection="none",
                rownames=FALSE,
                class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_histopathology_measured_affy <- downloadHandler(
                filename=function(){
                  paste0("histopathology_measured_affy_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_measured_affy[["M"]], file)
                }
              )
            }
          }
          
          if("RU1" %in% tmp_chip$chip_name){
            if("C" %in% names(tmp_chm_measured_codelink)){
              output$table_clinical_chemistry_measured_codelink <- renderDT({
                if(nrow(tmp_chm_measured_codelink[["C"]]) > 0){
                  tmp_chm_measured_codelink[["C"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
              selection="none",
              rownames=FALSE,
              class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_clinical_chemistry_measured_codelink <- downloadHandler(
                filename=function(){
                  paste0("clinicalchemistry_measured_codelink_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_measured_codelink[["C"]], file)
                }
              )
            }
            
            if("H" %in% names(tmp_chm_measured_codelink)){
              output$table_hematology_measured_codelink <- renderDT({
                if(nrow(tmp_chm_measured_codelink[["H"]]) > 0){
                  tmp_chm_measured_codelink[["H"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
              selection="none",
              rownames=FALSE,
              class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_hematology_measured_codelink <- downloadHandler(
                filename=function(){
                  paste0("hematology_measured_codelink_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_measured_codelink[["H"]], file)
                }
              )
            }
            
            if("M" %in% names(tmp_chm_measured_codelink)){
              output$table_histopathology_measured_codelink <- renderDT({
                if(nrow(tmp_chm_measured_codelink[["M"]]) > 0){
                  tmp_chm_measured_codelink[["M"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
              selection="none",
              rownames=FALSE,
              class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_histopathology_measured_codelink <- downloadHandler(
                filename=function(){
                  paste0("histopathology_measured_codelink_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_measured_codelink[["M"]], file)
                }
              )
            }
          }
        }
        
        if(input$datatype =="both" | input$datatype ==  "predicted"){
          if("RG230" %in% tmp_chip$chip_name){
            if("C" %in% names(tmp_chm_predicted_affy)){
              output$table_clinical_chemistry_predicted_affy <- renderDT({
                if(nrow(tmp_chm_predicted_affy[["C"]]) > 0){
                  tmp_chm_predicted_affy[["C"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
                selection="none",
                rownames=FALSE,
                class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_clinical_chemistry_predicted_affy <- downloadHandler(
                filename=function(){
                  paste0("clinicalchemistry_predicted_affy_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_predicted_affy[["C"]], file)
                }
              )
            }
            
            if("H" %in% names(tmp_chm_predicted_affy)){
              output$table_hematology_predicted_affy <- renderDT({
                if(nrow(tmp_chm_predicted_affy[["H"]]) > 0){
                  tmp_chm_predicted_affy[["H"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
                selection="none",
                rownames=FALSE,
                class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_hematology_predicted_affy <- downloadHandler(
                filename=function(){
                  paste0("hematology_predicted_affy_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_predicted_affy[["H"]], file)
                }
              )
            }
            
            if("M" %in% names(tmp_chm_predicted_affy)){
              output$table_histopathology_predicted_affy <- renderDT({
                if(nrow(tmp_chm_predicted_affy[["M"]]) > 0){
                  tmp_chm_predicted_affy[["M"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
                selection="none",
                rownames=FALSE,
                class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_histopathology_predicted_affy <- downloadHandler(
                filename=function(){
                  paste0("histopathology_predicted_affy_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_predicted_affy[["M"]], file)
                }
              )
            }
          }
            
          if("RU1" %in% tmp_chip$chip_name){
            if("C" %in% names(tmp_chm_predicted_codelink)){
              output$table_clinical_chemistry_predicted_codelink <- renderDT({
                if(nrow(tmp_chm_predicted_codelink[["C"]]) > 0){
                  tmp_chm_predicted_codelink[["C"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
              selection="none",
              rownames=FALSE,
              class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_clinical_chemistry_predicted_codelink <- downloadHandler(
                filename=function(){
                  paste0("clinicalchemistry_predicted_codelink_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_predicted_codelink[["C"]], file)
                }
              )
            }
            
            if("H" %in% names(tmp_chm_predicted_codelink)){
              output$table_hematology_predicted_codelink <- renderDT({
                if(nrow(tmp_chm_predicted_codelink[["H"]]) > 0){
                  tmp_chm_predicted_codelink[["H"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
              selection="none",
              rownames=FALSE,
              class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_hematology_predicted_codelink <- downloadHandler(
                filename=function(){
                  paste0("hematology_predicted_codelink_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_predicted_codelink[["H"]], file)
                }
              )
            }
            
            if("M" %in% names(tmp_chm_predicted_codelink)){
              output$table_histopathology_predicted_codelink <- renderDT({
                if(nrow(tmp_chm_predicted_codelink[["M"]]) > 0){
                  tmp_chm_predicted_codelink[["M"]][, c("annotation", "chemical_name", "time", "time_unit", "dose", "dose_unit", "value", "lower", "upper", "confidence", "lbn", "ubn")]
                } else{
                  data.frame()
                }
              },
              selection="none",
              rownames=FALSE,
              class="row-border stripe compact", options=list(scrollX=TRUE)
              )
              output$dl_histopathology_predicted_codelink <- downloadHandler(
                filename=function(){
                  paste0("histopathology_predicted_codelink_", Sys.Date(), ".csv")
                },
                content=function(file){
                  fwrite(tmp_chm_predicted_codelink[["M"]], file)
                }
              )
            }
          }
        }
    
    }
    showNotification("Finished loading annotations for specified arguments.", type="message") 
    updateTabsetPanel(session, inputId="main_tabs", selected="tab_annotations")
  }
  
  observeEvent(input$load_genes_1, {
    tmp_chemical <- chemical_chosen()
    tmp_chip <- chip_chosen()
    tmp_tissue <- tissue_chosen()
    tmp_gene_affy <- gene_chosen_affy()
    tmp_gene_codelink <- gene_chosen_codelink()
    if(nrow(tmp_chemical) == nrow(chemicals) |
       nrow(tmp_tissue) == nrow(tissues) |
       nrow(tmp_gene_affy) == nrow(genes_affy) |
       nrow(tmp_gene_codelink) == nrow(genes_codelink)
    ){
      showModal(modalDialog(
        HTML("<div id=\"warning\", style=\"color:red\"><p><b>Warning:</b> The provided filters are broad and may return many results. Your query may take a long time to load. Is this okay?</p></div>"),
        title="Query Warning",
        footer=div(actionButton(inputId="load_genes_2", label="Yes"), modalButton("No"))
      ))
    } else {
      load_genes()
    }
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  observeEvent(input$load_genes_2, {
    load_genes()
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
  
  output$table_loaded_genes <- renderDT({data.frame()}, options=list(scrollX=TRUE))
  output$enriched_plots <- renderUI(div())
  output$enriched_tables <- renderUI(div())
  
  # Event observers
  observeEvent(input$run_enrichr, {
    tmp_chip <- chip_chosen()
    
    # reset tables
    output$table_loaded_genes_affy_measured <- NULL
    output$table_loaded_genes_affy_predicted <- NULL
    output$enriched_plots_affy_measured <- NULL
    output$enriched_plots_affy_predicted <- NULL
    output$enriched_tables_affy_measured <- NULL
    output$enriched_tables_affy_predicted <- NULL
    output$table_loaded_genes_codelink_measured <- NULL
    output$table_loaded_genes_codelink_predicted <- NULL
    output$enriched_plots_codelink_measured <- NULL
    output$enriched_plots_codelink_predicted <- NULL
    output$enriched_tables_codelink_measured <- NULL
    output$enriched_tables_codelink_predicted <- NULL
    
    final_table_measured_affy <- final_annotation_table_measured_affy$table
    final_table_measured_codelink <- final_annotation_table_measured_codelink$table
    final_table_predicted_affy <- final_annotation_table_predicted_affy$table
    final_table_predicted_codelink <- final_annotation_table_predicted_codelink$table
    if(input$datatype == "measured"){
      if(nrow(final_table_measured_affy) < 1 & nrow(final_table_measured_codelink) < 1){
        showNotification("Error: no genes loaded.", type="error")
        return(FALSE)
      }
    } else if(input$datatype == "predicted"){
      if(nrow(final_table_predicted_affy) < 1 & nrow(final_table_predicted_codelink) < 1){
        showNotification("Error: no genes loaded.", type="error")
        return(FALSE)
      }
    } else if(input$datatype == "both"){
      if(nrow(final_table_measured_affy) < 1 & nrow(final_table_measured_codelink) < 1 & nrow(final_table_predicted_affy) < 1 & nrow(final_table_predicted_codelink) < 1){
        showNotification("Error: no genes loaded.", type="error")
        return(FALSE)
      }
    }
    
    mapped_human_genes_affy_measured <- data.frame()
    mapped_human_genes_affy_predicted <- data.frame()
    mapped_human_genes_codelink_measured <- data.frame()
    mapped_human_genes_codelink_predicted <- data.frame()
    
    enriched_plots_affy_measured <- list()
    enriched_plots_affy_predicted <- list()
    enriched_plots_codelink_measured <- list()
    enriched_plots_codelink_predicted <- list()

    withProgress(message="Enriching: ", value=0, {
      if("RG230" %in% tmp_chip$chip_name){
        if(input$datatype == "both" | input$datatype == "measured"){
          mapped_human_genes_affy_measured <- id_lookup_affy[id_lookup_affy$probeset_name %in% final_table_measured_affy$probeset_name]
          output$table_loaded_genes_affy_measured <- renderDT(
            mapped_human_genes_affy_measured[, c("human_gene", "human_entrez_id", "probeset_name", "probe")],
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            caption="Loaded Genes (RG230, Measured)", options=list(scrollX=TRUE)
          )
        }
        if(input$datatype == "both" | input$datatype == "predicted"){
          mapped_human_genes_affy_predicted <- id_lookup_affy[id_lookup_affy$probeset_name %in% final_table_predicted_affy$probeset_name]
          output$table_loaded_genes_affy_predicted <- renderDT(
            mapped_human_genes_affy_predicted[, c("human_gene", "human_entrez_id", "probeset_name", "probe")],
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            caption="Loaded Genes (RG230, Predicted)", options=list(scrollX=TRUE)
          )
        }
      }
      
      if("RU1" %in% tmp_chip$chip_name) {
        if(input$datatype == "both" | input$datatype == "measured"){
          mapped_human_genes_codelink_measured <- id_lookup_codelink[id_lookup_codelink$probeset_name %in% final_table_measured_codelink$probeset_name]
          output$table_loaded_genes_codelink_measured <- renderDT(
            mapped_human_genes_codelink_measured[, c("human_gene", "human_entrez_id", "probeset_name", "probe")],
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            caption="Loaded Genes (RU1, Measured)", options=list(scrollX=TRUE)
          )
        }
        if(input$datatype == "both" | input$datatype == "predicted"){
          mapped_human_genes_codelink_predicted <- id_lookup_codelink[id_lookup_codelink$probeset_name %in% final_table_predicted_codelink$probeset_name]
          output$table_loaded_genes_codelink_predicted <- renderDT(
            mapped_human_genes_codelink_predicted[, c("human_gene", "human_entrez_id", "probeset_name", "probe")],
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            caption="Loaded Genes (RU1, Predicted)", options=list(scrollX=TRUE)
          )
        }
        
      }
      
      incProgress(0.25, detail="Querying Enrichr")
      if("RG230" %in% tmp_chip$chip_name){
        if(input$datatype == "measured"){
          enriched_plots_affy_measured <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_affy_measured$human_gene)))
          enriched_plots_affy_predicted <- NULL
        } else if(input$datatype == "predicted"){
          enriched_plots_affy_measured <- NULL
          enriched_plots_affy_predicted <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_affy_predicted$human_gene)))
        } else if(input$datatype == "both"){
          enriched_plots_affy_measured <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_affy_measured$human_gene)))
          enriched_plots_affy_predicted <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_affy_predicted$human_gene)))
        }
      }
      
      if("RU1" %in% tmp_chip$chip_name) {
        if(input$datatype == "measured"){
          enriched_plots_codelink_measured <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_codelink_measured$human_gene)))
          enriched_plots_codelink_predicted <- NULL
        } else if(input$datatype == "predicted"){
          enriched_plots_codelink_measured <- NULL
          enriched_plots_codelink_predicted <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_codelink_predicted$human_gene)))
        } else if(input$datatype == "both"){
          enriched_plots_codelink_measured <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_codelink_measured$human_gene)))
          enriched_plots_codelink_predicted <- access_enrichr_api(genes=unique(unlist(mapped_human_genes_codelink_predicted$human_gene)))
        }
      }
      
      incProgress(0.7, detail="Generating plots and tables")
      
      if("RG230" %in% tmp_chip$chip_name){
        if(input$datatype == "both" | input$datatype == "measured"){
          output$enriched_plots_affy_measured <- renderUI(
            div(
              h4("RG230, Measured"),
              lapply(seq_len(length(enriched_plots_affy_measured)), function(x) div(plotOutput(paste0("plot_affy_measured_", x)), downloadButton(outputId=paste0("dl_plot_affy_measured_", x), label=paste0("Download ", names(enriched_plots_affy_measured)[x], " plots (measured, RG230)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_affy_measured)), function(x) {
            tryCatch({
              output[[paste0("plot_affy_measured_", x)]] <- renderPlot(plotEnrich(enriched_plots_affy_measured[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_affy_measured)[[x]])))
            }, error=function(cond){
              output[[paste0("plot_affy_measured_", x)]] <- renderPlot(ggplot() + theme_void())
            })
            output[[paste0("dl_plot_affy_measured_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_affy_measured)[x], "_affy_measured_", Sys.Date(), ".png")
              },
              content=function(file){
                png(file)
                print(plotEnrich(enriched_plots_affy_measured[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_affy_measured)[[x]])))
                dev.off()
              }
            )
          })
          
          output$enriched_tables_affy_measured <- renderUI(
            div(
              h4("RG230, Measured"),
              lapply(seq_len(length(enriched_plots_affy_measured)), function(x) div(DTOutput(paste0("enrichr_table_affy_measured_", x)), downloadButton(outputId=paste0("dl_enrichrtables_affy_measured_", x), label=paste0("Download ", names(enriched_plots_affy_measured)[x], " results (measured, RG230)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_affy_measured)), function(x) {
            tryCatch({
              output[[paste0("enrichr_table_affy_measured_", x)]] <- renderDT(enriched_plots_affy_measured[[x]], selection="none", rownames=FALSE, caption=paste0("Annotations for ", names(enriched_plots_affy_measured)[[x]]), class="row-border stripe compact", options=list(scrollX=TRUE))
            }, error=function(cond){
              output[[paste0("enrichr_table_affy_measured_", x)]] <- renderDT(data.frame(), options=list(scrollX=TRUE))
            })
            output[[paste0("dl_enrichrtables_affy_measured_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_affy_measured)[x], "_affy_measured_", Sys.Date(), ".csv")
              },
              content=function(file){
                fwrite(enriched_plots_affy_measured[x], file)
              }
            )
          })
        }
        if(input$datatype == "both" | input$datatype == "predicted"){
          output$enriched_plots_affy_predicted <- renderUI(
            div(
              h4("RG230, Predicted"),
              lapply(seq_len(length(enriched_plots_affy_predicted)), function(x) div(plotOutput(paste0("plot_affy_predicted_", x)), downloadButton(outputId=paste0("dl_plot_affy_predicted_", x), label=paste0("Download ", names(enriched_plots_affy_predicted)[x], " plots (predicted, RG230)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_affy_predicted)), function(x) {
            tryCatch({
              output[[paste0("plot_affy_predicted_", x)]] <- renderPlot(plotEnrich(enriched_plots_affy_predicted[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_affy_predicted)[[x]])))
            }, error=function(cond){
              output[[paste0("plot_affy_predicted_", x)]] <- renderPlot(ggplot() + theme_void())
            })
            output[[paste0("dl_plot_affy_predicted_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_affy_predicted)[x], "_affy_predicted_", Sys.Date(), ".png")
              },
              content=function(file){
                png(file)
                print(plotEnrich(enriched_plots_affy_predicted[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_affy_predicted)[[x]])))
                dev.off()
              }
            )
          })
          
          output$enriched_tables_affy_predicted <- renderUI(
            div(
              h4("RG230, Predicted"),
              lapply(seq_len(length(enriched_plots_affy_predicted)), function(x) div(DTOutput(paste0("enrichr_table_affy_predicted_", x)), downloadButton(outputId=paste0("dl_enrichrtables_affy_predicted_", x), label=paste0("Download ", names(enriched_plots_affy_predicted)[x], " results (predicted, RG230)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_affy_predicted)), function(x) {
            tryCatch({
              output[[paste0("enrichr_table_affy_predicted_", x)]] <- renderDT(enriched_plots_affy_predicted[[x]], selection="none", rownames=FALSE, caption=paste0("Annotations for ", names(enriched_plots_affy_predicted)[[x]]), class="row-border stripe compact", options=list(scrollX=TRUE))
            }, error=function(cond){
              output[[paste0("enrichr_table_affy_predicted_", x)]] <- renderDT(data.frame(), options=list(scrollX=TRUE))
            })
            output[[paste0("dl_enrichrtables_affy_predicted_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_affy_predicted)[x], "_affy_predicted_", Sys.Date(), ".csv")
              },
              content=function(file){
                fwrite(enriched_plots_affy_predicted[x], file)
              }
            )
          })
        }
      }
      
      if("RU1" %in% tmp_chip$chip_name) {
        if(input$datatype == "both" | input$datatype == "measured"){
          output$enriched_plots_codelink_measured <- renderUI(
            div(
              h4("RU1, Measured"),
              lapply(seq_len(length(enriched_plots_codelink_measured)), function(x) div(plotOutput(paste0("plot_codelink_measured_", x)), downloadButton(outputId=paste0("dl_plot_codelink_measured_", x), label=paste0("Download ", names(enriched_plots_codelink_measured)[x], " plots (measured, RU1)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_codelink_measured)), function(x) {
            tryCatch({
              output[[paste0("plot_codelink_measured_", x)]] <- renderPlot(plotEnrich(enriched_plots_codelink_measured[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_codelink_measured)[[x]])))
            }, error=function(cond){
              output[[paste0("plot_codelink_measured_", x)]] <- renderPlot(ggplot() + theme_void())
            })
            output[[paste0("dl_plot_codelink_measured_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_codelink_measured)[x], "_codelink_measured_", Sys.Date(), ".png")
              },
              content=function(file){
                png(file)
                print(plotEnrich(enriched_plots_codelink_measured[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_codelink_measured)[[x]])))
                dev.off()
              }
            )
          })
          
          output$enriched_tables_codelink_measured <- renderUI(
            div(
              h4("RU1, Measured"),
              lapply(seq_len(length(enriched_plots_codelink_measured)), function(x) div(DTOutput(paste0("enrichr_table_codelink_measured_", x)), downloadButton(outputId=paste0("dl_enrichrtables_codelink_measured_", x), label=paste0("Download ", names(enriched_plots_codelink_measured)[x], " results (measured, RU1)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_codelink_measured)), function(x) {
            tryCatch({
              output[[paste0("enrichr_table_codelink_measured_", x)]] <- renderDT(enriched_plots_codelink_measured[[x]], selection="none", rownames=FALSE, caption=paste0("Annotations for ", names(enriched_plots_codelink_measured)[[x]]), class="row-border stripe compact", options=list(scrollX=TRUE))
            }, error=function(cond){
              output[[paste0("enrichr_table_codelink_measured_", x)]] <- renderDT(data.frame(), options=list(scrollX=TRUE))
            })
            output[[paste0("dl_enrichrtables_codelink_measured_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_codelink_measured)[x], "_codelink_measured_", Sys.Date(), ".csv")
              },
              content=function(file){
                fwrite(enriched_plots_codelink_measured[x], file)
              }
            )
          })
        }
        if(input$datatype == "both" | input$datatype == "predicted"){
          output$enriched_plots_codelink_predicted <- renderUI(
            div(
              h4("RU1, Predicted"),
              lapply(seq_len(length(enriched_plots_codelink_predicted)), function(x) div(plotOutput(paste0("plot_codelink_predicted_", x)), downloadButton(outputId=paste0("dl_plot_codelink_predicted_", x), label=paste0("Download ", names(enriched_plots_codelink_predicted)[x], " plots (predicted, RU1)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_codelink_predicted)), function(x) {
            tryCatch({
              output[[paste0("plot_codelink_predicted_", x)]] <- renderPlot(plotEnrich(enriched_plots_codelink_predicted[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_codelink_predicted)[[x]])))
            }, error=function(cond){
              output[[paste0("plot_codelink_predicted_", x)]] <- renderPlot(ggplot() + theme_void())
            })
            output[[paste0("dl_plot_codelink_predicted_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_codelink_predicted)[x], "_codelink_predicted_", Sys.Date(), ".png")
              },
              content=function(file){
                png(file)
                print(plotEnrich(enriched_plots_codelink_predicted[[x]], showTerms = 20, numChar = 60, y = "Count", orderBy = "P.value", title=paste0("Annotations for ", names(enriched_plots_codelink_predicted)[[x]])))
                dev.off()
              }
            )
          })
          
          output$enriched_tables_codelink_predicted <- renderUI(
            div(
              h4("RU1, Predicted"),
              lapply(seq_len(length(enriched_plots_codelink_predicted)), function(x) div(DTOutput(paste0("enrichr_table_codelink_predicted_", x)), downloadButton(outputId=paste0("dl_enrichrtables_codelink_predicted_", x), label=paste0("Download ", names(enriched_plots_codelink_predicted)[x], " results (predicted, RU1)"))))
            )
          )
          lapply(seq_len(length(enriched_plots_codelink_predicted)), function(x) {
            tryCatch({
              output[[paste0("enrichr_table_codelink_predicted_", x)]] <- renderDT(enriched_plots_codelink_predicted[[x]], selection="none", rownames=FALSE, caption=paste0("Annotations for ", names(enriched_plots_codelink_predicted)[[x]]), class="row-border stripe compact", options=list(scrollX=TRUE))
            }, error=function(cond){
              output[[paste0("enrichr_table_codelink_predicted_", x)]] <- renderDT(data.frame(), options=list(scrollX=TRUE))
            })
            output[[paste0("dl_enrichrtables_codelink_predicted_", x)]] <- downloadHandler(
              filename=function(){
                paste0(names(enriched_plots_codelink_predicted)[x], "_codelink_predicted_", Sys.Date(), ".csv")
              },
              content=function(file){
                fwrite(enriched_plots_codelink_predicted[x], file)
              }
            )
          })
        }
      }
      
      incProgress(0.99, detail="Finishing up")
    })
    
    # Prepare download buttons
    if("RG230" %in% tmp_chip$chip_name){
      if(input$datatype =="both" | input$datatype ==  "measured"){
        # set downloadable files
        output$dl_lg_affy_measured <- downloadHandler(
          filename=function(){
            paste0("genes_affy_measured_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(mapped_human_genes_affy_measured, file)
          }
        )
      }
      if(input$datatype =="both" | input$datatype ==  "predicted"){
        # set downloadable files
        output$dl_lg_affy_predicted <- downloadHandler(
          filename=function(){
            paste0("genes_affy_predicted_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(mapped_human_genes_affy_predicted, file)
          }
        )
      }
    }
    
    if("RU1" %in% tmp_chip$chip_name){
      if(input$datatype =="both" | input$datatype == "measured"){
        # set downloadable files
        output$dl_lg_codelink_measured <- downloadHandler(
          filename=function(){
            paste0("genes_codelink_measured_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(mapped_human_genes_codelink_measured, file)
          }
        )
      }
      if(input$datatype =="both" | input$datatype == "predicted"){
        # set downloadable files
        output$dl_lg_codelink_predicted <- downloadHandler(
          filename=function(){
            paste0("genes_codelink_predicted_", Sys.Date(), ".csv")
          },
          content=function(file){
            fwrite(mapped_human_genes_codelink_predicted, file)
          }
        )
      }
    }
    updateTabsetPanel(session, inputId="main_tabs", selected="tab_enrichr")
  }, ignoreInit=TRUE, ignoreNULL=TRUE)
}