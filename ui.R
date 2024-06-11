ui <- pageWithSidebar(
  headerPanel("ToxCompl: Drugmatrix Completion"),
  sidebarPanel(style="position:fixed;width:33%;height:90%;overflow-y:scroll;",
    div(
      HTML("
      <h3>About</h3>
      <h4>Terminology</h4>
      <ul>
        <li><i>Value</i> - Log<sub>10</sub> ratio (treated vs. control) in gene expression; average severity in histopathology; average measured value of the treated group in clinical chemistry and hematology</li>
        <li><i>Expression Platform</i> - RU1 = GE Healthcare CodeLink UniSet Rat; RG230 = Affymetrix GeneChip Rat Genome 230 2.0; S1500 = S1500+ Gene Set Strategy</li>
        <li><i>Probeset ID</i> - Identifier assigned to each probe: the same probeset ID should represent the same gene in both RG230 and RU1 microarrays; not applicable for S1500</li>
        <li><i>Confidence</i> - Confidence interval range of the predicted value: only associated with predicted data</li>
      </ul>
      
      <h4>Citing ToxCompl</h4>
      <p>
        If you are using ToxCompl in your own research, please cite our manuscript: <a href=\"https://doi.org/10.1101/2024.03.26.586669\">https://doi.org/10.1101/2024.03.26.586669</a>
      </p>
      
      <h4>Data Availability</h4>
      <p>
        The data used in ToxCompl may be found <a href=\"https://cebs-ext.niehs.nih.gov/cebs/paper/15888/private/Mjc1MDIwNmU4NmQxMTNjMDhlYjdlMzA3NzUxMmI5NmUK\">here</a>.
      </p>
      
      "),
      
      bsCollapse(
        bsCollapsePanel("Instructions", 
          HTML("
               <p>
      <a href=\"https://www.youtube.com/embed/Gd1njaL3SrY?si=R6xX6N59qH4-oc0o\">View video tutorial here.</a>
      
      </p>
      <h4>Load Annotations</h4>
      <p>
        Specify terms in the following filters to search for annotations. Not specifying any terms for a given field will cause no filtering to be done using that field and return all relevant data irrespective of term (i.e., not specifying a platform will retrieve results for RG230, RU1, and S1500).<br>
        <b>Warning: Using nonspecific filtering may take a very long time to return data. We highly recommend using specific filtering criteria.</b>
      </p>
      
      <h4>Enrich with Enrichr</h4>
      <p>
        After loading annotations, you may click the \"Enrich with <b>Enrichr</b>\" button to send the genes from the results to be further annotated using the <a href=\"https://maayanlab.cloud/Enrichr/\">Enrichr</a> tool.
      </p>
          ")
        )
      ),
      
      
      hr(),
      h3("Search Options"),
      
      #selectInput("testinput", "test", choices=c("test1", "test2", "test3")),
      
      selectizeGroupUI(
        id = "my-chems",
        inline = TRUE,
        params = list(
          var_chemical = list(inputId = "chemical_name", title = "Chemical", placeholder = 'select chemical'),
          var_dose = list(inputId = "dose", title = "Dose", placeholder = 'select dose'),
          var_time = list(inputId = "time", title = "Time", placeholder = 'select time')
        )
      ),
      selectizeGroupUI(
        id = "my-tissue",
        inline = TRUE,
        params = list(
          var_tissue = list(inputId = "tissue", title = "Tissue", placeholder = 'select tissue')
        )
      ),
      selectizeGroupUI(
        id = "my-chip",
        inline = TRUE,
        params = list(
          var_chip = list(inputId = "chip_name", title = "Expression Platform", placeholder = 'select platform')
        )
      ),
      fluidRow(
        column(4,
          selectizeGroupUI(
            id = "my-gene_affy",
            inline = TRUE,
            params = list(
              var_gene = list(inputId = "gene_name", title = "Gene (RG230)", placeholder = 'select gene')
            )
          )
        ),
        column(4,
          selectizeGroupUI(
            id = "my-gene_codelink",
            inline = TRUE,
            params = list(
              var_gene = list(inputId = "gene_name", title = "Gene (RU1)", placeholder = 'select gene')
            )
          )
        ),
        column(4,
               selectizeGroupUI(
                 id = "my-gene_s1500",
                 inline = TRUE,
                 params = list(
                   var_gene = list(inputId = "rat_gene", title = "Gene (S1500)", placeholder = 'select gene')
                 )
               )
        )
      ),
      
      radioButtons(inputId="datatype", label="Data Type", inline=TRUE, choices=c("measured", "predicted", "both")),
      disabled(tags$div(title="Checking this option will filter predicted data to only include those which do not also have corresponding measured data. Returned measured data will be unaffected.", checkboxInput(inputId="predictedonly", label="Show purely predicted data only?"))),
      HTML("<p><b>Gene Expression Value Range</b></p>"),
      fluidRow(
        column(6,
          numericInput(inputId="valuerange_low", label="min.", min=-5, max=5, value=-5, step=0.01)
        ),
        column(6,
          numericInput(inputId="valuerange_high", label="max.", min=-5, max=5, value=5, step=0.01)
        )
      ),
      hr(),
      actionButton(inputId="load_genes_1", label=HTML("Load Annotations")),
      actionButton(inputId="run_enrichr", label=HTML("Enrich with <b>Enrichr</b>"))
    )
  ),
    
  mainPanel(
    useShinyjs(),
    tabsetPanel(id="main_tabs",
      tabPanel(title="Annotations", value="tab_annotations",
        tabsetPanel(
          tabPanel(title="Gene Expression",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                DTOutput("table_gene_expression_measured_affy") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_gene_expression_measured_affy", label="Download data table (measured, RG230)")
                )
              ),
              tabPanel(title="RG230, Predicted",
                 DTOutput("table_gene_expression_predicted_affy") %>% withSpinner(),
                 hidden(
                  downloadButton(outputId="dl_gene_expression_predicted_affy", label="Download data table (predicted, RG230)")
                 )
              ),
              tabPanel(title="RU1, Measured",
                DTOutput("table_gene_expression_measured_codelink") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_gene_expression_measured_codelink", label="Download data table (measured, RU1)")
                )
              ),
              tabPanel(title="RU1, Predicted",
                DTOutput("table_gene_expression_predicted_codelink") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_gene_expression_predicted_codelink", label="Download data table (predicted, RU1)")
                )
              ),

              tabPanel(title="S1500, Measured",
                 DTOutput("table_gene_expression_measured_s1500") %>% withSpinner(),
                 hidden(
                   downloadButton(outputId="dl_gene_expression_measured_s1500", label="Download data table (measured, S1500)")
                 )
              ),
              tabPanel(title="S1500, Predicted",
                 DTOutput("table_gene_expression_predicted_s1500") %>% withSpinner(),
                 hidden(
                   downloadButton(outputId="dl_gene_expression_predicted_s1500", label="Download data table (predicted, S1500)")
                 )
              )

            )
          ),


          tabPanel(title="Histopathology",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                DTOutput("table_histopathology_measured_affy") %>% withSpinner(),
                downloadButton(outputId="dl_histopathology_measured_affy", label="Download data table (measured, RG230)")
              ),
              tabPanel(title="RG230, Predicted",
                DTOutput("table_histopathology_predicted_affy") %>% withSpinner(),
                downloadButton(outputId="dl_histopathology_predicted_affy", label="Download data table (predicted, RG230)")
              ),
              tabPanel(title="RU1, Measured",
                DTOutput("table_histopathology_measured_codelink") %>% withSpinner(),
                downloadButton(outputId="dl_histopathology_measured_codelink", label="Download data table (predicted, RU1)")
              ),
              tabPanel(title="RU1, Predicted",
                DTOutput("table_histopathology_predicted_codelink") %>% withSpinner(),
                downloadButton(outputId="dl_histopathology_predicted_codelink", label="Download data table (predicted, RU1)")
              ),

              tabPanel(title="S1500, Measured",
                 DTOutput("table_histopathology_measured_s1500") %>% withSpinner(),
                 downloadButton(outputId="dl_histopathology_measured_s1500", label="Download data table (predicted, S1500)")
              ),
              tabPanel(title="S1500, Predicted",
                 DTOutput("table_histopathology_predicted_s1500") %>% withSpinner(),
                 downloadButton(outputId="dl_histopathology_predicted_s1500", label="Download data table (predicted, S1500)")
              )

            )
          ),
          tabPanel(title="Clinical Chemistry",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                DTOutput("table_clinical_chemistry_measured_affy") %>% withSpinner(),
                downloadButton(outputId="dl_clinical_chemistry_measured_affy", label="Download data table (measured, RG230)")
              ),
              tabPanel(title="RG230, Predicted",
                DTOutput("table_clinical_chemistry_predicted_affy") %>% withSpinner(),
                downloadButton(outputId="dl_clinical_chemistry_predicted_affy", label="Download data table (predicted, RG230)")
              ),
              tabPanel(title="RU1, Measured",
                DTOutput("table_clinical_chemistry_measured_codelink") %>% withSpinner(),
                downloadButton(outputId="dl_clinical_chemistry_measured_codelink", label="Download data table (measured, RU1)")
              ),
              tabPanel(title="RU1, Predicted",
                DTOutput("table_clinical_chemistry_predicted_codelink") %>% withSpinner(),
                downloadButton(outputId="dl_clinical_chemistry_predicted_codelink", label="Download data table (predicted, RU1)")
              ),

              tabPanel(title="S1500, Measured",
                 DTOutput("table_clinical_chemistry_measured_s1500") %>% withSpinner(),
                 downloadButton(outputId="dl_clinical_chemistry_measured_s1500", label="Download data table (measured, S1500)")
              ),
              tabPanel(title="S1500, Predicted",
                 DTOutput("table_clinical_chemistry_predicted_s1500") %>% withSpinner(),
                 downloadButton(outputId="dl_clinical_chemistry_predicted_s1500", label="Download data table (predicted, S1500)")
              )

            )
          ),
          tabPanel(title="Hematology",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                DTOutput("table_hematology_measured_affy") %>% withSpinner(),
                downloadButton(outputId="dl_hematology_measured_affy", label="Download data table (measured, RG230)")
              ),
              tabPanel(title="RG230, Predicted",
                DTOutput("table_hematology_predicted_affy") %>% withSpinner(),
                downloadButton(outputId="dl_hematology_predicted_affy", label="Download data table (predicted, RG230)")
              ),
              tabPanel(title="RU1, Measured",
                DTOutput("table_hematology_measured_codelink") %>% withSpinner(),
                downloadButton(outputId="dl_hematology_measured_codelink", label="Download data table (measured, RU1)")
              ),
              tabPanel(title="RU1, Predicted",
                DTOutput("table_hematology_predicted_codelink") %>% withSpinner(),
                downloadButton(outputId="dl_hematology_predicted_codelink", label="Download data table (predicted, RU1)")
              ),

              tabPanel(title="S1500, Measured",
                 DTOutput("table_hematology_measured_s1500") %>% withSpinner(),
                 downloadButton(outputId="dl_hematology_measured_s1500", label="Download data table (measured, S1500)")
              ),
              tabPanel(title="S1500, Predicted",
                 DTOutput("table_hematology_predicted_s1500") %>% withSpinner(),
                 downloadButton(outputId="dl_hematology_predicted_s1500", label="Download data table (predicted, S1500)")
              )

            )
          )
        )
      ),
      tabPanel(title="Enrichr Results", value="tab_enrichr",
        div(style="font-size:11px;",
          HTML("
            Enrichr (<a href='https://maayanlab.cloud/Enrichr/' target='_blank'>https://maayanlab.cloud/Enrichr/</a>) references:
            <ul>
            <li>Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR, Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list enrichment analysis tool. <i>BMC Bioinformatics. 2013; 128(14)</i>.</li>
            <li>Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z, Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD, Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment analysis web server 2016 update. <i>Nucleic Acids Research. 2016; gkw377</i>.</li>
            <li>Xie Z, Bailey A, Kuleshov MV, Clarke DJB., Evangelista JE, Jenkins SL, Lachmann A, Wojciechowicz ML, Kropiwnicki E, Jagodnik KM, Jeon M, & Ma’ayan A. Gene set knowledge discovery with Enrichr. <i>Current Protocols, 1, e90. 2021. doi: 10.1002/cpz1.90</i>.</li>
            </ul>
          ")
        ),
        hr(),
        tabsetPanel(
          tabPanel(title="Loaded Genes",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                DTOutput("table_loaded_genes_affy_measured") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_lg_affy_measured", label="Download data table (measured, RG230)")
                )
              ),
              tabPanel(title="RG230, Predicted",
                DTOutput("table_loaded_genes_affy_predicted") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_lg_affy_predicted", label="Download data table (predicted, RG230)")
                )
              ),
              tabPanel(title="RU1, Measured",
                DTOutput("table_loaded_genes_codelink_measured") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_lg_codelink_measured", label="Download data table (measured, RU1)")
                )
              ),
              tabPanel(title="RU1, Predicted",
                DTOutput("table_loaded_genes_codelink_predicted") %>% withSpinner(),
                hidden(
                  downloadButton(outputId="dl_lg_codelink_predicted", label="Download data table (predicted, RU1)")
                )
              ),

              tabPanel(title="S1500, Measured",
                 DTOutput("table_loaded_genes_s1500_measured") %>% withSpinner(),
                 hidden(
                   downloadButton(outputId="dl_lg_s1500_measured", label="Download data table (measured, S1500)")
                 )
              ),
              tabPanel(title="S1500, Predicted",
                 DTOutput("table_loaded_genes_s1500_predicted") %>% withSpinner(),
                 hidden(
                   downloadButton(outputId="dl_lg_s1500_predicted", label="Download data table (predicted, S1500)")
                 )
              )

            )
          ),
          tabPanel(title="Enrichr Results (Plot)",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                uiOutput("enriched_plots_affy_measured") %>% withSpinner()
              ),
              tabPanel(title="RG230, Predicted",
                uiOutput("enriched_plots_affy_predicted") %>% withSpinner()
              ),
              tabPanel(title="RU1, Measured",
                uiOutput("enriched_plots_codelink_measured") %>% withSpinner()
              ),
              tabPanel(title="RU1, Predicted",
                uiOutput("enriched_plots_codelink_predicted") %>% withSpinner()
              ),

              tabPanel(title="S1500, Measured",
                 uiOutput("enriched_plots_s1500_measured") %>% withSpinner()
              ),
              tabPanel(title="S1500, Predicted",
                 uiOutput("enriched_plots_s1500_predicted") %>% withSpinner()
              )
            )
          ),
          tabPanel(title="Enrichr Results (Table)",
            tabsetPanel(
              tabPanel(title="RG230, Measured",
                uiOutput("enriched_tables_affy_measured") %>% withSpinner()
              ),
              tabPanel(title="RG230, Predicted",
                uiOutput("enriched_tables_affy_predicted") %>% withSpinner()
              ),
              tabPanel(title="RU1, Measured",
                uiOutput("enriched_tables_codelink_measured") %>% withSpinner()
              ),
              tabPanel(title="RU1, Predicted",
                uiOutput("enriched_tables_codelink_predicted") %>% withSpinner()
              ),
              tabPanel(title="S1500, Measured",
                 uiOutput("enriched_tables_s1500_measured") %>% withSpinner()
              ),
              tabPanel(title="S1500, Predicted",
                 uiOutput("enriched_tables_s1500_predicted") %>% withSpinner()
              )
            )
          )
        )
      ),

      tabPanel("Clustering",
        uiOutput("cluster") %>% withSpinner()
      )
    )
  )
)
