library(data.table)
library(DBI)
library(dplyr)
library(DT)
library(enrichR)
library(plotly)
library(rjson)
library(RPostgres)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(visNetwork)

# Load variables from config.yml & assign to global variables
config_vars <- config::get("db")
DB_HOST <- config_vars$host
DB_PORT <- config_vars$port
DB_USER <- config_vars$user
DB_PASS <- config_vars$pass

# Define database driver
pgdrv <- RPostgres::Postgres()

# Load necessary RDS files from local storage - this is a replacement for loading them from the database.
chemicals <- as.data.table(readRDS(file = "./www/chemicals.Rds"))
tissues <- as.data.table(readRDS(file = "./www/tissues.Rds"))
chips <- as.data.table(readRDS(file = "./www/chips.Rds"))
genes_affy <- as.data.table(readRDS(file = "./www/gene_affy.Rds"))
NROW_GENES_AFFY <- nrow(genes_affy)
genes_codelink <- as.data.table(readRDS(file = "./www/gene_codelink.Rds"))
NROW_GENES_CODELINK <- nrow(genes_codelink)
genes_s1500 <- as.data.table(readRDS(file = "./www/gene_s1500.Rds"))
NROW_GENES_S1500 <- nrow(genes_s1500)
id_lookup_affy <- as.data.table(readRDS(file = "./www/mapping_affy.Rds"))
id_lookup_codelink <- as.data.table(readRDS(file = "./www/mapping_codelink.Rds"))
new_normal_bounds <- as.data.frame(fread(file = "./www/normal_ranges.txt"))

# Define functions
# Run database query on a given database
run_query <- function(query, args=list()){
    # Establish db connection
    psql_conn <-DBI::dbConnect(
        pgdrv, 
        dbname="drugmatrix_ornl_v1_bk3",
        host=DB_HOST,
        port=DB_PORT, 
        user=DB_USER,
        password=DB_PASS
    )
    df <- data.frame()
    try_query <- tryCatch({ # Attempt to query db
        res <- dbSendQuery(psql_conn, query)
        if(length(args) > 0){
            dbBind(res, args) # Bind arguments to parameterized query, if supplied
        }
        df <- dbFetch(res)
        dbClearResult(res)
    }, error=function(cond){
        print("Error in query")
        print(cond)
    })
    dbDisconnect(psql_conn) # Disconnect from db
    resp <- data.frame()
    if(nrow(df) > 0){
        resp <- df
    }
    return(resp)
}

run_search <- function(mode, predicted_only, chip, low=-5, high=5, chemicals=c(), tissues=c(), probes=c()){
    tmp <- NULL
    
  
      
    total_genes <- NROW_GENES_AFFY
    if(chip == "codelink"){
        total_genes <- NROW_GENES_CODELINK
    } else if(chip == "s1500"){
        total_genes <- NROW_GENES_S1500
    }
    
    query <- paste0("
        SELECT DISTINCT
            me.express_id,
            me.chem_id,
            ", if(chip == "s1500") "0"  else "me.chip_id", " AS chip_id,
            ", if(chip != "s1500") "me.probeset_id,", "
            ", if(chip != "s1500") "me.og_id,", "
            me.tiss_id,
            me.value,
            ", if(mode=='predicted') "me.lower, me.upper, me.confidence,", "
            me.chemical_name,
            me.time,
            me.time_unit,
            me.dose,
            me.dose_unit,
            ", if(chip == "s1500") "'S1500'"  else "me.chip_name", " AS chip_name,
            ", if(chip == "s1500") "me.probe_id"  else "pm.probe", " AS probe,
            ", if(chip != "s1500") "pm.symbol,", "
            me.tissue,
            me.", if(chip=="affy") "rg230_"  else if(chip=="codelink") "ru1_", "human_gene AS human_gene,
            me.", if(chip=="affy") "rg230_"  else if(chip=="codelink") "ru1_", "rat_gene AS rat_gene
            ", if(chip != "s1500") ", me.probeset_name", "
        FROM
            
            ", if(mode=="measured"){
                paste0("measured_expression_", chip)  
            } else if(mode == "predicted") {
                paste0(if(chip=="affy") "ornl_rg230_predicted_expression_2024_09" else if(chip=="codelink") "ornl_ru1_predicted_expression_2024_09" else "expression_s1500")
            }
            , " me
            
            ", if(chip != "s1500") ", probe_mapping pm", "
            
        WHERE
        ", if(chip != "s1500") {paste0("me.chip_id = ", if(chip=='affy') "92924910 AND" else if(chip=='codelink') "22605 AND")}, " 
        ", if(chip != "s1500") {"me.probeset_name = pm.pid"}, "
        ", if(chip != "s1500") {paste0("AND pm.probetype = ", if(chip=='affy') "'RG230-2'" else if(chip=='codelink') "'RU1'")}, "
        ", if(chip != "s1500") {"AND pm.symbol IS NOT NULL AND"}, "
        me.value BETWEEN $1 AND $2
        ", if(length(chemicals) > 0) paste0("AND me.chem_id IN (", paste0(lapply(seq(3, 2 + length(chemicals)), function(num) paste0("$", num)), collapse=","), ")") , "
        ", if(length(tissues) > 0) paste0("AND me.tiss_id IN (", paste0(lapply(seq(3 + length(chemicals), 2 + length(chemicals) + length(tissues)), function(num) paste0("$", num)), collapse=","), ")") , "
        ", if(chip != "s1500") {paste0(if(length(probes) > 0 & length(probes) != total_genes) paste0("AND me.probeset_name IN (", paste0(lapply(seq(3 + length(chemicals) + length(tissues), 2 + length(chemicals) + length(tissues) + length(probes)), function(num) paste0("$", num)), collapse=","), ")"))}, "
        AND ", if(chip=="affy") "rg230_"  else if(chip=="codelink") "ru1_", "human_gene != 'no gene linked'
        AND ", if(chip=="affy") "rg230_"  else if(chip=="codelink") "ru1_", "rat_gene != 'no gene linked'
    ")
    if(chip != "s1500"){
        tmp <- run_query(query, args=c(as.list(low), as.list(high), as.list(chemicals), as.list(tissues)))
    } else {
        if(length(probes) == total_genes){
            tmp <- run_query(query, args=c(as.list(low), as.list(high), as.list(chemicals), as.list(tissues)))
        } else {
            tmp <- run_query(query, args=c(as.list(low), as.list(high), as.list(chemicals), as.list(tissues), as.list(probes)))
        }
    }
    if(predicted_only == TRUE){
        query <- paste0("SELECT DISTINCT express_id FROM mv_", if(chip=="affy") "rg230" else if(chip=="codelink") "ru1" else "s1500", "_pred_only WHERE chemical_name IN (", paste0(lapply(seq_len(length(unique(tmp$chemical_name))), function(x) paste0("$", x)), collapse=", "), ")")
        ids <- run_query(query, args=c(as.list(unique(tmp$chemical_name))))
        tmp <- tmp[tmp$express_id %in% ids$express_id, ]
        
    }

    tmp <- as.data.table(tmp)
    
    return(tmp)
}


# predonly_rg230 <- run_query("SELECT * FROM mv_rg230_pred_only")
# predonly_rg230 <- saveRDS(predonly_rg230, "./predonly_rg230.RDS")
# print("done 1")
# predonly_ru1 <- run_query("SELECT * FROM mv_ru1_pred_only")
# predonly_ru1 <- saveRDS(predonly_ru1, "./predonly_ru1.RDS")
# print("done 2")
# predonly_s1500 <- run_query("SELECT * FROM mv_s1500_pred_only")
# predonly_s1500 <- saveRDS(predonly_s1500, "./predonly_s1500.RDS")
# print("done 3")
# # predonly_rg230 <- readRDS("./predonly_rg230.RDS")
# # predonly_ru1 <- readRDS("./predonly_ru1.RDS")
# # predonly_s1500 <- readRDS("./predonly_s1500.RDS")