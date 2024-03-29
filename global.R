library(data.table)
library(DBI)
library(dplyr)
library(DT)
library(enrichR)
library(plotly)
library(rjson)
library(RPostgres)
library(shiny)
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
genes_codelink <- as.data.table(readRDS(file = "./www/gene_codelink.Rds"))
id_lookup_affy <- as.data.table(readRDS(file = "./www/mapping_affy.Rds"))
id_lookup_codelink <- as.data.table(readRDS(file = "./www/mapping_codelink.Rds"))

# Define functions
# Run database query on a given database
run_query <- function(query, args=list()){
    # Establish db connection
    psql_conn <-DBI::dbConnect(
        pgdrv, 
        dbname="drugmatrix_ornl_v1",
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
    query <- paste0("
        SELECT DISTINCT
            me.express_id,
            me.chem_id,
            me.chip_id,
            me.probeset_id,
            me.og_id,
            me.tiss_id,
            me.value,
            ", if(mode=='predicted') "me.lower, me.upper, me.confidence,", "
            me.chemical_name,
            me.time,
            me.time_unit,
            me.dose,
            me.dose_unit,
            me.chip_name,
            me.tissue,
            me.", if(chip=="affy") "rg230"  else "ru1", "_human_gene AS human_gene,
            me.", if(chip=="affy") "rg230"  else "ru1", "_rat_gene AS rat_gene,
            me.probeset_name
        FROM
            ", mode, "_expression_", chip, if(predicted_only==TRUE) paste0("_only"), " me
        WHERE
            me.chip_id = ", if(chip=='affy') "92924910" else "22605", " 
        AND me.value BETWEEN $1 AND $2
        ", if(length(chemicals) > 0) paste0("AND me.chem_id IN (", paste0(lapply(seq(3, 2 + length(chemicals)), function(num) paste0("$", num)), collapse=","), ")") , "
        ", if(length(tissues) > 0) paste0("AND me.tiss_id IN (", paste0(lapply(seq(3 + length(chemicals), 2 + length(chemicals) + length(tissues)), function(num) paste0("$", num)), collapse=","), ")") , "
        ", if(length(probes) > 0) paste0("AND me.probeset_name IN (", paste0(lapply(seq(3 + length(chemicals) + length(tissues), 2 + length(chemicals) + length(tissues) + length(probes)), function(num) paste0("$", num)), collapse=","), ")") , "
        AND ", if(chip=="affy") "rg230"  else "ru1", "_human_gene != 'no gene linked'
        AND ", if(chip=="affy") "rg230"  else "ru1", "_rat_gene != 'no gene linked'
    ")
    return(run_query(query, args=c(as.list(low), as.list(high), as.list(chemicals), as.list(tissues), as.list(probes))))
}