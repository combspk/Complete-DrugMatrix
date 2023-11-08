# Complete DrugMatrix
Complete DrugMatrix is a Shiny app developed in collaboration with the Oak Ridge National Laboratory to assist in predicting missing data in DrugMatrix<sup>1,2</sup>.

The app allows a user to view the gene expression, histopathology, clinical chemistry, and hematology information for the samples (chemical/dose/duration) in DrugMatrix. Gene expression data is mapped from rat gene to human gene using both the [(Affymetrix) GeneChip® RG230](https://www.thermofisher.com/order/catalog/product/901259) and the CodeLink RU1 microarrays.

The user may also enrich a set of genes pulled from the DrugMatrix samples using the [Enrichr tool](https://maayanlab.cloud/Enrichr/)<sup>3,4,5</sup>. 

We have also provided a cluster visualization in the app for the samples, split by each combination of tissue and microarray. To perform the clustering, we took the following steps:
1. Split the matrix into combinations of tissue and microarray type (not all tissues are tested with both microarrays). The matrix contains the log<sub>10</sub> ratio of treatment gene expression level vs. control gene expression level for each gene for each sample.
2. For each row of each split matrix, scale the data:  
    1. Save directionality of each log<sub>10</sub> ratio.
    2. Rank genes in descending order by the absolute value of the log<sub>10</sub> ratio.
    3. Assign the top 100 genes a rank from 100 (highest) to 1 (lowest). If two or more genes had the same log<sub>10</sub> ratio, they were given the same ranking to have a maximum of 100 ranked genes; that is, if two genes are assigned rank 2, the next highest-ranked gene is assigned rank 4. Genes not in the top 100 are assigned a rank of 0.
  2.4. Multiply each gene's directionality by its top 100 ranking.
3. Perform dimension reduction using the Uniform Manifold Approximation and Projection (UMAP) package for Python<sup>6</sup>.

## Additional software dependencies
| name | version |
|------|-------|
| R | 4.2.3 |
| stats | base |
| graphics | base |
| grDevices | base |
| utils | base |
| datasets | base |
| methods | base |
| base | base |
| visNetwork | 2.1.2 |
| stringr | 1.5.0 |
| shinyWidgets | 0.7.6 |
| shinyjs | 2.1.0 |
| shinycssloaders | 1.0.0 |
| RPostgres | 1.4.4 |
| rjson | 0.2.21 |
| enrichR | 3.2 |       
| DT | 0.28 |
| dplyr | 1.0.10 |
| DBI | 1.1.3 |
| data.table | 1.14.6 |
| shiny | 1.7.4.1 |  
| tidyselect | 1.2.0 |
| bslib | 0.5.0 |
| colorspace | 2.1-0 |
| vctrs | 0.6.3 |
| generics | 0.1.3 |
| htmltools | 0.5.5 |
| yaml | 2.3.6 |
| utf8 | 1.2.2 |
| blob | 1.2.4 |
| rlang | 1.1.1 |
| later | 1.3.1 |
| pillar | 1.9.0 |
| jquerylib | 0.1.4 |
| glue | 1.6.2 |
| withr | 2.5.0 |
| bit64 | 4.0.5 |
| lifecycle | 1.0.3 |
| munsell | 0.5.0 |
| gtable | 0.3.3 |
| htmlwidgets | 1.6.2 |
| WriteXLS | 6.4.0 |
| fastmap | 1.1.1 |
| httpuv | 1.6.11 |
| curl | 5.0.1 |
| fansi | 1.0.3 |
| Rcpp | 1.0.9 |
| xtable | 1.8-4 |
| promises | 1.2.0.1 |
| scales | 1.2.1 |
| cachem | 1.0.8 |   
| jsonlite | 1.8.7 |
| config | 0.3.1 |
| mime | 0.12 |
| bit | 4.0.5 |
| ggplot2 | 3.4.2 |
| hms | 1.1.3 |
| digest | 0.6.33 |
| stringi | 1.7.8 |
| grid | 4.2.3 |
| cli | 3.6.0 |     
| tools | 4.2.3 |
| magrittr | 2.0.3 |
| sass | 0.4.7 |
| tibble | 3.1.8 |
| pkgconfig | 2.0.3 |
| ellipsis | 0.3.2 |
| httr | 1.4.6 |
| rstudioapi | 0.15.0 |
| R6 | 2.5.1 |
| compiler | 4.2.3 |

## References
1. <sup>1</sup>https://ntp.niehs.nih.gov/data/drugmatrix
2. <sup>2</sup>https://github.com/NIEHS/DrugMatrix
3. <sup>3</sup>Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR, Ma'ayan A.
Enrichr: interactive and collaborative HTML5 gene list enrichment analysis tool. BMC Bioinformatics. 2013; 128(14).
4. <sup>4</sup>Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z, Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD, Gundersen GW, Ma'ayan A.
Enrichr: a comprehensive gene set enrichment analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377 .
5. <sup>5</sup>Xie Z, Bailey A, Kuleshov MV, Clarke DJB., Evangelista JE, Jenkins SL, Lachmann A, Wojciechowicz ML, Kropiwnicki E, Jagodnik KM, Jeon M, & Ma’ayan A.
Gene set knowledge discovery with Enrichr. Current Protocols, 1, e90. 2021. doi: 10.1002/cpz1.90
6. <sup>6</sup>https://doi.org/10.48550/arXiv.1802.03426
