# ToxCompl: Drugmatrix Completion
ToxCompl: Drugmatrix Completion is a Shiny app developed in collaboration with the Oak Ridge National Laboratory to assist in predicting missing data in DrugMatrix<sup>1,2</sup>.

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
Each chemical in these clusters are colored by their associated mode of action(s) as defined in ChEMBL<sup>26, 27, 28</sup>.


## Additional software dependencies
| name | version | citation |
|------|-------|-------|
| RStudio | 2023.09.1 build 494 | <sup>8</sup> |
| R | 4.2.3 | <sup>9</sup> |
| stats | base | <sup>9</sup> |
| graphics | base | <sup>9</sup> |
| grDevices | base | <sup>9</sup> |
| utils | base | <sup>9</sup> |
| datasets | base | <sup>9</sup> |
| methods | base | <sup>9</sup> |
| base | base | <sup>9</sup> |
| visNetwork | 2.1.2 | <sup>10</sup> |
| stringr | 1.5.0 | <sup>11</sup> |
| shinyWidgets | 0.7.6 | <sup>12</sup> |
| shinyjs | 2.1.0 | <sup>13</sup> |
| shinycssloaders | 1.0.0 | <sup>14</sup> |
| RPostgres | 1.4.4 | <sup>15</sup> |
| rjson | 0.2.21 | <sup>16</sup> |
| enrichR | 3.2 | <sup>17</sup> |
| DT | 0.28 | <sup>18</sup> |
| dplyr | 1.0.10 | <sup>19</sup> |
| DBI | 1.1.3 | <sup>20</sup> |
| data.table | 1.14.6 | <sup>21</sup> |
| shiny | 1.7.4.1 | <sup>22</sup> |
| scrypt | 0.1.6 | <sup>23</sup> |
| shinymanager | 1.0.410 | <sup>24</sup> |
| tidyverse | 2019 | <sup>25</sup> |

## References
1. https://ntp.niehs.nih.gov/data/drugmatrix
2. https://github.com/NIEHS/DrugMatrix
3. Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR, Ma'ayan A.
Enrichr: interactive and collaborative HTML5 gene list enrichment analysis tool. BMC Bioinformatics. 2013; 128(14).
4. Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z, Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD, Gundersen GW, Ma'ayan A.
Enrichr: a comprehensive gene set enrichment analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377 .
5. Xie Z, Bailey A, Kuleshov MV, Clarke DJB., Evangelista JE, Jenkins SL, Lachmann A, Wojciechowicz ML, Kropiwnicki E, Jagodnik KM, Jeon M, & Ma’ayan A.
Gene set knowledge discovery with Enrichr. Current Protocols, 1, e90. 2021. doi: 10.1002/cpz1.90
6. https://doi.org/10.48550/arXiv.1802.03426
8. Posit team (2023). _RStudio: Integrated Development Environment for R_. Posit Software, PBC, Boston, MA. <http://www.posit.co/>.
9. R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
10. Almende BV and Contributors, Thieurmel B (2022). _visNetwork: Network Visualization using 'vis.js' Library_. R package version 2.1.2, <https://CRAN.R-project.org/package=visNetwork>.
11. Wickham H (2022). _stringr: Simple, Consistent Wrappers for Common String Operations_. R package version 1.5.0, <https://CRAN.R-project.org/package=stringr>.
12. Perrier V, Meyer F, Granjon D (2023). _shinyWidgets: Custom Inputs Widgets for Shiny_. R package version 0.7.6, <https://CRAN.R-project.org/package=shinyWidgets>.
13. Attali D (2021). _shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds_. R package version 2.1.0, <https://CRAN.R-project.org/package=shinyjs>.
14. Sali A, Attali D (2020). _shinycssloaders: Add Loading Animations to a 'shiny' Output While It's Recalculating_. R package version 1.0.0, <https://CRAN.R-project.org/package=shinycssloaders>.
15. Wickham H, Ooms J, Müller K (2022). _RPostgres: Rcpp Interface to PostgreSQL_. R package version 1.4.4, <https://CRAN.R-project.org/package=RPostgres>.
16. Couture-Beil A (2022). _rjson: JSON for R_. R package version 0.2.21, <https://CRAN.R-project.org/package=rjson>.
17. Jawaid W (2023). _enrichR: Provides an R Interface to 'Enrichr'_. R package version 3.2, <https://CRAN.R-project.org/package=enrichR>.
18. Xie Y, Cheng J, Tan X (2023). _DT: A Wrapper of the JavaScript Library 'DataTables'_. R package version 0.28, <https://CRAN.R-project.org/package=DT>.
19. Wickham H, François R, Henry L, Müller K (2022). _dplyr: A Grammar of Data Manipulation_. R package version 1.0.10, <https://CRAN.R-project.org/package=dplyr>.
20. R Special Interest Group on Databases (R-SIG-DB), Wickham H, Müller K (2022). _DBI: R Database Interface_. R package version 1.1.3, <https://CRAN.R-project.org/package=DBI>.
21. Dowle M, Srinivasan A (2022). _data.table: Extension of `data.frame`_. R package version 1.14.6, <https://CRAN.R-project.org/package=data.table>.
22. Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2023). _shiny: Web Application Framework for R_. R package version 1.7.4.1, <https://CRAN.R-project.org/package=shiny>.
23. Kipp A and Percival C (2023). _scrypt: Key Derivation Functions for R based on Scrypt_. R package version 0.1.6, <https://CRAN.R-project.org/package=scrypt>.
24. Thieurmel B and Perrier V (2022). _shinymanager: Authentication Management for Shiny Applications_. R package version 1.0.410, <https://CRAN.R-project.org/package=shinymanager>.
25. Wickham H, Averick M, Bryan J, Chang W, D'Agostino L, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo, Yutani H (2019). _Welcome to the tidyverse_. Journal of Open Source Software. 2019; 4:43, 1686. doi: 10.21105/joss.01686.
26. https://www.ebi.ac.uk/chembl/
27. Zdrazil B, Felix E, Hunter F, Manners EJ, Blackshaw J, Corbett S, de Veij M, Ioannidis H, Mendez Lopez D, Mosquera JF, Magarinos MP, Bosc N, Arcila R, Kizilören T, Gaulton A, Bento AP, Adasme MF, Monecke P, Landrum GA, Leach AR. _The ChEMBL Database in 2023: a drug discovery platform spanning multiple bioactivity data types and time periods_. Nucleic Acids Research, Volume 52, Issue D1, 5 January 2024, Pages D1180–D1192, https://doi.org/10.1093/nar/gkad1004.
28. Davies M, Nowotka M, Papadatos G, Dedman N, Gaulton A, Atkinson F, Bellis L, Overington JP. _ChEMBL web services: streamlining access to drug discovery data and utilities_. Nucleic Acids Res. 2015 Jul 1;43(W1):W612-20. doi: 10.1093/nar/gkv352. Epub 2015 Apr 16. PMID: 25883136; PMCID: PMC4489243.
