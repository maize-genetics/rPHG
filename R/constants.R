## ----
# Specified BrAPI endpoints
BRAPI_ENDPOINTS <- list(
    "METHOD_TABLE"   = "allelematrix",
    "SAMPLES"        = "samples",
    "SERVER_INFO"    = "serverinfo",
    "VARIANT_TABLES" = "variantTables",
    "VARIANTS"       = "variants"
)


## ----
# Commonly used BrAPI parameters
BRAPI_PARAMS <- list(
    "PAGE_SIZE" =  "pageSize=%i"
)


## ----
# TASSEL and PHG class calls for rJava
TASSEL_API <- list(
    "BUILD_GRAPH_FROM_PATHS"  = "net/maizegenetics/pangenome/api/BuildGraphFromPathsPlugin",
    "DATA_SET"                = "net/maizegenetics/plugindef/DataSet",
    "DB_LOADING_UTILS"        = "net/maizegenetics/pangenome/db_loading/DBLoadingUtils",
    "FRAME"                   = "java/awt/Frame",
    "HAPLOTYPE_GRAPH_BUILDER" = "net/maizegenetics/pangenome/api/HaplotypeGraphBuilderPlugin",
    "METHOD_TABLE_REPORT"     = "net/maizegenetics/pangenome/api/MethodTableReportPlugin",
    "PARAMETER_CACHE"         = "net/maizegenetics/plugindef/ParameterCache",
    "R_METHODS"               = "net/maizegenetics/pangenome/api/RMethods",
    "RESULT_SET"              = "java/sql/ResultSet"
)
