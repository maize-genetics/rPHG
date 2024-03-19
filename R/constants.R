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
    "DEMO_N_RR_SIZE"     = 5,
    "DEMO_N_RR_TOTAL"    = 25,
    "DEMO_N_SAMPLES"     = 5,
    "MAX_N_RR_SIZE"      = 5000,
    "MAX_N_RR_TOTAL"     = 150000,
    "MAX_N_SAMPLES"      = 10000,
    "PAGE_SIZE"          = "pageSize=%i",
    "REST_QUERY"         = "?",
    "REST_KV_SEP"        = "&",
    "METHOD_ID_KEY"      = "variantSetDbId=%s",
    "METHOD_RR_SIZE"     = "dimensionCallSetPageSize=%i",
    "METHOD_RR_PAGE"     = "dimensionCallSetPage=%i",
    "METHOD_SAMPLE_SIZE" = "dimensionVariantPageSize=%i",
    "METHOD_SAMPLE_PAGE" = "dimensionVariantPage=%i"
)


## ----
# TASSEL and PHG class calls for rJava
TASSEL_API <- list(
    "BUILD_GRAPH_FROM_PATHS"  = "net/maizegenetics/pangenome/api/BuildGraphFromPathsPlugin",
    "DATA_SET"                = "net/maizegenetics/plugindef/DataSet",
    "DB_LOADING_UTILS"        = "net/maizegenetics/pangenome/db_loading/DBLoadingUtils",
    "FRAME"                   = "java/awt/Frame",
    "HAPLOTYPE_GRAPH_BUILDER" = "net/maizegenetics/pangenome/api/HaplotypeGraphBuilderPlugin",
    "LOGGING_UTILS"           = "net/maizegenetics/util/LoggingUtils",
    "METHOD_TABLE_REPORT"     = "net/maizegenetics/pangenome/api/MethodTableReportPlugin",
    "PARAMETER_CACHE"         = "net/maizegenetics/plugindef/ParameterCache",
    "R_METHODS"               = "net/maizegenetics/pangenome/api/RMethods",
    "RESULT_SET"              = "java/sql/ResultSet",
    "VECTOR"                  = "java/util/Vector"
)


