#' @title Get DB PHG methods for graph building
#'
#' @description Gets all available PHG methods from the graph database
#'   using a path parameter to the database configuration file.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#'
#' @importFrom rJava .jcast
#' @importFrom rJava .jnull
#' @importFrom rJava J
#' @importFrom rJava new
#' @importFrom tibble tibble
#'
#' @export
showPHGMethods <- function(configFile) {
    
    configCatcher(configFile)

    ## Get table report plugin and pull data from DB
    plugin <- rJava::new(
        rJava::J("net/maizegenetics/pangenome/api/MethodTableReportPlugin")
    )
    plugin <- plugin$configFile(configFile)
    ds <- plugin$performFunction(
        rJava::.jnull("net/maizegenetics/plugindef/DataSet")
    )
    datum <- ds$getData(0L)
    tabRep <- rJava::.jcast(
        datum$getData(),
        new.class = "net/maizegenetics/util/TableReport"
    )
    resultVectors <- rJava::J(
        "net/maizegenetics/plugindef/GenerateRCode",
        "tableReportToVectors",
        tabRep
    )

    ## Get data vectors
    data <- resultVectors$dataVector

    ## Convert to native R data frame
    dfMethods <- tibble::tibble(
        data$get(0L),
        data$get(1L),
        data$get(2L),
        data$get(3L),
        data$get(4L)
    )

    ## Convert names
    names(dfMethods) <- resultVectors$columnNames

    ## Return object
    return(dfMethods)
}
