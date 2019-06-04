.onLoad <- function(libname, pkgname) {
    ## Initialize Jar
    rJava::.jpackage(pkgname, lib.loc = libname)
    rJava::.jaddClassPath(dir(file.path(getwd(), "inst/java"), full.names = TRUE))
}

.onAttach <- function(libname, pkgname) {
    msg <- paste0(
        "Welcome to rPHG (version: ", utils::packageVersion("rPHG"), ")", "\n"
    )
    packageStartupMessage(msg)
}
