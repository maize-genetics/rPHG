## ----
#' Convert PHG HashMap to tibble (house-keeping)
tnHashMapToTibble <- function(x) {
    rrNames <- names(x)
    hapNames <- lapply(x, names)

    rrNamesVec <- lapply(seq_along(hapNames), function(i) {
        rep(rrNames[i], length(hapNames[[i]]))
    }) |> unlist()

    hapNamesVec <- unlist(hapNames)
    taxaIdVec <- lapply(seq_along(hapNames), function(i) {
        tmpCache <- x[[i]]
        lapply(seq_along(tmpCache), function(j) {
            tmpCache[[j]]
        })
    })

    return(
        tibble::tibble(
            ref_range_id = rrNamesVec,
            hap_id = hapNamesVec,
            taxa_id = taxaIdVec |> unlist(recursive = FALSE)
        )
    )
}


