test_that("Basic tests", {
    logFile    <- tempfile(fileext = ".txt")
    startLogger(logFile)

    testUrl <- "phg.maizegdb.org"
    # testUrl <- "demo.hub.maizegenetics.net"

    phgSrvCon <- PHGServerCon(testUrl)
    phgSrvConOutput <- utils::capture.output(phgSrvCon)

    expect_true(is(phgSrvCon, "PHGServerCon"))
    expect_true(inherits(phgSrvCon, "PHGCon"))
    expect_true(is(brapiURL(phgSrvCon), "character"))
    expect_true(is(brapiVersion(phgSrvCon), "character"))
    expect_true(is(port(phgSrvCon), "numeric"))
    expect_true(is(httProtocol(phgSrvCon), "character"))
    expect_true(is(serverInfo(phgSrvCon), "tbl"))
    expect_true(is(showPHGMethods(phgSrvCon), "tbl"))

    expect_equal(length(phgSrvConOutput), 3)
    expect_equal(
        object = httProtocol(PHGServerCon(testUrl, protocol = "https")),
        expected = "https"
    )
    expect_equal(
        object = httProtocol(PHGServerCon(testUrl, protocol = "http")),
        expected = "http"
    )
    expect_equal(
        object = httProtocol(phgSrvCon),
        expected = "https"
    )
    expect_equal(
        object = port(PHGServerCon(testUrl, protocol = "https")),
        expected = 443
    )
    expect_equal(
        object = port(phgSrvCon),
        expected = 443
    )
    expect_equal(
        object = port(PHGServerCon(testUrl, protocol = "http")),
        expected = 80
    )
    expect_equal(
        object = httProtocol(PHGServerCon(paste0("https://", testUrl))),
        expected = "https"
    )
    expect_equal(
        object = httProtocol(PHGServerCon(paste0("http://", testUrl))),
        expected = "http"
    )

    expect_error(
        object = PHGServerCon(testUrl, port = -1),
        regexp = "Not a valid port number"
    )
    expect_error(object = PHGServerCon(testUrl, protocol = "htp"))
    expect_error(object = PHGServerCon(testUrl, version = "v3"))
    expect_error(object = PHGServerCon("www.google.com"))

})



