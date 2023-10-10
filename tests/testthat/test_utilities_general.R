test_that("Basic tests", {
    configFile1 <- tempfile()
    writeLines(
        c(
            "host=localhost",
            "DBtype=sqlite",
            "user=user",
            "password=pass"
        ),
        con = configFile1
    )

    expect_error(
        object = rPHG:::configCatcher(configFile1),
        regexp = "Some mandatory connection fields are missing"
    )

    configFile2 <- tempfile()
    writeLines(
        c(
            "host=localhost",
            "DBtype=postgres",
            "DB=my_phg",
            "user=user",
            "user=user",
            "password=pass",
            "password=pass"
        ),
        con = configFile2
    )
    expect_error(
        object = rPHG:::configCatcher(configFile2),
        regexp = "Some mandatory connection fields are duplicated"
    )

    configFile3 <- tempfile()
    writeLines(
        c(
            "host=localhost",
            "DBtype=postgressss",
            "DB=my_phg",
            "user=user",
            "password=pass"
        ),
        con = configFile3
    )
    expect_error(
        object = rPHG:::configCatcher(configFile3),
    )

    configFile4 <- tempfile()
    writeLines(
        c(
            "host=localhost",
            "DBtype=sqlite",
            "DB=/does/not/exist",
            "user=user",
            "password=pass"
        ),
        con = configFile4
    )
    expect_error(
        object = rPHG:::configCatcher(configFile4)
    )
})


