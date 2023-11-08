test_that("Basic tests.", {
    expect_silent(
        .onLoad(pkgname = "rPHG", libname = system.file(package = "rPHG"))
    )
})
