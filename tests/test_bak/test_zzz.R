# === Tests for initializer methods =================================

test_that("onLoad function is called without error", {
    expect_silent(
        .onLoad(pkgname = "rPHG", libname = system.file(package = "rPHG"))
    )
})

