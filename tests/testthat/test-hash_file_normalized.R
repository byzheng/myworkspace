# Test for hash_file_normalized

test_that("hash_file_normalized gives same hash for different EOLs", {
    skip_if_not(requireNamespace("digest", quietly = TRUE))
    tmp1 <- tempfile(fileext = ".txt")
    tmp2 <- tempfile(fileext = ".txt")
    on.exit({
        unlink(tmp1)
        unlink(tmp2)
    })
    # Write same content with different EOLs
    writeLines(c("a", "b", "c"), tmp1, sep = "\n")
    writeLines(c("a", "b", "c"), tmp2, sep = "\r\n")
    h1 <- hash_file_normalized(tmp1)
    h2 <- hash_file_normalized(tmp2)
    expect_identical(h1, h2)
})

test_that("hash_file_normalized matches digest for binary", {
    skip_if_not(requireNamespace("digest", quietly = TRUE))
    tmp <- tempfile(fileext = ".bin")
    on.exit(unlink(tmp))
    writeBin(as.raw(1:10), tmp)
    h1 <- hash_file_normalized(tmp)
    h2 <- digest::digest(file = tmp, algo = "sha256")
    expect_identical(h1, h2)
})
