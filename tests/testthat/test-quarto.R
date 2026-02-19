test_that("get_param returns matching value from params", {
    f <- get_param
    environment(f) <- list2env(
        list(params = list(alpha = 123)),
        parent = environment(get_param)
    )

    expect_equal(f("alpha"), 123)
})

test_that("get_param uses default_fun when param is missing", {
    expect_equal(get_param("missing", function() 10), 10)
    expect_equal(get_param("missing", function(x, y) x + y, 2, 3), 5)
})

test_that("get_param treats NULL value as missing", {
    f <- get_param
    environment(f) <- list2env(
        list(params = list(alpha = NULL)),
        parent = environment(get_param)
    )

    expect_equal(f("alpha", function() "fallback"), "fallback")
})

test_that("get_param validates inputs", {
    expect_error(get_param(1), "is.character")
    expect_error(get_param(c("a", "b")), "length")
    expect_error(get_param("a", default_fun = 1), "is.function")
})

test_that("read_data reads RDS file", {
    subdir <- paste0("tmp-quarto-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
    rel_dir <- file.path("tests", "testthat", subdir)
    abs_dir <- here::here(rel_dir)
    dir.create(abs_dir, recursive = TRUE)
    on.exit(unlink(abs_dir, recursive = TRUE), add = TRUE)

    obj <- list(a = 1, b = "x")
    saveRDS(obj, file.path(abs_dir, "x.rds"))

    result <- read_data(file.path(rel_dir, "x.rds"), as_tibble = FALSE)

    expect_equal(result, obj)
})

test_that("read_data reads CSV file", {
    subdir <- paste0("tmp-quarto-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
    rel_dir <- file.path("tests", "testthat", subdir)
    abs_dir <- here::here(rel_dir)
    dir.create(abs_dir, recursive = TRUE)
    on.exit(unlink(abs_dir, recursive = TRUE), add = TRUE)

    write.csv(data.frame(a = 1:2, b = c("x", "y")),
        file.path(abs_dir, "x.csv"), row.names = FALSE
    )

    result <- read_data(file.path(rel_dir, "x.csv"), as_tibble = FALSE)

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_equal(names(result), c("a", "b"))
})

test_that("read_data converts to tibble when requested and available", {
    skip_if_not_installed("tibble")

    subdir <- paste0("tmp-quarto-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
    rel_dir <- file.path("tests", "testthat", subdir)
    abs_dir <- here::here(rel_dir)
    dir.create(abs_dir, recursive = TRUE)
    on.exit(unlink(abs_dir, recursive = TRUE), add = TRUE)

    write.csv(data.frame(a = 1:2), file.path(abs_dir, "x.csv"), row.names = FALSE)

    result <- read_data(file.path(rel_dir, "x.csv"), as_tibble = TRUE)

    expect_s3_class(result, "tbl_df")
})

test_that("read_data validates file type and arguments", {
    subdir <- paste0("tmp-quarto-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
    rel_dir <- file.path("tests", "testthat", subdir)
    abs_dir <- here::here(rel_dir)
    dir.create(abs_dir, recursive = TRUE)
    on.exit(unlink(abs_dir, recursive = TRUE), add = TRUE)

    writeLines("abc", file.path(abs_dir, "x.txt"))

    expect_error(read_data(file.path(rel_dir, "x.txt")), "Unsupported file type")
    expect_error(read_data(1), "is.character")
    expect_error(read_data(file.path(rel_dir, "x.txt"), as_tibble = c(TRUE, FALSE)), "length")
    expect_error(read_data(file.path(rel_dir, "missing.csv")), "file.exists")
})
