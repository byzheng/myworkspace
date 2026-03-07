test_that("create_external_sentinel creates valid sentinel file", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure in temp directory
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    # Initialize here anchor at project root
    writeLines("", "README.md")
    here::i_am("README.md")
    
    # Create input files
    dir.create("data", showWarnings = FALSE)
    saveRDS(mtcars, "data/input1.rds")
    saveRDS(iris, "data/input2.rds")
    
    # Create sentinel
    result <- create_external_sentinel(
        sentinel_path = ".external/sentinel.json",
        input_files = c("data/input1.rds", "data/input2.rds"),
        metadata = list(job_id = "12345", nodes = 4)
    )
    
    expect_equal(result, ".external/sentinel.json")
    expect_true(file.exists(".external/sentinel.json"))
    
    # Read and validate contents
    sentinel_data <- jsonlite::read_json(".external/sentinel.json")
    expect_true("completed_at" %in% names(sentinel_data))
    expect_true("input_files" %in% names(sentinel_data))
    expect_true("metadata" %in% names(sentinel_data))
    expect_equal(length(sentinel_data$input_files), 2)
    expect_equal(sentinel_data$metadata$job_id, "12345")
    expect_equal(sentinel_data$metadata$nodes, 4)
    # Verify relative paths are stored, not absolute
    expect_true("data/input1.rds" %in% names(sentinel_data$input_files))
    # Ensure only 'hash' field is present for each input file
    for (info in sentinel_data$input_files) {
        expect_equal(names(info), "hash")
        expect_type(info$hash, "character")
    }
})

test_that("create_external_sentinel validates input_files", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- file.path(test_proj, "test_sentinel.json")
    
    expect_error(
        create_external_sentinel(sentinel_path, character(0)),
        "length.*> 0"
    )
    
    expect_error(
        create_external_sentinel(sentinel_path, 123),
        "is.character.*input_files"
    )
})

test_that("create_external_sentinel warns about missing input files", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    nonexistent_file <- "does_not_exist.rds"
    
    expect_error(
        create_external_sentinel(sentinel_path, nonexistent_file)
    )
})

test_that("check_external_sentinel validates existing sentinel", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    # Create mock input file
    saveRDS(mtcars, input1)
    # Create sentinel
    create_external_sentinel(sentinel_path, input1)
    
    # Check should pass
    result <- check_external_sentinel(sentinel_path, input1)
    expect_equal(normalizePath(result, winslash = "/"), normalizePath(sentinel_path, winslash = "/"))
})


test_that("check_external_sentinel stops when sentinel missing", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "nonexistent_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    saveRDS(mtcars, input1)
    
    expect_error(
        check_external_sentinel(sentinel_path, input1, on_missing = "stop"),
        "External process not completed yet"
    )
})

test_that("check_external_sentinel warns when sentinel missing and on_missing='warn'", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "nonexistent_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    saveRDS(mtcars, input1)
    
    expect_warning(
        result <- check_external_sentinel(sentinel_path, input1, on_missing = "warn"),
        "External process not completed yet"
    )
    expect_null(result)
})

test_that("check_external_sentinel detects stale inputs", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    # Create input and sentinel
    saveRDS(mtcars, input1)
    create_external_sentinel(sentinel_path, input1)
    
    expect_no_error(check_external_sentinel(sentinel_path, input1))
    
    # Wait and modify input file (need >1 sec for staleness detection)
    Sys.sleep(1.5)
    saveRDS(iris, input1)
    
    expect_error(
        check_external_sentinel(sentinel_path, input1, on_stale = "stop"),
        "Input data changed after external process completed"
    )
})

test_that("check_external_sentinel deletes stale sentinel when on_stale='delete'", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    # Create input and sentinel
    saveRDS(mtcars, input1)
    create_external_sentinel(sentinel_path, input1)
    
    # Wait and modify input file (need >1 sec for staleness detection)
    Sys.sleep(1.5)
    saveRDS(iris, input1)
    
    expect_error(
        check_external_sentinel(sentinel_path, input1, on_stale = "delete"),
        "Input data changed after external process completed"
    )
    # Sentinel should be deleted
    expect_false(file.exists(sentinel_path))
    # Clean up
    unlink(input1)
})

test_that("check_external_sentinel detects missing tracked inputs", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    input2 <- "data/input2.csv"
    
    # Create only input1, not input2
    saveRDS(mtcars, input1)
    create_external_sentinel(sentinel_path, input1)
    
    # Now create input2 and check with both inputs
    write.csv(iris, input2, row.names = FALSE)
    
    expect_error(
        check_external_sentinel(sentinel_path, c(input1, input2), on_stale = "stop"),
        "External process sentinel is incomplete"
    )
})

test_that("check_external_sentinel handles corrupted sentinel", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    # Create corrupted JSON file
    writeLines("not valid json {{{", sentinel_path)
    saveRDS(mtcars, input1)
    
    expect_error(
        check_external_sentinel(sentinel_path, input1),
        "Sentinel file corrupted"
    )
    # Sentinel should be deleted
    expect_false(file.exists(sentinel_path))
    # Clean up
    unlink(input1)
})

test_that("check_external_sentinel validates input_files parameter", {
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    
    expect_error(
        check_external_sentinel(sentinel_path, character(0)),
        "length.*> 0"
    )
    expect_error(
        check_external_sentinel(sentinel_path, 123),
        "is.character.*input_files"
    )
})

test_that("get_external_sentinel_metadata retrieves metadata", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    
    # Create input and sentinel
    saveRDS(mtcars, input1)
    create_external_sentinel(
        sentinel_path, 
        input1,
        metadata = list(job_id = "99999", cluster = "external01")
    )
    
    # Get metadata
    meta <- get_external_sentinel_metadata(sentinel_path)
    
    expect_true("completed_at" %in% names(meta))
    expect_true("input_files" %in% names(meta))
    expect_true("metadata" %in% names(meta))
    expect_equal(meta$metadata$job_id, "99999")
    expect_equal(meta$metadata$cluster, "external01")
})

test_that("get_external_sentinel_metadata stops when file missing", {
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "nonexistent_sentinel.json"
    
    expect_error(
        get_external_sentinel_metadata(sentinel_path),
        "Sentinel file not found"
    )
})



test_that("check_external_sentinel detects stale inputs in multiple targets", {
    skip_if_not_installed("jsonlite")
    skip_if_not_installed("here")
    
    # Create a test project structure
    test_proj <- tempfile("external_test_")
    dir.create(test_proj)
    
    old_wd <- getwd()
    setwd(test_proj)
    on.exit({
        setwd(old_wd)
        unlink(test_proj, recursive = TRUE, force = TRUE)
    }, add = FALSE)
    
    writeLines("", "README.md")
    here::i_am("README.md")
    
    sentinel_path <- "test_sentinel.json"
    sentinel_path2 <- "test_sentinel2.json"
    dir.create("data", showWarnings = FALSE)
    input1 <- "data/input1.rds"
    input2 <- "data/input2.rds"
    # Create input and sentinel
    saveRDS(mtcars, input1)
    saveRDS(iris, input2)
    create_external_sentinel(sentinel_path, input1)
    create_external_sentinel(sentinel_path2, c(sentinel_path, input2))
    # File created. no change should be detected
    expect_no_error(check_external_sentinel(sentinel_path, input1))
    expect_no_error(check_external_sentinel(sentinel_path2, c(sentinel_path, input2)))
    # Wait and modify input file (need >1 sec for staleness detection)
    Sys.sleep(1.5)
    # Save file contents, no changes should be detected
    saveRDS(mtcars, input1)
    expect_no_error(check_external_sentinel(sentinel_path, input1))
    expect_no_error(check_external_sentinel(sentinel_path2, c(sentinel_path, input2)))

    # Recreate sentinel with same input, no change should be detected
    create_external_sentinel(sentinel_path, input1)
    expect_no_error(check_external_sentinel(sentinel_path, input1))
    # failed as the target 1 is updated
    expect_error(check_external_sentinel(sentinel_path2, c(sentinel_path, input2)))

    # File 1 is changed. Sentinel should detect staleness
    Sys.sleep(1.5)
    saveRDS(iris, input1)
    expect_error(
        check_external_sentinel(sentinel_path, input1, on_stale = "stop"),
        "Input data changed after external process completed"
    )    
    expect_error(check_external_sentinel(sentinel_path2, c(sentinel_path, input2)))
    # Recreate sentinel 1 with updated input1, should be valid now
    create_external_sentinel(sentinel_path, input1)
    expect_no_error(check_external_sentinel(sentinel_path, input1))
    expect_error(
        check_external_sentinel(sentinel_path2, c(sentinel_path, input2), on_stale = "stop"),
        "Input data changed after external process completed"
    )        
    create_external_sentinel(sentinel_path2, c(sentinel_path, input2))
    expect_no_error(check_external_sentinel(sentinel_path2, c(sentinel_path, input2)))
    
})
