test_that("load_functions sources R files from provided folders", {
    skip_if_not_installed("here")

    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    setwd(root)

    test_folder_name <- paste0("tmp-load-functions-", as.integer(stats::runif(1, 1, 1e9)))
    test_folder <- file.path(root, test_folder_name)
    dir.create(test_folder, recursive = TRUE, showWarnings = FALSE)

    on.exit(unlink(test_folder, recursive = TRUE, force = TRUE), add = TRUE)

    test_value <- "load_functions_test_value"
    if (exists(test_value, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = test_value, envir = .GlobalEnv)
    }
    on.exit({
        if (exists(test_value, envir = .GlobalEnv, inherits = FALSE)) {
            rm(list = test_value, envir = .GlobalEnv)
        }
    }, add = TRUE)

    writeLines("load_functions_test_value <- 42", file.path(test_folder, "helper.R"))
    reset_prj_config()
    expect_warning(
        load_functions()
    )
    writeLines(sprintf("
functions:
    - %s/helper.R
", basename(test_folder)), 
    file.path(root,  "_project.yml"))
    reset_prj_config()
    load_functions()
    expect_true(exists(test_value, envir = .GlobalEnv, inherits = FALSE))
    expect_identical(get(test_value, envir = .GlobalEnv), 42)
})
