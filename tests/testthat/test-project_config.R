test_that("project_config", {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)

    root <- file.path(tempdir(), paste0("workspace-test-", as.integer(stats::runif(1, 1, 1e9))))
    
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    setwd(root)
    # failed as No _project.yml file 
    expect_error(
        get_prj_config("data_dir")
    )

    config <- list(
        data_dir = "data",
        output_dir = "output",
        start = list(year = 2020, month = 1, day = 1)
    )
    project_file <- file.path(root, "_project.yml")
    yaml::write_yaml(config, project_file)

    expect_equal(get_prj_config("data_dir"), "data")
    expect_equal(get_prj_config("output_dir"), "output")
    expect_equal(get_prj_config("start")$year, 2020)
    expect_equal(get_prj_config("start")$month, 1)
    expect_equal(get_prj_config("start")$day, 1)
    expect_equal(get_prj_config("start.year"), 2020)
    expect_equal(get_prj_config("start.month"), 1)
    expect_equal(get_prj_config("start.day"), 1)
})

