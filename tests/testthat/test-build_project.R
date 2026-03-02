test_that("build_project runs without error and handles missing files gracefully", {
    skip_if_not_installed("targets")
    skip_if_not_installed("quarto")
    skip_if_not_installed("withr")
    tmp <- tempfile("prj_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    oldwd <- setwd(tmp)
    on.exit(setwd(oldwd), add = TRUE)
    # No _targets.R or pipeline_targets.qmd present
    expect_error(build_project())
    # Create _targets.R
    file.create(".project")
    expect_error(build_project())
    file.create("_targets.R")
    expect_no_error(build_project())
    # Create pipeline_targets.qmd
    file.create("pipeline_targets.qmd")
})
