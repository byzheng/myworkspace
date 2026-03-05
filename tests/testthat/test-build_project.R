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


test_that("build_project runs with init_project without workspace", {
    skip_if_not_installed("targets")
    skip_if_not_installed("quarto")
    skip_if_not_installed("withr")
    tmp <- tempfile("prj_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    oldwd <- setwd(tmp)
    on.exit(setwd(oldwd), add = TRUE)
    
    init_project("TestProject")
    # No _targets.R or pipeline_targets.qmd present
    expect_no_error(build_project(pipeline_only = TRUE))
    expect_true(file.exists("_site/pipeline_targets.html"))
    expect_false(file.exists("_site/script/index.html"))
    expect_false(file.exists("_site/story/output/index.html"))
    expect_false(file.exists("_site/story/source/index.html"))
    expect_no_error(build_project())
    expect_true(file.exists("_site/index.html"))
    expect_true(file.exists("_site/pipeline_targets.html"))
    expect_true(file.exists("_site/script/index.html"))
    expect_true(file.exists("_site/story/output/index.html"))
    expect_true(file.exists("_site/story/source/index.html"))
})


test_that("build_project runs with init_project with workspace", {
    skip_if_not_installed("targets")
    skip_if_not_installed("quarto")
    skip_if_not_installed("withr")
    tmp <- tempfile("prj_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    file.create(file.path(tmp, ".workspace"))
    prj_name <- file.path(tmp, "test_project")
    dir.create(prj_name)
    oldwd <- setwd(prj_name)
    on.exit(setwd(oldwd), add = TRUE)
    
    init_project("TestProject")
    # No _targets.R or pipeline_targets.qmd present
    expect_no_error(build_project(pipeline_only = TRUE))
    expect_true(file.exists("../_site/test_project/pipeline_targets.html"))
    expect_false(file.exists("../_site/test_project/script/index.html"))
    expect_false(file.exists("../_site/test_project/story/output/index.html"))
    expect_false(file.exists("../_site/test_project/story/source/index.html"))
    expect_no_error(build_project())
    expect_true(file.exists("../_site/test_project/index.html"))
    expect_true(file.exists("../_site/test_project/pipeline_targets.html"))
    expect_true(file.exists("../_site/test_project/script/index.html"))
    expect_true(file.exists("../_site/test_project/story/output/index.html"))
    expect_true(file.exists("../_site/test_project/story/source/index.html"))
})
