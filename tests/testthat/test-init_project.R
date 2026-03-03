test_that("init creates all directories", {
    # Create a temporary directory for testing
    temp_dir <- tempfile("init_test")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = FALSE)

    # Run init
    init_project("TestProject", root = temp_dir)

    # Check that all directories exist
    expected_dirs <- c(
        "script", "source", "derived", "output", "story",
        file.path("story", "source"),
        file.path("story", "output")
    )

    for (dir in expected_dirs) {
        expect_true(dir.exists(file.path(temp_dir, dir)),
            info = paste("Directory", dir, "should exist")
        )
    }
})

test_that("init creates root index.qmd with correct content", {
    temp_dir <- tempfile("init_test")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = FALSE)

    project_name <- "TestProject"
    init_project(project_name, root = temp_dir)

    # Check root index.qmd exists
    root_index <- file.path(temp_dir, "index.qmd")
    expect_true(file.exists(root_index))

    # Check content includes project name
    content <- readLines(root_index)
    expect_true(any(grepl(project_name, content)))
    expect_true(any(grepl("Sources", content)))
    expect_true(any(grepl("Scripts", content)))
    expect_true(any(grepl("Outputs", content)))
})

test_that("init creates index.qmd in specified folders", {
    temp_dir <- tempfile("init_test")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = FALSE)

    init_project("TestProject", root = temp_dir)

    # Check index.qmd files exist in the three main folders
    expected_indexes <- c(
        file.path(temp_dir, "script", "index.qmd"),
        file.path(temp_dir, "story", "source", "index.qmd"),
        file.path(temp_dir, "story", "output", "index.qmd")
    )

    for (index_file in expected_indexes) {
        expect_true(file.exists(index_file),
            info = paste("Index file", index_file, "should exist")
        )
    }
})

test_that("init_project doesn't overwrite existing files", {
    temp_dir <- tempfile("init_test")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = FALSE)

    # Create an existing index.qmd with custom content
    root_index <- file.path(temp_dir, "index.qmd")
    custom_content <- "Custom content"
    writeLines(custom_content, root_index)

    # Run init_project and expect a message about existing file
    expect_message(
        init_project("TestProject", root = temp_dir),
        "already exists"
    )

    # Check that original content is preserved
    content <- readLines(root_index)
    expect_equal(content, custom_content)
})

test_that("init_project requires existing root directory", {
    # Test that init_project() fails when given a non-existent directory
    nonexistent_dir <- tempfile("nonexistent")
    
    expect_error(
        init_project("TestProject", root = nonexistent_dir),
        "dir.exists\\(root\\) is not TRUE"
    )
    
    # Ensure no directories were created
    expect_false(dir.exists(nonexistent_dir))
})

test_that("init_project index.qmd files have correct titles", {
    temp_dir <- tempfile("init_test")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = FALSE)

    init_project("TestProject", root = temp_dir)

    # Check script index title
    script_index <- file.path(temp_dir, "script", "index.qmd")
    script_content <- readLines(script_index)
    expect_true(any(grepl("Scripts", script_content)))

    # Check story/source index title
    source_index <- file.path(temp_dir, "story", "source", "index.qmd")
    source_content <- readLines(source_index)
    expect_true(any(grepl("Sources", source_content)))

    # Check story/output index title
    output_index <- file.path(temp_dir, "story", "output", "index.qmd")
    output_content <- readLines(output_index)
    expect_true(any(grepl("Outputs", output_content)))
})



test_that("init_project creates template files", {
    tmp <- tempfile()
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))

    name <- "TestProject"
    init_project(name, root = tmp)

    # Check main template files
    expect_true(file.exists(file.path(tmp, ".project")))
    expect_true(file.exists(file.path(tmp, ".here")))
    expect_true(file.exists(file.path(tmp, "_targets.R")))
    expect_true(file.exists(file.path(tmp, "_quarto.yml")))
    expect_true(file.exists(file.path(tmp, "_build.R")))
    expect_true(file.exists(file.path(tmp, "_build.bat")))
    expect_true(file.exists(file.path(tmp, "pipeline_targets.qmd")))

    # Check index.qmd files
    expect_true(file.exists(file.path(tmp, "index.qmd")))
    expect_true(file.exists(file.path(tmp, "script", "index.qmd")))
    expect_true(file.exists(file.path(tmp, "story", "source", "index.qmd")))
    expect_true(file.exists(file.path(tmp, "story", "output", "index.qmd")))

    # Check folders
    expect_true(dir.exists(file.path(tmp, "script")))
    expect_true(dir.exists(file.path(tmp, "source")))
    expect_true(dir.exists(file.path(tmp, "derived")))
    expect_true(dir.exists(file.path(tmp, "output")))
    expect_true(dir.exists(file.path(tmp, "story")))
    expect_true(dir.exists(file.path(tmp, "story", "source")))
    expect_true(dir.exists(file.path(tmp, "story", "output")))
})
