test_that("copy_markdown copies markdown files and skips excluded directories", {
    skip_if_not_installed("fs")

    src_dir <- file.path(tempdir(), "copy_markdown_src")
    dst_dir <- file.path(tempdir(), "copy_markdown_dst")

    if (dir.exists(src_dir)) fs::dir_delete(src_dir)
    if (dir.exists(dst_dir)) fs::dir_delete(dst_dir)

    dir.create(file.path(src_dir, "docs"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(src_dir, ".git", "ignored"), recursive = TRUE, showWarnings = FALSE)

    writeLines("home page", file.path(src_dir, "README.md"))
    writeLines("guide", file.path(src_dir, "docs", "guide.md"))
    writeLines("ignored", file.path(src_dir, ".git", "ignored.md"))

    copied <- copy_markdown(
        src = src_dir,
        dst = dst_dir,
        overwrite = TRUE,
        exclude = c(".git")
    )

    expect_true(all(c("README.md", "docs/guide.md") %in% copied))
    expect_true(file.exists(file.path(dst_dir, "README.md")))
    expect_true(file.exists(file.path(dst_dir, "docs", "guide.md")))
    expect_false(file.exists(file.path(dst_dir, ".git", "ignored.md")))

    fs::dir_delete(src_dir)
    fs::dir_delete(dst_dir)
})

test_that("copy_markdown clears non-empty destinations when overwrite is TRUE", {
    skip_if_not_installed("fs")

    src_dir <- file.path(tempdir(), "copy_markdown_src_clear")
    dst_dir <- file.path(tempdir(), "copy_markdown_dst_clear")

    if (dir.exists(src_dir)) fs::dir_delete(src_dir)
    if (dir.exists(dst_dir)) fs::dir_delete(dst_dir)

    dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)

    writeLines("source", file.path(src_dir, "README.md"))
    writeLines("stale", file.path(dst_dir, "stale.md"))

    copied <- copy_markdown(src = src_dir, dst = dst_dir, overwrite = TRUE)

    expect_true("README.md" %in% copied)
    expect_true(file.exists(file.path(dst_dir, "README.md")))
    expect_false(file.exists(file.path(dst_dir, "stale.md")))

    fs::dir_delete(src_dir)
    fs::dir_delete(dst_dir)
})

test_that("copy_markdown errors for non-empty destinations when overwrite is FALSE", {
    skip_if_not_installed("fs")

    src_dir <- file.path(tempdir(), "copy_markdown_src_error")
    dst_dir <- file.path(tempdir(), "copy_markdown_dst_error")

    if (dir.exists(src_dir)) fs::dir_delete(src_dir)
    if (dir.exists(dst_dir)) fs::dir_delete(dst_dir)

    dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)

    writeLines("source", file.path(src_dir, "README.md"))
    writeLines("stale", file.path(dst_dir, "stale.md"))

    expect_error(
        copy_markdown(src = src_dir, dst = dst_dir, overwrite = FALSE),
        "not empty"
    )

    fs::dir_delete(src_dir)
    fs::dir_delete(dst_dir)
})

test_that("copy_markdown rewrites .html.md targets in nested directories as .md", {
    skip_if_not_installed("fs")

    src_dir <- file.path(tempdir(), "copy_markdown_src_nested_html")
    dst_dir <- file.path(tempdir(), "copy_markdown_dst_nested_html")

    if (dir.exists(src_dir)) fs::dir_delete(src_dir)
    if (dir.exists(dst_dir)) fs::dir_delete(dst_dir)

    dir.create(file.path(src_dir, "docs"), recursive = TRUE, showWarnings = FALSE)

    writeLines("nested html markdown", file.path(src_dir, "docs", "guide.html.md"))

    copied <- copy_markdown(src = src_dir, dst = dst_dir, overwrite = TRUE)

    expect_true("docs/guide.html.md" %in% copied)
    expect_true(file.exists(file.path(dst_dir, "docs", "guide.md")))
    expect_false(file.exists(file.path(dst_dir, "docs", "guide.html.md")))

    fs::dir_delete(src_dir)
    fs::dir_delete(dst_dir)
})

test_that("copy_markdown rewrites .html.md targets as .md", {
    skip_if_not_installed("fs")

    src_dir <- file.path(tempdir(), "copy_markdown_src_html")
    dst_dir <- file.path(tempdir(), "copy_markdown_dst_html")

    if (dir.exists(src_dir)) fs::dir_delete(src_dir)
    if (dir.exists(dst_dir)) fs::dir_delete(dst_dir)

    dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)

    writeLines("html markdown", file.path(src_dir, "guide.html.md"))

    copied <- copy_markdown(src = src_dir, dst = dst_dir, overwrite = TRUE)

    expect_true("guide.html.md" %in% copied)
    expect_true(file.exists(file.path(dst_dir, "guide.md")))
    expect_false(file.exists(file.path(dst_dir, "guide.html.md")))

    fs::dir_delete(src_dir)
    fs::dir_delete(dst_dir)
})

test_that("copy_markdown keeps source .html.md files when keep_md is TRUE", {
    skip_if_not_installed("fs")

    src_dir <- file.path(tempdir(), "copy_markdown_src_keep_md")
    dst_dir <- file.path(tempdir(), "copy_markdown_dst_keep_md")

    if (dir.exists(src_dir)) fs::dir_delete(src_dir)
    if (dir.exists(dst_dir)) fs::dir_delete(dst_dir)

    dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)

    src_file <- file.path(src_dir, "guide.html.md")
    writeLines("html markdown", src_file)

    copied <- copy_markdown(
        src = src_dir,
        dst = dst_dir,
        overwrite = TRUE,
        keep_md = TRUE
    )

    expect_true("guide.html.md" %in% copied)
    expect_true(file.exists(file.path(dst_dir, "guide.md")))
    expect_true(file.exists(src_file))

    fs::dir_delete(src_dir)
    fs::dir_delete(dst_dir)
})
