# Test for read_asset_file
test_that("read_asset_file reads CSS and JS correctly", {
    css <- read_asset_file("mermaid-zoom.css")
    js  <- read_asset_file("mermaid-zoom.js")
    expect_true(grepl("zoom-container", css))
    expect_true(grepl("enableZoomPan", js))
    expect_true(nchar(css) > 0)
    expect_true(nchar(js) > 0)
})

# Test for knit_targets_mermaid

test_that("knit_targets_mermaid returns knitr asis mermaid output", {
    skip_if_not_installed("targets")
    skip_if_not_installed("knitr")
    # Use a temporary directory for _targets.R pipeline
    tmp <- tempfile("targets_test_")
    dir.create(tmp)
    writeLines(c(
        "library(targets)",
        "list(",
        "  tar_target(x, 1 + 1),",
        "  tar_target(y, x * 2)",
        ")"
    ), con = file.path(tmp, "_targets.R"))
    oldwd <- setwd(tmp)
    on.exit(setwd(oldwd), add = TRUE)
    Sys.sleep(0.5)
    mermaid <- knit_targets_mermaid()
    expect_type(mermaid, "character")
    expect_true(grepl("mermaid", mermaid))
    expect_true(grepl("direction TB", as.character(unlist(mermaid))))
    expect_true(grepl("knit_asis", class(mermaid)))
        expect_true(grepl("<style>.*zoom-container.*</style>", mermaid))
        expect_true(grepl("<script>.*enableZoomPan.*</script>", mermaid))
})
