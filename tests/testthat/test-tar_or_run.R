test_that("tar_or_run returns fallback when target read fails", {
    skip_if_not_installed("targets")

    fallback_value <- data.frame(x = 1:5)
    result <- tar_or_run(nonexistent_target, fallback_value, quiet = TRUE)

    expect_identical(result, fallback_value)
})

test_that("tar_or_run validates target argument", {
    expect_error(
        tar_or_run("string_target", 1 + 1),
        "must be an unquoted name"
    )
})

test_that("tar_or_run requires target argument", {
    expect_error(
        tar_or_run(, 1 + 1),
        "must be provided"
    )
})

test_that("tar_or_run evaluates fallback expression when target fails", {
    skip_if_not_installed("targets")

    fallback_result <- 42
    result <- tar_or_run(nonexistent_target, fallback_result, quiet = TRUE)

    expect_equal(result, fallback_result)
})

test_that("tar_or_run messages when target read fails", {
    skip_if_not_installed("targets")

    expect_message(
        tar_or_run(nonexistent_target, 1, quiet = FALSE),
        "Target read failed"
    )
    expect_message(
        tar_or_run(nonexistent_target, 1, quiet = FALSE),
        "Running fallback expression"
    )
})

test_that("tar_or_run suppresses messages with quiet = TRUE", {
    skip_if_not_installed("targets")

    expect_no_message(
        tar_or_run(nonexistent_target, 1, quiet = TRUE)
    )
})
