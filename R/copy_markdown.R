#' Copy Markdown Files to a Destination Directory
#'
#' Recursively copies markdown files from a source directory to a destination
#' directory while skipping excluded directories such as build outputs or git
#' metadata. Existing files are only overwritten when requested.
#'
#' @param src Character scalar. Source directory to scan for markdown files.
#'   Defaults to the current working directory.
#' @param dst Character scalar. Destination directory where markdown files will
#'   be copied.
#' @param overwrite Logical scalar. Whether to overwrite existing files in the
#'   destination when the source file is newer. Defaults to TRUE.
#' @param exclude Character vector. Directory names to skip when scanning for
#'   markdown files. Defaults to common build and editor directories.
#' @return Invisible character vector of relative paths copied to the
#'   destination directory.
#' @export
#' @examples
#' \dontrun{
#' copy_markdown(src = "./docs", dst = "./site-docs")
#' }
copy_markdown <- function(
    src = ".",
    dst,
    overwrite = TRUE,
    exclude = c("_site", ".quarto", ".git", ".targets", "_freeze", ".Rproj.user", ".vscode")
) {

    stopifnot(dir.exists(src))
    fs::dir_create(dst)

    md_files <- fs::dir_ls(
        src,
        recurse = TRUE,
        type = "file",
        regexp = "\\.md$"
    )

    # remove excluded folders
    if (length(exclude) > 0) {
        pattern <- paste0("(^|/)", exclude, "(/|$)", collapse = "|")
        md_files <- md_files[!grepl(pattern, md_files)]
    }

    copied <- vector("character")

    for (f in md_files) {

        rel <- fs::path_rel(f, start = src)
        target <- fs::path(dst, rel)

        fs::dir_create(fs::path_dir(target))

        do_copy <- TRUE

        if (file.exists(target)) {

        src_time <- file.info(f)$mtime
        dst_time <- file.info(target)$mtime

        do_copy <- overwrite &&
            isTRUE(src_time > dst_time)
        }

        if (do_copy) {
        fs::file_copy(f, target, overwrite = TRUE)
        copied <- c(copied, rel)
        }
    }

    message(length(copied), " markdown files deployed to:")
    message(normalizePath(dst))

    invisible(copied)
}

