## Minimal stand-in so the tests need no extra dependency.
withr_tempdir <- function() {
  dir <- file.path(tempdir(), paste0("substances-", sample.int(1e6, 1)))
  dir.create(dir, recursive = TRUE)
  dir
}
