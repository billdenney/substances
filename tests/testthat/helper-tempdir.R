## tempfile() already returns a unique path under tempdir(); rolling our own
## with sample.int() would draw from the RNG stream and shift the seed for
## whatever test ran next.
withr_tempdir <- function() {
  dir <- tempfile("substances-")
  dir.create(dir, recursive = TRUE)
  dir
}
