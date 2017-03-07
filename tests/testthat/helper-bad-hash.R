#' Filesystem Cache with dysfunctional hash function to trigger hash collisions for testing
#'
#' Use a cache on the local filesystem that will persist between R sessions. DO NOT USE IN PRODUCTIVE ENVIRONMENT, FOR TESTING ONLY.
#'
#' @param path Directory in which to store cached items.
#'
#' @examples
#'
#' \dontrun{
#' # Use with Dropbox
#'
#' db <- cache_filesystem_collision("~/Dropbox/.rcache")
#'
#' mem_runif <- memoise(runif, cache = db)
#'
#' # Use with Google Drive
#'
#' gd <- cache_filesystem_collision("~/Google Drive/.rcache")
#'
#' mem_runif <- memoise(runif, cache = gd)
#'
#' }
#'
#' @export
#' @inheritParams cache_memory
cache_filesystem_collision <- function(path) {

  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE)
  }

  cache_reset <- function() {
    cache_files <- list.files(path, full.names = TRUE)
    file.remove(cache_files)
  }

  cache_set <- function(key, value) {
    saveRDS(value, file = file.path(path, key))
  }

  cache_get <- function(key) {
    readRDS(file = file.path(path, key))
  }

  cache_has_key <- function(key) {
    file.exists(file.path(path, key))
  }

  list(
    digest = function(...) return("CONSTANT"),
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = function() list.files(path)
  )
}
