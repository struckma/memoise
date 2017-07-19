#' Filesystem Cache
#'
#' Use a cache on the local filesystem that will persist between R sessions.
#'
#' @param path Directory in which to store cached items.
#'
#' @examples
#'
#' \dontrun{
#' # Use with Dropbox
#'
#' db <- cache_filesystem("~/Dropbox/.rcache")
#'
#' mem_runif <- memoise(runif, cache = db)
#'
#' # Use with Google Drive
#'
#' gd <- cache_filesystem("~/Google Drive/.rcache")
#'
#' mem_runif <- memoise(runif, cache = gd)
#'
#' }
#'
#' @export
#' @inheritParams cache_memory
cache_filesystem <- function(path, algo = "xxhash64") {

  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  cache_reset <- function() {
    cache_files <- list.files(path, full.names = TRUE)
    file.remove(cache_files)
  }

  cache_set <- function(key, value) {
    if (!dir.exists(path)) {
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }
    file = file.path(path, key)
    tfile <- tempfile(basename(file), dirname(file))                                            # http://r.789695.n4.nabble.com/Risk-of-readRDS-not-detecting-race-conditions-with-parallel-saveRDS-td4643235.html
    on.exit(if (file.exists(tfile)) unlink(tfile))                                              #
    retval <- saveRDS(value, tfile)                                                             #
    if (file.exists(file))                                                                      #
      unlink(file)                                                                              #
    if (!file.rename(tfile, file)) {                                                            #
      stop("Cannot rename temporary file ", tfile, " to ", file)                                #
    }                                                                                           #
    invisible(value)
  }

  cache_get <- function(key) {
    readRDS(file = file.path(path, key))
  }

  cache_has_key <- function(key) {
    file.exists(file.path(path, key))
  }

  list(
    digest = function(...) digest::digest(..., algo = algo),
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = function() list.files(path)
  )
}
