context("filesystem-with-concurrency")

test_that("running 20 tasks in parllel, each calling exactly the same function", {
  skip() # testing framework doesnt support parallel processing
  skip_without_parallel()

  require(parallel, quietly = TRUE)

  path =  tempfile()

  fs <- cache_filesystem(path)
  fn <- function(i) { Sys.sleep(abs(rnorm(1, 0, sd = 0.2))); return(i**2) }
  fnm <- memoise(fn, cache = fs)
  n = as.integer(max(2, floor(parallel::detectCores()/2)))
  cl = makeCluster(n)
  clusterExport(cl, "fnm", envir = environment())
  on.exit({
    stopCluster(cl)
    forget(fnm)
  })

  work = parLapply(cl, 1:(n*15), function(i) fnm(12))
  expect_true(all(work == 144))

})
