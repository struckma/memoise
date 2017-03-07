context("filesystem-with-collision-hash")

test_that("using a filesystem cache with a very bad hash-function works despite hash-collision", {

  path =  tempfile()

  fs <- cache_filesystem_collision(path)
  i <- 0
  fn <- function() { i <<- i + 1; i }
  fnm <- memoise(fn, cache = fs)
  on.exit(forget(fnm))

  j <- 0
  fn2 <- function() { j <<- j + 1; j }
  fn2m <- memoise(fn2, cache = fs)
  on.exit(forget(fn2m))

  expect_equal(fn(), 1)
  expect_equal(fn(), 2)
  expect_equal(fnm(), 3)
  expect_equal(fnm(), 3)
  expect_equal(fn(), 4)
  expect_equal(fnm(), 3)

  expect_equal(fn2(), 1)
  expect_equal(fn2m(), 2)
  expect_equal(fn2m(), 2)
  expect_equal(fn2(), 3)
  expect_equal(fn2m(), 2)

  expect_false(forget(fn))
  expect_true(forget(fnm))
  expect_equal(fnm(), 5)

  expect_false(forget(fn2))
  expect_true(forget(fn2m))
  expect_equal(fn2m(), 4)

  expect_true(is.memoised(fnm))
  expect_false(is.memoised(fn))

  expect_true(is.memoised(fn2m))
  expect_false(is.memoised(fn2)) # not necessary, since fnm and fn2m share the same cache, and this cache has been deleted on forget(fnm) above.
})
