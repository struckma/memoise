skip_without_aws_credetials <- function() {
  # -# Sys.setenv("AWS_ACCESS_KEY_ID" = "<access key>", "AWS_SECRET_ACCESS_KEY" = "<access secret>")
  if (nzchar(Sys.getenv("AWS_ACCESS_KEY_ID")) && nzchar(Sys.getenv("AWS_SECRET_ACCESS_KEY"))) {
    return(invisible(TRUE))
  }

  testthat::skip("No AWS Credentials")
}

skip_on_travis_pr <- function() {
  if (identical(Sys.getenv("TRAVIS"), "true") && !identical(Sys.getenv("TRAVIS_PULL_REQUEST", "false"), "false")) {
    return(testthat::skip("On Travis PR"))
  }

  invisible(TRUE)
}

skip_without_parallel <- function() {
  if ("parallel" %in% rownames(installed.packages())) {
    require(parallel, quietly = TRUE)
    if (parallel::detectCores() > 1)
      return(invisible(TRUE))
  }
  return(testthat::skip("parallel library missing or only one core available."))
}
