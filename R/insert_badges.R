#' Insert badges in README
#'
#' @param path Path to the package root
#'
#' @export
#'
#' @rdname insert-badges
insert_badge_rcmdcheck <- function(path = ".") {

  pkgname <- file.path(path, "DESCRIPTION") |>
    read.dcf("Package")

  paste0(
    "[",
    "![R-CMD-check]",
    "(https://github.com/epiverse-trace/",
    pkgname,
    "/actions/workflows/R-CMD-check.yaml/badge.svg)",
    "]",
    "(https://github.com/epiverse-trace/",
    pkgname,
    "/actions/workflows/R-CMD-check.yaml)"
  )

}

#' @export
#'
#' @rdname insert-badges
insert_badge_codecov <- function(path = ".") {

  pkgname <- file.path(path, "DESCRIPTION") |>
    read.dcf("Package")

  paste0(
    "[",
    "![Codecov test coverage]",
    "(https://codecov.io/gh/epiverse-trace/",
    pkgname,
    "/branch/main/graph/badge.svg)",
    "]",
    "(https://app.codecov.io/gh/epiverse-trace/",
    pkgname,
    "?branch=main)"
  )

}
