#' Extract declared minimum R version from DESCRIPTION
#'
#' @param path Path to the package root
#'
#' @returns An R version number or `character(0)` if no minimum R version has
#' been declared
#'
#' @export
extract_min_r_version <- function(path = ".") {

  min_r_ver <- file.path(path, "DESCRIPTION") |>
    read.dcf("Depends") |>
    strsplit(",", fixed = TRUE) |>
    unlist() |>
    trimws() |>
    (\(x) grep("R ", x, value = TRUE, fixed = TRUE))() |>
    (\(x) gsub("^R \\(>=?\\s(.+)\\)", "\\1", x))() |>
    unname()

  # According to 'Writing R Extensions', the trailing 0 for patch version can
  # be dropped.
  # But we want to identically match an existing version number so we add it if
  # it's missing.
  gsub("^(\\d+\\.\\d+)$", "\\1.0", min_r_ver)

}
