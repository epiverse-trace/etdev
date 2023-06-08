#' Extract declared minimum R version from DESCRIPTION
#'
#' @param path Path to the package root
#'
#' @returns An R version number or `character(0)` if no minimum R version has
#' been declared
#'
#' @export
extract_min_r_version <- function(path = ".") {

  file.path(path, "DESCRIPTION") |>
    read.dcf("Depends") |>
    strsplit(",") |>
    unlist() |>
    trimws() |>
    (\(x) grep("R ", x, value = TRUE))() |>
    (\(x) gsub("^R \\(>=?\\s(.+)\\)", "\\1", x))() |>
    unname()

}
