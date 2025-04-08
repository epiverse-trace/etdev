#' Render README.Rmd to README.md on your local developer machine.
#'
#' Adapted from the `render_readme.yml` Github action.
#'
#' Improves developer feedback and testing.
#'
#' @export
render_readme <- function() {
  writeLines(
    knitr::knit_expand(
      "README.Rmd",
      packagename = read.dcf("DESCRIPTION", "Package"),
      gh_repo = ifelse(
        Sys.getenv("GITHUB_REPOSITORY") == "",
        gsub( # this makes it run locally
          "https://github.com/", "",
          read.dcf(
            "DESCRIPTION", "URL"
          )[1],
          fixed = TRUE
        ),
        Sys.getenv("GITHUB_REPOSITORY")
      )
    ),
    "README_expanded.Rmd"
  )
  rmarkdown::render(
    "README_expanded.Rmd",
    output_file = "README.md",
    output_dir = "."
  )
  unlink(c("README.html", "README_expanded.Rmd"))
}
