spring_clean <- function() {
    vv <- function(x, crlf = FALSE) {
        eol <- ifelse(crlf, "\n\n", "\n")
        cat(paste0(x, eol))
    }
    files_with <- function(file_list, pattern, return_logicals = TRUE) {
        # index file_list with logical results of pattern search
        y <- unlist( # unlist lapply below
            lapply(file_list, function(x) { # for each file in file_list
                x <- paste(readLines(x), collapse = " ") # get file contents
                grepl(pattern, x)
            })
        )
        if (return_logicals == FALSE) {
            y <- file_list[y]
        }
        y
    }

    package <- read.dcf("DESCRIPTION", "Package")
    yml <- yaml::read_yaml("_pkgdown.yml")
    webpage_url <- paste0("https://epiverse.github.io/", package)
    api_url <- paste0("https://api.github.com/repos/epiverse-trace/", package)


    result <- yml$url == webpage_url
    result <- ifelse(result, "🟢", "❌")
    vv(paste(result, "Website URL is listed in _pkgdown.yml"))

    result <- yml$template$package == "epiversetheme"
    result <- ifelse(result, "🟢", "❌")
    vv(paste(result, "pkgdown site is using epiversetheme"))

    result <- file_test("-f", "man/figures/logo.svg")
    result <- ifelse(result, "🟢", "❌")
    vv(paste(result, "svg version of logo used"))
    vv(paste(result, "Logo properly detected by pkgdown & r-universe"))

    result <- !is.null(yml$reference)
    result <- ifelse(result, "🟢", "❌")
    vv(paste(result, "Add a pkgdown reference index"))

    # Check for files that plot graphics without alt text
    rmd_files <- list.files(pattern = "Rmd", recursive = TRUE)
    with_plot <- files_with(rmd_files, "plot\\(") # covers ggplot as well
    with_fig_alt <- files_with(rmd_files, "fig\\.alt")
    result <- rmd_files[xor(with_fig_alt, with_plot)]
    if (length(result) > 0) {
        vv("❌ No alt-text in the following files")
        vv(result)
        vv("[Example: ```{r, fig.alt = 'Alt text here'}]")
    } else {
        vv("🟢 All images include helpful alt-text")
    }


    vv("Updating lintr and devtools to the latest version")
    pak::pak(c("lintr", "devtools"), dependencies = TRUE)

    vv("Getting latest updates from packagetemplate")
    system("git clone https://github.com/epiverse-trace/packagetemplate")
    vv("Copying .lintr")
    system("mv -f packagetemplate/.lintr .")
    vv("Copying Github Actions")
    system("mv -f packagetemplate/.github/workflows/*.y* .github/workflows")
    vv("Copying tools/ folder")
    system("rm -rf tools && mv packagetemplate/tools .")
    vv("Copying tests/spelling.R")
    system("mv -f packagetemplate/tests/spelling.R tests")
    vv("Copying test/testthat/*.R")
    system("mv -f packagetemplate/tests/testthat/* tests/testthat/")
    system("rm -rf packagetemplate")
    vv("Rename .r to .R ")
    system(
        'for file in $(find . -name "*.r"); do  mv "$file" "${file%.r}.R"; done'
    )

    # Get the year the repo was created to update LICENSE
    year <- api_url |>
        readLines() |>
        jsonlite::fromJSON() |>
        (\(x) x$created_at)() |>
        substr(1, 4)
    # Update LICENSE
    readLines("LICENSE") |>
        paste(collapse = "\n") |>
        (\(x) gsub("YEAR:\\s*\\d{4}", paste("YEAR:", year), x))() |>
        writeLines(con = "LICENSE")
    vv(paste("🟢 License file year updated to", year))

    # Checking for .R files with #nolint comment
    nolint_files <- list.files(pattern = "R$", recursive = TRUE) |>
        files_with("nolint", return_logicals = FALSE)
    if (nolint_files |> length() > 0) {
        vv("❌ # nolint found in the following files")
        vv(nolint_files)
        vv("")
        vv("New versions of lintr may have squashed previous bugs")
        vv("Consider reviewing...")
    }


    # vv("Spell checking package")
    # spelling::spell_check_package()

    # vv("Checking GitHub links")
    # usethis::use_github_links()

    # vv("Standardize DESCRIPTION")
    # desc::desc_normalize()

    # vv("Checking URLs")
    # urlchecker::url_check()

    vv("[ ] Verify intro vignette appears under 'Get started'")
    vv("[ ] Verify vignettes are ordered correctly (if applicable)")

    vv("[ ] Use roxygen2 markdown syntax (`usethis::use_roxygen_md()`)")
    vv("[ ] Add a related project section at the end of the `README`")
    vv("[ ] Add package-level documentation page: `usethis::use_package_doc()`")
    vv("[ ] Move package-level imports to the package-level documentation")
    vv("[ ] Verify that all `@import` and `@importFrom` import functions that are actually used in the relevant function")
    vv("[ ] Identify potential for [reuse in documentation](https://roxygen2.r-lib.org/articles/reuse.html)")
    vv("[ ] Run `devtools::document()`")
}
