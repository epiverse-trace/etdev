#' Ensure that comparisons to `NA` or `NULL` are not made as strings
#'
#' In an attempt to "fix" the fact that comparison to `NA` and `NULL` don't
#' return `TRUE`/`FALSE`, users sometimes convert them as character. This is
#' incorrect and will produce unexpected results.
#'
#' @details
#' This linter has been proposed for integration in the lintr package (issue
#' #2130) but was rejected due to the high possibility of false positives, in
#' the case when users actually want to compare to `"NA"` or `"NULL"` as
#' strings. This can happen when working with database or more generally when
#' the character values have not been parsed as `NA` or `NULL` by R.
#'
#' @export
#'
#' @examples
#' lintr::lint(
#'   text = "x == 'NA'",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = "x != 'NA'",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = "x %in% 'NA'",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = "'NA' %in% x",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = "x == 'NULL'",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = "x != 'NULL'",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = 'x != "NULL"',
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' lintr::lint(
#'   text = "x %in% 'NULL'",
#'   linters = etdev::na_null_strings_linter()
#' )
#'
#' # Pass
#' lintr::lint(
#'   text = 'x == "a"',
#'   linters = etdev::na_null_strings_linter()
#' )
na_null_strings_linter <- function() {

  # Code mostly adapted from lintr::equals_na_linter() (MIT license)
  na_null_strings <- lintr:::xp_text_in_table(c(
    glue::double_quote(c("NA", "NULL")),
    glue::single_quote(c("NA", "NULL"))
  ))

  xpath = glue::glue("
    //STR_CONST[{ na_null_strings }]
      /parent::expr
      /parent::expr[EQ or NE]
    |
    //SPECIAL[
      text() = '%in%' and
      following-sibling::expr/STR_CONST[{ na_null_strings }]
    ]
      /parent::expr
  ")

  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    lintr::xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = paste(
        "Are you sure you want to compare to *the string* 'NA' or 'NULL'?",
        "To compare to the special values NA or NULL,",
        "please use `is.null()` or `is.na()` instead."
      ),
      type = "warning"
    )
  })
}
