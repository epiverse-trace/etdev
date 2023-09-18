test_that("na_null_strings_linter() skips allowed usages", {

  linter <- na_null_strings_linter()

  lintr::expect_lint("x == 'a'", NULL, linter)
  lintr::expect_lint("x == 'NANA'", NULL, linter)
  lintr::expect_lint("'a' == 'b'", NULL, linter)

})

test_that("na_null_strings_linter() blocks simple disallowed usages", {

  linter <- na_null_strings_linter()
  lint_message <- rex::rex(
    "Are you sure you want to compare to *the string* 'NA' or 'NULL'?"
  )

  lintr::expect_lint("x == 'NA'", lint_message, linter)
  lintr::expect_lint('x == "NA"', lint_message, linter)

  lintr::expect_lint("x == 'NULL'", lint_message, linter)
  lintr::expect_lint('x == "NULL"', lint_message, linter)

  lintr::expect_lint("x %in% 'NA'", lint_message, linter)
  lintr::expect_lint("x != 'NA'", lint_message, linter)

  lintr::expect_lint("x %in% 'NULL'", lint_message, linter)
  lintr::expect_lint("x != 'NULL'", lint_message, linter)

  lintr::expect_lint('"NULL" == x', lint_message, linter)

  lintr::expect_lint('x == r"{NULL}"', lint_message, linter)
})

test_that("na_null_strings_linters() false positives", {

  linter <- na_null_strings_linter()

  # I cannot think of any case where users migth want to compare two constants
  lintr::expect_lint('"a" == "NA"', NULL, linter)

})
