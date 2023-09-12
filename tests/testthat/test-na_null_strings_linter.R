test_that("na_null_strings_linter() skips allowed usages", {

  linter <- na_null_strings_linter()

  lintr::expect_lint("x == 'a'", NULL, linter)
  lintr::expect_lint("x == 'NANA'", NULL, linter)

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

})
