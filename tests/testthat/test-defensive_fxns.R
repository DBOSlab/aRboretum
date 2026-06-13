# file: tests/testthat/test-defensive_fxns.R

testthat::test_that("first_upper capitalizes first letter and lowercases the rest", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_identical(aRboretum:::.first_upper("fabaceae"), "Fabaceae")
  testthat::expect_identical(aRboretum:::.first_upper("eUTERPE"), "Euterpe")
  testthat::expect_identical(aRboretum:::.first_upper("x"), "X")
})

testthat::test_that("arg_check_spp_list trims and capitalizes lowercase species names", {
  testthat::skip_if_not_installed("aRboretum")

  out <- aRboretum:::.arg_check_spp_list(c(
    "  euterpe edulis  ",
    "paubrasilia echinata",
    "Inga edulis"
  ))

  testthat::expect_identical(
    out,
    c("Euterpe edulis", "Paubrasilia echinata", "Inga edulis")
  )
})

testthat::test_that("arg_check_spp_list errors when entries are not binomial names", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_error(
    aRboretum:::.arg_check_spp_list(c("Euterpe", "Paubrasilia echinata")),
    "Please make sure you used only species names as input!"
  )

  testthat::expect_error(
    aRboretum:::.arg_check_spp_list(c("Euterpe", "Inga")),
    "You may need to correct the folowing names"
  )
})

testthat::test_that("arg_check_dir validates type length and empty strings", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_error(
    aRboretum:::.arg_check_dir(1),
    "'dir' must be a single character string.",
    fixed = TRUE
  )

  testthat::expect_error(
    aRboretum:::.arg_check_dir(c("a", "b")),
    "'dir' must be a single character string.",
    fixed = TRUE
  )

  testthat::expect_error(
    aRboretum:::.arg_check_dir("   "),
    "'dir' cannot be an empty string.",
    fixed = TRUE
  )
})

testthat::test_that("arg_check_dir trims whitespace and removes only trailing slash", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_identical(
    aRboretum:::.arg_check_dir("  output_dir/  "),
    "output_dir"
  )

  testthat::expect_identical(
    aRboretum:::.arg_check_dir("nested/path"),
    "nested/path"
  )
})

testthat::test_that("arg_check_printed_lang validates presence and allowed values", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_error(
    aRboretum:::.arg_check_printed_lang(NULL),
    "'printed_lang' must contain at least one language code.",
    fixed = TRUE
  )

  testthat::expect_error(
    aRboretum:::.arg_check_printed_lang(character(0)),
    "'printed_lang' must contain at least one language code.",
    fixed = TRUE
  )

  testthat::expect_error(
    aRboretum:::.arg_check_printed_lang(c("pt", "de")),
    "Invalid language code(s): de. Allowed values: pt, en, fr, es",
    fixed = TRUE
  )

  testthat::expect_error(
    aRboretum:::.arg_check_printed_lang(c("PANARA")),
    "Invalid language code(s): panara. Allowed values: pt, en, fr, es",
    fixed = TRUE
  )
})

testthat::test_that("arg_check_printed_lang normalizes case whitespace and duplicates", {
  testthat::skip_if_not_installed("aRboretum")

  out <- aRboretum:::.arg_check_printed_lang(c(" PT ", "en", "Fr", "pt", "ES"))

  testthat::expect_identical(out, c("pt", "en", "fr", "es"))
})
