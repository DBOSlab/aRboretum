# Phrase generation, run on a data file reviewed after arboretum_data()

testthat::test_that("arboretum_phrases adds the phrase columns to the data file and writes the guide", {
  testthat::skip_if_not_installed("aRboretum")

  .with_tmp_wd("phrases-first-run", {
    dir.create("arboretum_data")
    csv <- .write_fixture_csv(file.path("arboretum_data", "arboretum_data.csv"))

    msgs <- testthat::capture_messages(
      out <- aRboretum::arboretum_phrases(csv, printed_lang = c("en", "pt"), verbose = TRUE)
    )

    # Every phrase column is created, but only printed_lang is filled
    testthat::expect_true(all(aRboretum:::.phrase_cols() %in% names(out)))
    testthat::expect_true(all(nzchar(out$full_phrases_EN)))
    testthat::expect_true(all(nzchar(out$full_phrases_PT)))
    testthat::expect_true(all(is.na(out$full_phrases_FR)))
    testthat::expect_true(all(is.na(out$full_phrases_ADD_LANGUAGE)))
    testthat::expect_match(out$full_phrases_EN[1], "Paubrasilia echinata")
    testthat::expect_match(out$full_phrases_EN[1], "endangered \\(EN\\)")
    testthat::expect_true(any(grepl("Generated 2 phrase\\(s\\) for language: EN", msgs)))

    # The data file is updated in place
    saved <- utils::read.csv(csv, stringsAsFactors = FALSE, encoding = "UTF-8")
    testthat::expect_identical(saved$full_phrases_EN, out$full_phrases_EN)
    testthat::expect_identical(names(saved), names(out))

    # The editable guide sits next to the data file
    guide <- file.path("arboretum_data", "__phrase_generating_guide.html")
    testthat::expect_true(file.exists(guide))
    html <- paste(readLines(guide, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_match(html, 'data-col="full_phrases_EN"', fixed = TRUE)
    testthat::expect_match(html, 'data-col="full_phrases_PT"', fixed = TRUE)
    testthat::expect_match(html, "arboretum_data.csv", fixed = TRUE)
  })
})

testthat::test_that("arboretum_phrases keeps edited phrases, fills cleared ones and can overwrite", {
  testthat::skip_if_not_installed("aRboretum")

  .with_tmp_wd("phrases-rerun", {
    csv <- .write_fixture_csv("data.csv")
    first <- aRboretum::arboretum_phrases(csv, printed_lang = c("en", "pt"), verbose = FALSE)

    # The user edits one phrase and clears another directly in the data file
    edited <- utils::read.csv(csv, stringsAsFactors = FALSE, encoding = "UTF-8")
    edited$full_phrases_EN[1] <- "My own phrase about pau-brasil."
    edited$full_phrases_PT[2] <- NA
    .write_fixture_csv(csv, edited)

    second <- aRboretum::arboretum_phrases(csv, printed_lang = c("en", "pt"), verbose = FALSE)
    testthat::expect_identical(second$full_phrases_EN[1], "My own phrase about pau-brasil.")
    testthat::expect_identical(second$full_phrases_EN[2], first$full_phrases_EN[2])
    testthat::expect_true(nzchar(second$full_phrases_PT[2]))

    saved <- utils::read.csv(csv, stringsAsFactors = FALSE, encoding = "UTF-8")
    testthat::expect_identical(saved$full_phrases_PT, second$full_phrases_PT)

    html <- paste(readLines("__phrase_generating_guide.html", warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    testthat::expect_match(html, "My own phrase about pau-brasil.", fixed = TRUE)

    # Nothing to generate: the data file is not rewritten
    before <- file.info(csv)$mtime
    Sys.sleep(1.1)
    aRboretum::arboretum_phrases(csv, printed_lang = c("en", "pt"), verbose = FALSE)
    testthat::expect_identical(file.info(csv)$mtime, before)

    # overwrite = TRUE regenerates the edited phrase from the data
    forced <- aRboretum::arboretum_phrases(csv, printed_lang = "en", overwrite = TRUE,
                                           verbose = FALSE)
    # (generated phrases sample vernacular names and countries at random)
    testthat::expect_false(identical(forced$full_phrases_EN[1], "My own phrase about pau-brasil."))
    testthat::expect_match(forced$full_phrases_EN[1], "^<i>Paubrasilia echinata</i> belongs to")
  })
})

testthat::test_that("arboretum_phrases offers the custom language field and keeps its text", {
  testthat::skip_if_not_installed("aRboretum")
  testthat::skip_if_not_installed("openxlsx")

  .with_tmp_wd("phrases-add-lang", {
    df <- .fixture_species_df()
    openxlsx::write.xlsx(df, "data.xlsx", rowNames = FALSE)

    aRboretum::arboretum_phrases("data.xlsx", printed_lang = "pt", add_lang = "PANARA",
                                 verbose = FALSE)
    html <- paste(readLines("__phrase_generating_guide.html", warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    testthat::expect_match(html, 'data-col="full_phrases_ADD_LANGUAGE"', fixed = TRUE)

    # Text entered for the custom language is kept, even with overwrite = TRUE
    edited <- openxlsx::read.xlsx("data.xlsx")
    edited$full_phrases_ADD_LANGUAGE[1] <- "Texto em panará"
    openxlsx::write.xlsx(edited, "data.xlsx", rowNames = FALSE)

    out <- aRboretum::arboretum_phrases("data.xlsx", printed_lang = "pt", add_lang = "PANARA",
                                        overwrite = TRUE, verbose = FALSE)
    testthat::expect_identical(out$full_phrases_ADD_LANGUAGE[1], "Texto em panará")
    html <- paste(readLines("__phrase_generating_guide.html", warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    testthat::expect_match(html, 'data-lang="PANARA"', fixed = TRUE)
  })
})

testthat::test_that("arboretum_phrases with save = FALSE writes nothing", {
  .with_tmp_wd("phrases-nosave", {
    csv <- .write_fixture_csv("data.csv")
    out <- aRboretum::arboretum_phrases(csv, printed_lang = "es", save = FALSE, verbose = FALSE)

    testthat::expect_true(all(nzchar(out$full_phrases_ES)))
    testthat::expect_false(file.exists("__phrase_generating_guide.html"))
    saved <- utils::read.csv(csv, stringsAsFactors = FALSE)
    testthat::expect_false("full_phrases_ES" %in% names(saved))
  })
})

testthat::test_that("arboretum_phrases validates its arguments", {
  testthat::expect_error(aRboretum::arboretum_phrases(NULL), "'data_path' must be provided")
  testthat::expect_error(aRboretum::arboretum_phrases("x.csv", printed_lang = "de"),
                         "Invalid language code")
  testthat::expect_error(aRboretum::arboretum_phrases("x.csv", overwrite = NA),
                         "'overwrite' must be TRUE or FALSE")
})
