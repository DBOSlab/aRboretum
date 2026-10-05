# End-to-end minisite generation with the dashboard built from the species data

.make_label_files <- function(dir, files = c("FABACEAE_Paubrasilia_echinata_label.html",
                                             "RUBIACEAE_Coffea_arabica_label.html")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in files) writeLines("<html></html>", file.path(dir, f))
  invisible(files)
}

testthat::test_that("arboretum_minisite builds a dashboard from the species data", {
  .with_tmp_wd("minisite-dashboard", {
    .make_label_files("labels")
    .write_fixture_csv("data.csv")
    .write_fixture_png("logo.png")

    msgs <- testthat::capture_messages(
      out <- aRboretum::arboretum_minisite(
        labels_dir = "labels",
        data_path = "data.csv",
        site_title = "Jardim Bot\u00e2nico",
        printed_lang = c("pt", "en"),
        logo = "logo.png",
        logo_url = "https://example.org",
        verbose = TRUE
      )
    )

    testthat::expect_identical(out, file.path("labels", "index.html"))
    testthat::expect_true(any(grepl("2 species across 2 families \\[pt/en\\]", msgs)))
    testthat::expect_true(any(grepl("renamed as 'Jardim_Botanico_minisite'", msgs)))

    # The labels folder is renamed after the ASCII site title
    testthat::expect_false(dir.exists("labels"))
    index <- file.path("Jardim_Botanico_minisite", "index.html")
    testthat::expect_true(file.exists(index))

    html <- paste(readLines(index, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_match(html, '<section class="dashboard">', fixed = TRUE)
    testthat::expect_match(html, "conic-gradient", fixed = TRUE)
    testthat::expect_match(html, "bar-chart", fixed = TRUE)
    testthat::expect_match(html, "Atlantic Forest", fixed = TRUE)
    testthat::expect_match(html, "Ethiopia", fixed = TRUE)
    testthat::expect_match(html, "pau-brasil | ibirapitanga", fixed = TRUE)
    testthat::expect_match(html, '<a href="https://example.org"', fixed = TRUE)
    testthat::expect_match(html, "data:image/png;base64,", fixed = TRUE)
    testthat::expect_match(html, 'class="lang-btn active" data-lang="pt"', fixed = TRUE)
  })
})

testthat::test_that("arboretum_minisite works with one language, an output file and no grouping", {
  .with_tmp_wd("minisite-single", {
    .make_label_files("labels")
    .write_fixture_png("logo.png")
    dir.create("site")

    out <- aRboretum::arboretum_minisite(
      labels_dir = "labels",
      output_file = file.path("site", "home.html"),
      printed_lang = "en",
      group_by_family = FALSE,
      logo = "logo.png",
      verbose = FALSE
    )

    testthat::expect_true(file.exists(file.path("site", "home.html")))
    html <- paste(readLines(file.path("site", "home.html"), warn = FALSE), collapse = "\n")
    testthat::expect_no_match(html, '<section class="dashboard">', fixed = TRUE)
    testthat::expect_no_match(html, 'class="lang-btn', fixed = TRUE)
    testthat::expect_match(html, '<img src="data:image/png;base64,', fixed = TRUE)
  })
})

testthat::test_that("arboretum_minisite warns and proceeds when data_path cannot be read", {
  .with_tmp_wd("minisite-bad-data", {
    .make_label_files("labels")
    writeLines("x", "bad.txt")

    testthat::expect_warning(
      aRboretum::arboretum_minisite(labels_dir = "labels", data_path = "bad.txt",
                                    printed_lang = "en", verbose = FALSE),
      "Could not load data_path"
    )
    testthat::expect_true(file.exists(file.path("Plant_Collection_minisite", "index.html")))
  })
})

testthat::test_that("arboretum_minisite errors when no label files are present", {
  .with_tmp_wd("minisite-empty", {
    dir.create("labels")
    testthat::expect_error(
      aRboretum::arboretum_minisite(labels_dir = "labels", verbose = FALSE),
      "No '\\*_label.html' files found"
    )
  })
})

# Helpers --------------------------------------------------------------------------

testthat::test_that("parse_label_filenames joins vernacular names from the data", {
  mined <- .fixture_species_df()
  mined$FFB.vernacularName[2] <- " | "

  out <- aRboretum:::.parse_label_filenames(
    c("FABACEAE_Paubrasilia_echinata_label.html", "RUBIACEAE_Coffea_arabica_label.html",
      "MYRTACEAE_Eugenia_uniflora_label.html"),
    mined
  )

  testthat::expect_identical(out$family, c("FABACEAE", "RUBIACEAE", "MYRTACEAE"))
  testthat::expect_identical(out$species, c("Paubrasilia echinata", "Coffea arabica", "Eugenia uniflora"))
  testthat::expect_identical(out$vernacular, c("pau-brasil | ibirapitanga", "", ""))
})

testthat::test_that("establish_color maps establishment means to colours", {
  testthat::expect_identical(aRboretum:::.establish_color("Native"), "#2c5f2d")
  testthat::expect_identical(aRboretum:::.establish_color(" naturalizada "), "#7b5ea7")
  testthat::expect_identical(aRboretum:::.establish_color("Cultivated"), "#e07b39")
  testthat::expect_identical(aRboretum:::.establish_color("unknown"), "#95a5a6")
})

testthat::test_that("compute_dashboard_stats counts species, origins and places", {
  species_df <- data.frame(family = c("FABACEAE", "RUBIACEAE"),
                           species = c("Paubrasilia echinata", "Coffea arabica"),
                           stringsAsFactors = FALSE)
  dash <- aRboretum:::.compute_dashboard_stats(species_df, .fixture_species_df())

  testthat::expect_equal(dash$n_species, 2)
  testthat::expect_equal(dash$n_genera, 2)
  testthat::expect_equal(dash$n_families, 2)
  testthat::expect_equal(as.integer(dash$estab_counts[c("Native", "Cultivated")]), c(1L, 1L))
  testthat::expect_setequal(names(dash$state_counts), c("BA", "PE", "SP"))
  testthat::expect_identical(names(dash$phyto_counts), "Atlantic Forest")
  testthat::expect_setequal(names(dash$country_counts), c("Brazil", "Ethiopia", "Kenya"))

  empty <- .fixture_species_df()
  empty[, c("FFB.establishmentMeans", "FFB.stateProvince", "FFB.phytogeographicDomain", "country")] <- NA
  dash_empty <- aRboretum:::.compute_dashboard_stats(species_df, empty)
  testthat::expect_length(dash_empty$estab_counts, 0)
  testthat::expect_length(dash_empty$state_counts, 0)

  # Without any data the dashboard still renders, with placeholders
  section <- aRboretum:::.build_dashboard_section_html(
    dash_empty, aRboretum:::.dash_labels[["en"]], lang_buttons_html = ""
  )
  testthat::expect_match(section, '<section class="dashboard">', fixed = TRUE)
  testthat::expect_no_match(section, "conic-gradient", fixed = TRUE)
})

testthat::test_that("donut and bar chart builders render or fall back", {
  counts <- c(Native = 3L, Cultivated = 1L)

  testthat::expect_identical(aRboretum:::.build_donut_html(integer(0), 0, "spp"), "")
  donut <- aRboretum:::.build_donut_html(counts, 4, "spp",
                                         color_func = aRboretum:::.establish_color)
  testthat::expect_match(donut, "#2c5f2d 0deg 270deg", fixed = TRUE)
  testthat::expect_match(donut, "3&nbsp;(75%)", fixed = TRUE)
  testthat::expect_match(aRboretum:::.build_donut_html(counts, 4, "spp"), "#5B8C5A", fixed = TRUE)

  testthat::expect_match(aRboretum:::.build_bar_chart_html(integer(0)), "--", fixed = TRUE)
  many <- stats::setNames(30:1, paste0("C", 1:30))
  bars <- aRboretum:::.build_bar_chart_html(many, max_show = 5L, wide_label = TRUE)
  testthat::expect_equal(lengths(regmatches(bars, gregexpr('class="bar-row"', bars))), 5)
  testthat::expect_match(bars, "bar-label-wide", fixed = TRUE)
})

testthat::test_that("html_escape escapes special characters", {
  testthat::expect_identical(aRboretum:::.html_escape('<a href="x">&</a>'),
                             "&lt;a href=&quot;x&quot;&gt;&amp;&lt;/a&gt;")
})
