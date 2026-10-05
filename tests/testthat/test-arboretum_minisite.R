# file: tests/testthat/test-arboretum_minisite.R

testthat::test_that("arboretum_minisite errors for missing labels directory", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_error(
    aRboretum::arboretum_minisite(labels_dir = file.path(tempdir(), "does-not-exist")),
    "Directory not found"
  )
})

testthat::test_that("arboretum_minisite validates site_title and output directory", {
  testthat::skip_if_not_installed("aRboretum")

  dir_path <- file.path(tempdir(), "minisite-title-check")
  unlink(dir_path, recursive = TRUE, force = TRUE)
  dir.create(dir_path, recursive = TRUE)
  writeLines("<html></html>", file.path(dir_path, "FABACEAE_Paubrasilia_echinata_label.html"))

  testthat::local_mocked_bindings(
    .arg_check_printed_lang = function(x) x,
    .parse_label_filenames = function(html_files, mined_df) {
      data.frame(
        family = "FABACEAE",
        species = "Paubrasilia echinata",
        vernacular = "",
        file = html_files,
        stringsAsFactors = FALSE
      )
    },
    .build_minisite_html = function(...) "<html><body>ok</body></html>",
    .package = "aRboretum"
  )

  testthat::expect_error(
    aRboretum::arboretum_minisite(
      labels_dir = dir_path,
      site_title = "",
      verbose = FALSE
    ),
    "site_title"
  )

  testthat::expect_error(
    aRboretum::arboretum_minisite(
      labels_dir = dir_path,
      output_file = file.path(tempdir(), "missing-parent", "index.html"),
      verbose = FALSE
    ),
    "Output directory does not exist"
  )
})

testthat::test_that("arboretum_minisite embeds logo and renames folder using ASCII site title", {
  testthat::skip_if_not_installed("aRboretum")

  dir_path <- file.path(tempdir(), "minisite-logo-case")
  unlink(dir_path, recursive = TRUE, force = TRUE)
  dir.create(dir_path, recursive = TRUE)
  writeLines("<html></html>", file.path(dir_path, "FABACEAE_Paubrasilia_echinata_label.html"))

  svg_file <- tempfile(fileext = ".svg")
  writeLines(
    c(
      '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
      '<rect width="10" height="10" /></svg>'
    ),
    svg_file
  )

  build_args <- list()

  testthat::local_mocked_bindings(
    .arg_check_printed_lang = function(x) x,
    .parse_label_filenames = function(html_files, mined_df) {
      data.frame(
        family = "FABACEAE",
        species = "Paubrasilia echinata",
        vernacular = "pau-brasil",
        file = html_files,
        stringsAsFactors = FALSE
      )
    },
    .build_minisite_html = function(species_df, mined_df, site_title, printed_lang,
                                    logo_data_uri, logo_url, group_by_family) {
      build_args$site_title <<- site_title
      build_args$printed_lang <<- printed_lang
      build_args$logo_data_uri <<- logo_data_uri
      build_args$logo_url <<- logo_url
      build_args$group_by_family <<- group_by_family
      "<html><body>logo ok</body></html>"
    },
    .package = "aRboretum"
  )

  out <- aRboretum::arboretum_minisite(
    labels_dir = dir_path,
    site_title = "Coleção Árvores",
    printed_lang = c("pt", "en"),
    logo = svg_file,
    logo_url = "https://example.org",
    group_by_family = FALSE,
    verbose = FALSE
  )

  renamed_dir <- "Colecao_Arvores_minisite"

  testthat::expect_identical(out, file.path(dir_path, "index.html"))
  testthat::expect_true(grepl("^data:image/svg\\+xml;base64,", build_args$logo_data_uri))
  testthat::expect_identical(build_args$logo_url, "https://example.org")
  testthat::expect_identical(build_args$printed_lang, c("pt", "en"))
  testthat::expect_false(build_args$group_by_family)
  testthat::expect_true(dir.exists(renamed_dir))
  testthat::expect_true(file.exists(file.path(renamed_dir, "index.html")))

  unlink(renamed_dir, recursive = TRUE, force = TRUE)
})

testthat::test_that("arboretum_minisite errors for missing or unsupported logo file", {
  testthat::skip_if_not_installed("aRboretum")

  dir_path <- file.path(tempdir(), "minisite-bad-logo")
  unlink(dir_path, recursive = TRUE, force = TRUE)
  dir.create(dir_path, recursive = TRUE)
  writeLines("<html></html>", file.path(dir_path, "FABACEAE_Paubrasilia_echinata_label.html"))

  testthat::local_mocked_bindings(
    .arg_check_printed_lang = function(x) x,
    .parse_label_filenames = function(html_files, mined_df) {
      data.frame(
        family = "FABACEAE",
        species = "Paubrasilia echinata",
        vernacular = "",
        file = html_files,
        stringsAsFactors = FALSE
      )
    },
    .build_minisite_html = function(...) "<html><body>ok</body></html>",
    .package = "aRboretum"
  )

  testthat::expect_error(
    aRboretum::arboretum_minisite(
      labels_dir = dir_path,
      logo = "missing-logo.png",
      verbose = FALSE
    ),
    "Logo file not found"
  )

  txt_file <- tempfile(fileext = ".txt")
  writeLines("plain text", txt_file)

  testthat::expect_error(
    aRboretum::arboretum_minisite(
      labels_dir = dir_path,
      logo = txt_file,
      verbose = FALSE
    ),
    "Unsupported logo format"
  )
})

testthat::test_that("parse_label_filenames fills empty vernacular names when mined data is absent", {
  testthat::skip_if_not_installed("aRboretum")

  html_files <- c("FABACEAE_Paubrasilia_echinata_label.html")
  out <- aRboretum:::.parse_label_filenames(html_files, mined_df = NULL)

  testthat::expect_identical(out$family, "FABACEAE")
  testthat::expect_identical(out$species, "Paubrasilia echinata")
  testthat::expect_identical(out$vernacular, "")
  testthat::expect_identical(out$file, html_files)
})

testthat::test_that("build_minisite_html without dashboard places language buttons in search bar", {
  testthat::skip_if_not_installed("aRboretum")

  species_df <- data.frame(
    family = "FABACEAE",
    species = "Paubrasilia echinata",
    vernacular = "",
    file = "FABACEAE_Paubrasilia_echinata_label.html",
    stringsAsFactors = FALSE
  )

  html <- aRboretum:::.build_minisite_html(
    species_df = species_df,
    mined_df = NULL,
    site_title = "Plant Collection",
    printed_lang = c("en", "pt"),
    logo_data_uri = NULL,
    logo_url = NULL,
    group_by_family = FALSE
  )

  testthat::expect_false(grepl("Collection dashboard", html, fixed = TRUE))
  testthat::expect_match(html, '<div class="search-bar">', fixed = TRUE)
  testthat::expect_match(html, 'data-lang="en"', fixed = TRUE)
  testthat::expect_match(html, 'data-lang="pt"', fixed = TRUE)
  testthat::expect_match(html, 'var groupByFamily = false;', fixed = TRUE)
})


# NOT WORKING TESTS ------------------------------------------------------------

# testthat::test_that("build_minisite_html renders dashboard logo and language controls", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   species_df <- data.frame(
#     family = c("ARECACEAE", "FABACEAE"),
#     species = c("Euterpe edulis", "Paubrasilia echinata"),
#     vernacular = c("juçara", "pau-brasil"),
#     file = c("ARECACEAE_Euterpe_edulis_label.html", "FABACEAE_Paubrasilia_echinata_label.html"),
#     stringsAsFactors = FALSE
#   )
#
#   mined_df <- data.frame(
#     taxonName = c("Euterpe edulis", "Paubrasilia echinata"),
#     origin = c("native", "native"),
#     phytogeographicDomain = c("Atlantic Forest", "Atlantic Forest"),
#     FFB.stateProvince = c("BA", "RJ"),
#     botanical_country = c("Brazil", "Brazil"),
#     stringsAsFactors = FALSE
#   )
#
#   html <- aRboretum:::.build_minisite_html(
#     species_df = species_df,
#     mined_df = mined_df,
#     site_title = 'My & "Site"',
#     printed_lang = c("en", "pt"),
#     logo_data_uri = "data:image/png;base64,abc",
#     logo_url = "https://example.org",
#     group_by_family = TRUE
#   )
#
#   testthat::expect_match(html, "<!DOCTYPE html>", fixed = TRUE)
#   testthat::expect_match(html, "My &amp; &quot;Site&quot;")
#   testthat::expect_match(html, 'data:image/png;base64,abc', fixed = TRUE)
#   testthat::expect_match(html, 'data-lang="en"', fixed = TRUE)
#   testthat::expect_match(html, 'data-lang="pt"', fixed = TRUE)
#   testthat::expect_match(html, "Collection dashboard", fixed = TRUE)
#   testthat::expect_match(html, "ARECACEAE", fixed = TRUE)
#   testthat::expect_match(html, "FABACEAE", fixed = TRUE)
#   testthat::expect_match(html, "juçara", fixed = TRUE)
# })
#
# testthat::test_that("html_escape escapes special characters and handles NA", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   testthat::expect_identical(aRboretum:::.html_escape(NA_character_), "")
#   testthat::expect_identical(
#     aRboretum:::.html_escape('a & <b> "c"'),
#     "a &amp; &lt;b&gt; &quot;c&quot;"
#   )
# })
#
# testthat::test_that("arboretum_minisite warns and proceeds when data_path cannot be read", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   dir_path <- file.path(tempdir(), "minisite-read-warning")
#   unlink(dir_path, recursive = TRUE, force = TRUE)
#   dir.create(dir_path, recursive = TRUE)
#   writeLines("<html></html>", file.path(dir_path, "FABACEAE_Paubrasilia_echinata_label.html"))
#
#   build_calls <- list()
#
#   testthat::local_mocked_bindings(
#     .arg_check_printed_lang = function(x) x,
#     .read_species_data = function(data_path, verbose) stop("bad input file"),
#     .parse_label_filenames = function(html_files, mined_df) {
#       build_calls$mined_df_is_null <<- is.null(mined_df)
#       data.frame(
#         family = "FABACEAE",
#         species = "Paubrasilia echinata",
#         vernacular = "",
#         file = html_files,
#         stringsAsFactors = FALSE
#       )
#     },
#     .build_minisite_html = function(species_df, mined_df, ...) {
#       build_calls$build_mined_df_is_null <<- is.null(mined_df)
#       "<html><body>ok</body></html>"
#     },
#     .package = "aRboretum"
#   )
#
#   out <- testthat::expect_warning(
#     aRboretum::arboretum_minisite(
#       labels_dir = dir_path,
#       data_path = "bad.xlsx",
#       site_title = "Collection Test",
#       verbose = FALSE
#     ),
#     "Could not load data_path"
#   )
#
#   renamed_dir <- "Collection_Test_minisite"
#   testthat::expect_identical(out, file.path(dir_path, "index.html"))
#   testthat::expect_true(isTRUE(build_calls$mined_df_is_null))
#   testthat::expect_true(isTRUE(build_calls$build_mined_df_is_null))
#   testthat::expect_true(dir.exists(renamed_dir))
#
#   unlink(renamed_dir, recursive = TRUE, force = TRUE)
# })
#
# testthat::test_that("arboretum_minisite errors when no label html files are present", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   dir_path <- file.path(tempdir(), "minisite-empty-labels")
#   unlink(dir_path, recursive = TRUE, force = TRUE)
#   dir.create(dir_path, recursive = TRUE)
#
#   testthat::expect_error(
#     aRboretum::arboretum_minisite(labels_dir = dir_path, verbose = FALSE),
#     "No '\\*_label\\\\.html' files found"
#   )
# })
#
# testthat::test_that("parse_label_filenames extracts family species and joins vernacular names", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   html_files <- c(
#     "FABACEAE_Paubrasilia_echinata_label.html",
#     "ARECACEAE_Euterpe_edulis_label.html"
#   )
#
#   mined_df <- data.frame(
#     taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
#     family = c("Fabaceae", "Arecaceae"),
#     vernacularNames = c("pau-brasil, ibirapiranga", "juçara"),
#     stringsAsFactors = FALSE
#   )
#
#   out <- aRboretum:::.parse_label_filenames(html_files, mined_df)
#
#   testthat::expect_identical(out$family, c("ARECACEAE", "FABACEAE"))
#   testthat::expect_identical(out$species, c("Euterpe edulis", "Paubrasilia echinata"))
#   testthat::expect_identical(out$vernacular, c("juçara", "pau-brasil, ibirapiranga"))
#   testthat::expect_identical(out$file, c(
#     "ARECACEAE_Euterpe_edulis_label.html",
#     "FABACEAE_Paubrasilia_echinata_label.html"
#   ))
# })
#
# testthat::test_that("establish_color returns mapped and fallback colors", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   testthat::expect_identical(aRboretum:::.establish_color("native"), "#2c5f2d")
#   testthat::expect_identical(aRboretum:::.establish_color("cultivated"), "#f1c40f")
#   testthat::expect_identical(aRboretum:::.establish_color("unknown-key"), "#95a5a6")
# })
#
# testthat::test_that("build_dashboard_section_html renders translated dashboard blocks", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   dash <- list(
#     n_species = 3L,
#     n_families = 2L,
#     origin_counts = c(native = 2, cultivated = 1),
#     phyto_counts = c(`Atlantic Forest` = 2, Amazon = 1),
#     state_counts = c(BA = 2, RJ = 1),
#     country_counts = c(Brazil = 2, Peru = 1)
#   )
#
#   lbl <- list(
#     title = "Collection dashboard",
#     species = "Species",
#     families = "Families",
#     origins = "Origins",
#     phytos = "Domains",
#     states = "States",
#     countries = "Countries",
#     no_data = "No data"
#   )
#
#   out <- aRboretum:::.build_dashboard_section_html(
#     dash = dash,
#     lbl = lbl,
#     lang_div = '<button class="lang-btn" data-lang="en">English</button>'
#   )
#
#   testthat::expect_match(out, "Collection dashboard", fixed = TRUE)
#   testthat::expect_match(out, "English", fixed = TRUE)
#   testthat::expect_match(out, "Species", fixed = TRUE)
#   testthat::expect_match(out, "Families", fixed = TRUE)
#   testthat::expect_match(out, "Countries", fixed = TRUE)
# })
#
# testthat::test_that("compute_dashboard_stats summarizes origins phyto and top places", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   species_df <- data.frame(
#     family = c("Fabaceae", "Arecaceae", "Fabaceae"),
#     species = c("Paubrasilia echinata", "Euterpe edulis", "Inga edulis"),
#     vernacular = c("pau-brasil", "juçara", "ingá"),
#     file = c("a.html", "b.html", "c.html"),
#     stringsAsFactors = FALSE
#   )
#
#   mined_df <- data.frame(
#     taxonName = c("Paubrasilia echinata", "Euterpe edulis", "Inga edulis"),
#     origin = c("native", "cultivated", "native"),
#     phytogeographicDomain = c("Atlantic Forest", "Atlantic Forest | Amazon", "Amazon"),
#     FFB.stateProvince = c("BA | RJ", "BA", NA),
#     botanical_country = c("Brazil", "Brazil | Peru", "Peru"),
#     stringsAsFactors = FALSE
#   )
#
#   out <- aRboretum:::.compute_dashboard_stats(species_df, mined_df)
#
#   testthat::expect_identical(out$n_species, 3L)
#   testthat::expect_identical(out$n_families, 2L)
#   testthat::expect_true("native" %in% names(out$origin_counts))
#   testthat::expect_true("cultivated" %in% names(out$origin_counts))
#   testthat::expect_true("Atlantic Forest" %in% names(out$phyto_counts))
#   testthat::expect_true("Amazon" %in% names(out$phyto_counts))
#   testthat::expect_true("BA" %in% names(out$state_counts))
#   testthat::expect_true("Brazil" %in% names(out$country_counts))
# })
#
# testthat::test_that("build_donut_html and build_bar_chart_html render expected fragments", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   donut <- aRboretum:::.build_donut_html(
#     counts = c(native = 2, cultivated = 1),
#     colors = c(native = "#2c5f2d", cultivated = "#f1c40f"),
#     center_number = 3,
#     center_label = "species"
#   )
#
#   bars <- aRboretum:::.build_bar_chart_html(
#     counts = c(BA = 3, RJ = 1),
#     colors = c(BA = "#2c5f2d", RJ = "#95a5a6"),
#     wide_labels = FALSE
#   )
#
#   empty_bars <- aRboretum:::.build_bar_chart_html(
#     counts = integer(0),
#     colors = character(0),
#     wide_labels = TRUE
#   )
#
#   testthat::expect_match(donut, 'class="donut"', fixed = TRUE)
#   testthat::expect_match(donut, "species", fixed = TRUE)
#   testthat::expect_match(donut, "native", ignore.case = TRUE)
#
#   testthat::expect_match(bars, 'class="bar-chart"', fixed = TRUE)
#   testthat::expect_match(bars, "BA", fixed = TRUE)
#   testthat::expect_match(bars, "RJ", fixed = TRUE)
#
#   testthat::expect_identical(empty_bars, "")
# })


# ------------------------------------------------------------------------------
# End-to-end minisite generation, with the dashboard built from the species data
# ------------------------------------------------------------------------------

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
