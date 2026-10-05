# Real rendering of QR-code label sheets (no mocked graphics devices)

testthat::test_that("arboretum_qrcodes renders every detailed layout to PNG and PDF", {
  csv <- .write_fixture_csv(tempfile(fileext = ".csv"))
  logo <- system.file("figures", "reflora.png", package = "aRboretum")
  out_root <- file.path(tempdir(), "qr-detailed")
  unlink(out_root, recursive = TRUE)
  on.exit(unlink(out_root, recursive = TRUE), add = TRUE)

  for (layout in c("classic", "modern", "botanical")) {
    for (format in c("png", "pdf")) {
      out <- aRboretum::arboretum_qrcodes(
        data_path = csv,
        layout = layout,
        format = format,
        path_to_logo = logo,
        id_code = "genus",
        verbose = FALSE,
        dir = file.path(out_root, layout, format)
      )
      testthat::expect_length(out, 1)
      testthat::expect_true(file.exists(out), info = paste(layout, format))
      testthat::expect_gt(file.info(out)$size, 0)
    }
  }
})

testthat::test_that("arboretum_qrcodes builds label URLs from base_url and reports progress", {
  csv <- .write_fixture_csv(tempfile(fileext = ".csv"))
  out_dir <- file.path(tempdir(), "qr-base-url")
  unlink(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  urls <- character(0)
  draw <- aRboretum:::.draw_detailed_qr
  testthat::local_mocked_bindings(
    .draw_detailed_qr = function(..., qr_url) {
      urls <<- c(urls, qr_url)
      draw(..., qr_url = qr_url)
    },
    .package = "aRboretum"
  )

  msgs <- testthat::capture_messages(
    out <- aRboretum::arboretum_qrcodes(
      data_path = csv,
      layout = "modern",
      base_url = "https://example.org/site/",
      id_code = c("A-1", "B-2"),
      color = "#123456",
      format = "png",
      verbose = TRUE,
      dir = out_dir
    )
  )

  testthat::expect_true(file.exists(out))
  testthat::expect_identical(urls, c(
    "https://example.org/site/FABACEAE_Paubrasilia_echinata_label.html",
    "https://example.org/site/RUBIACEAE_Coffea_arabica_label.html"
  ))
  testthat::expect_true(any(grepl("Created directory", msgs)))
  testthat::expect_true(any(grepl("Using minisite base URL", msgs)))
  testthat::expect_true(any(grepl("Layout: modern", msgs)))
  testthat::expect_true(any(grepl("Saved:", msgs)))
})

testthat::test_that("arboretum_qrcodes falls back to FFB urls, then to taxon names", {
  df <- .fixture_species_df()
  df$POWO.url <- NA
  out_dir <- file.path(tempdir(), "qr-fallback")
  unlink(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  urls <- character(0)
  testthat::local_mocked_bindings(
    .draw_minimalist_qr = function(species_name, vernacular_name, qr_url, ...) {
      urls <<- c(urls, qr_url)
      invisible(NULL)
    },
    .package = "aRboretum"
  )

  ffb_csv <- .write_fixture_csv(tempfile(fileext = ".csv"), df)
  aRboretum::arboretum_qrcodes(ffb_csv, format = "png", verbose = FALSE, dir = out_dir)
  testthat::expect_identical(urls, c("https://floradobrasil.jbrj.gov.br/FB1", "Coffea arabica"))

  urls <- character(0)
  df$FFB.url <- NA
  name_csv <- .write_fixture_csv(tempfile(fileext = ".csv"), df)
  testthat::expect_message(
    aRboretum::arboretum_qrcodes(name_csv, format = "png", verbose = TRUE, dir = out_dir),
    "encoding taxon names"
  )
  testthat::expect_identical(urls, c("Paubrasilia echinata", "Coffea arabica"))
})

testthat::test_that("draw_detailed_qr handles every language, endemism, logo shape and missing field", {
  wide_logo <- tempfile(fileext = ".png")
  tall_logo <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, dim = c(20, 100, 3)), wide_logo)
  png::writePNG(array(0.5, dim = c(100, 20, 3)), tall_logo)

  cases <- expand.grid(
    lang = c("en", "pt", "es", "fr", "xx"),
    country = c("Brazil", "Peru | Bolivia"),
    stringsAsFactors = FALSE
  )

  out <- tempfile(fileext = ".png")
  grDevices::png(out, width = 21, height = 29.7, units = "cm", res = 72)
  on.exit(unlink(out), add = TRUE)

  for (k in seq_len(nrow(cases))) {
    for (layout in c("classic", "modern", "botanical")) {
      testthat::expect_no_error(aRboretum:::.draw_detailed_qr(
        species_name = "Paubrasilia echinata",
        authorship = if (k %% 2 == 0) NA else "Lam.",
        family_name = if (k %% 3 == 0) NA else "Fabaceae",
        vernacular_name = if (k %% 2 == 0) "pau-brasil | ibirapitanga" else NA,
        endemism = "Endemic",
        country = cases$country[k],
        qr_url = if (k %% 4 == 0) NA else "https://example.org",
        id_code = if (k %% 2 == 0) "ID-1" else NA,
        path_to_logo = if (k %% 3 == 0) NULL else if (k %% 2 == 0) wide_logo else tall_logo,
        x_cm = 1, y_cm = 1, w_cm = 6, h_cm = 8,
        qr_color = if (k %% 2 == 0) "#1a2e1a" else NULL,
        font_family = "sans",
        printed_lang = cases$lang[k],
        layout = layout,
        a4_w = 21, a4_h = 29.7
      ))
    }
  }

  # Endemic species without a country, and non-endemic species
  for (endem in list(c("Endemic", NA), c("Non-endemic", "Brazil"), c(NA, NA))) {
    testthat::expect_no_error(aRboretum:::.draw_detailed_qr(
      species_name = "Coffea arabica", authorship = "L.", family_name = "Rubiaceae",
      vernacular_name = "", endemism = endem[1], country = endem[2],
      qr_url = "", id_code = "", path_to_logo = NULL,
      x_cm = 1, y_cm = 1, w_cm = 6, h_cm = 8, qr_color = NULL, font_family = "serif",
      printed_lang = "en", layout = "classic", a4_w = 21, a4_h = 29.7
    ))
  }
  grDevices::dev.off()
})

testthat::test_that("qr_to_raster adds a white quiet zone around coloured modules", {
  mat <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
  r <- aRboretum:::.qr_to_raster(mat, dark_color = "#FF0000", quiet_zone = 1L)

  testthat::expect_s3_class(r, "raster")
  testthat::expect_identical(dim(r), c(4L, 4L))
  m <- as.matrix(r)
  testthat::expect_identical(m[1, 1], "#FFFFFF")
  testthat::expect_identical(m[2, 2], "#FF0000")
  testthat::expect_identical(m[2, 3], "#FFFFFF")
})

testthat::test_that("load_logo_raster reads jpeg logos and warns on unreadable files", {
  jpg <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(array(0.5, dim = c(10, 10, 3)), jpg)
  testthat::expect_identical(dim(aRboretum:::.load_logo_raster(jpg)), c(10L, 10L, 3L))

  broken <- tempfile(fileext = ".png")
  writeLines("not a png", broken)
  testthat::expect_warning(
    testthat::expect_null(aRboretum:::.load_logo_raster(broken)),
    "Failed to load logo"
  )
})
