# file: tests/testthat/test-arboretum_qrcodes.R

testthat::test_that("arboretum_qrcodes creates output dir and saves minimalist PDF using POWO fallback", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    FFB.vernacularName = c("pau-brasil|ibirapitanga", "juçara"),
    POWO.url = c("https://powo.example/paubrasilia", NA),
    FFB.url = c(NA, "https://ffb.example/euterpe"),
    stringsAsFactors = FALSE
  )

  minimalist_calls <- list()
  device_calls <- list()

  old_wd <- getwd()
  tmp <- tempdir()
  out_dir <- file.path(tmp, "qrcode-pdf-out")
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)
  unlink(out_dir, recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .arg_check_dir = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .draw_minimalist_qr = function(species_name, vernacular_name, qr_url,
                                   x_cm, y_cm, w_cm, h_cm,
                                   qr_color, font_family, a4_w, a4_h) {
      minimalist_calls[[length(minimalist_calls) + 1L]] <<- list(
        species_name = species_name,
        vernacular_name = vernacular_name,
        qr_url = qr_url,
        x_cm = x_cm,
        y_cm = y_cm,
        w_cm = w_cm,
        h_cm = h_cm,
        qr_color = qr_color,
        font_family = font_family
      )
      invisible(NULL)
    },
    .draw_complete_qr = function(...) stop("complete layout should not be used"),
    .package = "aRboretum"
  )
  testthat::local_mocked_bindings(
    pdf = function(file, ...) {
      device_calls[[length(device_calls) + 1L]] <<- list(type = "pdf", file = file)
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      file.create(file)
      invisible(NULL)
    },
    png = function(file, ...) stop("png device should not be used"),
    dev.off = function(...) invisible(NULL),
    .package = "grDevices"
  )
  testthat::local_mocked_bindings(
    grid.newpage = function(...) invisible(NULL),
    grid.rect = function(...) invisible(NULL),
    .package = "grid"
  )

  out <- aRboretum::arboretum_qrcodes(
    data_path = "fake.xlsx",
    layout = "minimalist",
    format = "pdf",
    verbose = FALSE,
    dir = out_dir
  )

  testthat::expect_length(out, 1)
  testthat::expect_true(file.exists(out))
  testthat::expect_equal(length(device_calls), 1)
  testthat::expect_equal(length(minimalist_calls), 2)

  testthat::expect_identical(minimalist_calls[[1]]$qr_url, "https://powo.example/paubrasilia")
  testthat::expect_identical(minimalist_calls[[2]]$qr_url, "Euterpe edulis")
  testthat::expect_identical(minimalist_calls[[1]]$vernacular_name, "pau-brasil|ibirapitanga")
  testthat::expect_identical(minimalist_calls[[1]]$qr_color, "#1a2e1a")
})

testthat::test_that("arboretum_qrcodes uses explicit url vector and species filter", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis", "Inga edulis"),
    family = c("Fabaceae", "Arecaceae", "Fabaceae"),
    FFB.vernacularName = c(NA, "juçara", "ingá"),
    stringsAsFactors = FALSE
  )

  minimalist_calls <- list()

  out_dir <- file.path(tempdir(), "qrcode-url-out")
  unlink(out_dir, recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .arg_check_dir = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .draw_minimalist_qr = function(species_name, vernacular_name, qr_url,
                                   x_cm, y_cm, w_cm, h_cm,
                                   qr_color, font_family, a4_w, a4_h) {
      minimalist_calls[[length(minimalist_calls) + 1L]] <<- list(
        species_name = species_name,
        qr_url = qr_url
      )
      invisible(NULL)
    },
    .package = "aRboretum"
  )
  testthat::local_mocked_bindings(
    pdf = function(file, ...) {
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      file.create(file)
      invisible(NULL)
    },
    dev.off = function(...) invisible(NULL),
    .package = "grDevices"
  )
  testthat::local_mocked_bindings(
    grid.newpage = function(...) invisible(NULL),
    grid.rect = function(...) invisible(NULL),
    .package = "grid"
  )

  aRboretum::arboretum_qrcodes(
    data_path = "fake.xlsx",
    species = c("Inga edulis", "Paubrasilia echinata"),
    url = c("https://u1.example", "https://u2.example"),
    layout = "minimalist",
    verbose = FALSE,
    dir = out_dir
  )

  testthat::expect_equal(length(minimalist_calls), 2)
  testthat::expect_identical(minimalist_calls[[1]]$species_name, "Paubrasilia echinata")
  testthat::expect_identical(minimalist_calls[[2]]$species_name, "Inga edulis")
  testthat::expect_identical(minimalist_calls[[1]]$qr_url, "https://u1.example")
  testthat::expect_identical(minimalist_calls[[2]]$qr_url, "https://u2.example")
})

testthat::test_that("arboretum_qrcodes splits into multiple PNG pages when needed", {
  testthat::skip_if_not_installed("aRboretum")

  n <- 10L
  df <- data.frame(
    taxonName = paste("Species", seq_len(n)),
    family = rep("Fabaceae", n),
    FFB.vernacularName = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  minimalist_calls <- list()
  png_files <- character()

  out_dir <- file.path(tempdir(), "qrcode-png-pages")
  unlink(out_dir, recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .arg_check_dir = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .draw_minimalist_qr = function(species_name, vernacular_name, qr_url,
                                   x_cm, y_cm, w_cm, h_cm,
                                   qr_color, font_family, a4_w, a4_h) {
      minimalist_calls[[length(minimalist_calls) + 1L]] <<- species_name
      invisible(NULL)
    },
    .package = "aRboretum"
  )
  testthat::local_mocked_bindings(
    pdf = function(...) stop("pdf device should not be used"),
    png = function(file, ...) {
      png_files <<- c(png_files, file)
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      file.create(file)
      invisible(NULL)
    },
    dev.off = function(...) invisible(NULL),
    .package = "grDevices"
  )
  testthat::local_mocked_bindings(
    grid.newpage = function(...) invisible(NULL),
    grid.rect = function(...) invisible(NULL),
    .package = "grid"
  )

  out <- aRboretum::arboretum_qrcodes(
    data_path = "fake.xlsx",
    layout = "minimalist",
    format = "png",
    width = 9,
    length = 9,
    verbose = FALSE,
    dir = out_dir
  )

  testthat::expect_equal(length(out), 3)
  testthat::expect_equal(length(png_files), 3)
  testthat::expect_equal(length(minimalist_calls), n)
  testthat::expect_true(all(file.exists(out)))
  testthat::expect_true(any(grepl("page01\\.png$", out)))
  testthat::expect_true(any(grepl("page02\\.png$", out)))
  testthat::expect_true(any(grepl("page03\\.png$", out)))
})

testthat::test_that("arboretum_qrcodes validates species filter and dimensions", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .arg_check_dir = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .package = "aRboretum"
  )

  testthat::expect_error(
    aRboretum::arboretum_qrcodes(
      data_path = "fake.xlsx",
      species = "Species not found",
      verbose = FALSE
    ),
    "None of the specified species were found"
  )

  testthat::expect_error(
    aRboretum::arboretum_qrcodes(
      data_path = "fake.xlsx",
      width = 0,
      verbose = FALSE
    ),
    "'width' and 'length' must be positive numbers"
  )
})

testthat::test_that("load_logo_raster handles null missing unsupported and png paths", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_null(aRboretum:::.load_logo_raster(NULL))

  testthat::expect_warning(
    testthat::expect_null(aRboretum:::.load_logo_raster("missing-logo.png")),
    "Logo file not found"
  )

  txt_file <- tempfile(fileext = ".txt")
  writeLines("plain text", txt_file)
  testthat::expect_warning(
    testthat::expect_null(aRboretum:::.load_logo_raster(txt_file)),
    "Unsupported logo format"
  )

  png_file <- tempfile(fileext = ".png")
  writeBin(as.raw(1:10), png_file)

  testthat::local_mocked_bindings(
    readPNG = function(path) array(1, dim = c(2L, 3L, 4L)),
    .package = "png"
  )

  out <- aRboretum:::.load_logo_raster(png_file)
  testthat::expect_true(is.array(out))
  testthat::expect_identical(dim(out), c(2L, 3L, 4L))
})

testthat::test_that("draw_minimalist_qr uses species name when qr_url is missing", {
  testthat::skip_if_not_installed("aRboretum")

  qr_inputs <- list()
  raster_calls <- list()
  text_labels <- character()
  line_calls <- 0L

  testthat::local_mocked_bindings(
    .qr_to_raster = function(qr_mat, dark_color, quiet_zone = 2L) {
      qr_inputs[[length(qr_inputs) + 1L]] <<- list(
        qr_mat = qr_mat,
        dark_color = dark_color,
        quiet_zone = quiet_zone
      )
      matrix("#FFFFFF", nrow = 2, ncol = 2)
    },
    .package = "aRboretum"
  )
  testthat::local_mocked_bindings(
    qr_code = function(x, ecl = "M") {
      qr_inputs[[length(qr_inputs) + 1L]] <<- list(encoded = x, ecl = ecl)
      matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
    },
    .package = "qrcode"
  )
  testthat::local_mocked_bindings(
    pushViewport = function(...) invisible(NULL),
    popViewport = function(...) invisible(NULL),
    viewport = function(...) structure(list(...), class = "viewport"),
    unit = function(x, units) x,
    gpar = function(...) list(...),
    grid.rect = function(...) invisible(NULL),
    grid.text = function(label, ...) {
      text_labels <<- c(text_labels, label)
      invisible(NULL)
    },
    grid.lines = function(...) {
      line_calls <<- line_calls + 1L
      invisible(NULL)
    },
    grid.raster = function(image, ...) {
      raster_calls[[length(raster_calls) + 1L]] <<- image
      invisible(NULL)
    },
    .package = "grid"
  )

  aRboretum:::.draw_minimalist_qr(
    species_name = "Paubrasilia echinata",
    vernacular_name = "pau-brasil|ibirapitanga",
    qr_url = NA_character_,
    x_cm = 1,
    y_cm = 1,
    w_cm = 2.5,
    h_cm = 2.5,
    qr_color = "#1a2e1a",
    font_family = "serif",
    a4_w = 21,
    a4_h = 29.7
  )

  encoded_entry <- qr_inputs[[1]]$encoded
  testthat::expect_identical(encoded_entry, "Paubrasilia echinata")
  testthat::expect_true("Paubrasilia echinata" %in% text_labels)
  testthat::expect_true("pau-brasil" %in% text_labels)
  testthat::expect_equal(line_calls, 1L)
  testthat::expect_length(raster_calls, 1)
})


# NOT WORKING TESTS ------------------------------------------------------------

# testthat::test_that("draw_complete_qr handles endemism phrase, id_code, and logo loading", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   qr_inputs <- list()
#   text_labels <- character()
#   raster_calls <- list()
#
#   testthat::local_mocked_bindings(
#     .load_logo_raster = function(path_to_logo) array(1, dim = c(2L, 4L, 4L)),
#     .qr_to_raster = function(qr_mat, dark_color, quiet_zone = 4L) {
#       qr_inputs[[length(qr_inputs) + 1L]] <<- list(qr_mat = qr_mat, dark_color = dark_color, quiet_zone = quiet_zone)
#       matrix("#FFFFFF", nrow = 2, ncol = 2)
#     },
#     .package = "aRboretum"
#   )
#   testthat::local_mocked_bindings(
#     qr_code = function(x, ecl = "M") {
#       qr_inputs[[length(qr_inputs) + 1L]] <<- list(encoded = x, ecl = ecl)
#       matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
#     },
#     .package = "qrcode"
#   )
#   testthat::local_mocked_bindings(
#     pushViewport = function(...) invisible(NULL),
#     popViewport = function(...) invisible(NULL),
#     viewport = function(...) structure(list(...), class = "viewport"),
#     unit = function(x, units) x,
#     gpar = function(...) list(...),
#     grid.rect = function(...) invisible(NULL),
#     grid.text = function(label, ...) {
#       text_labels <<- c(text_labels, label)
#       invisible(NULL)
#     },
#     grid.lines = function(...) invisible(NULL),
#     grid.raster = function(image, ...) {
#       raster_calls[[length(raster_calls) + 1L]] <<- image
#       invisible(NULL)
#     },
#     .package = "grid"
#   )
#
#   aRboretum:::.draw_complete_qr(
#     species_name = "Paubrasilia echinata",
#     authorship = "Lam.",
#     family_name = "Fabaceae",
#     vernacular_name = "pau-brasil|ibirapitanga",
#     endemism = "Endemic",
#     country = "Brazil",
#     qr_url = "https://example.org/species",
#     id_code = "JBRJ-001",
#     path_to_logo = "logo.png",
#     x_cm = 1,
#     y_cm = 1,
#     w_cm = 6,
#     h_cm = 8,
#     qr_color = "#1a2e1a",
#     font_family = "serif",
#     printed_lang = "en",
#     a4_w = 21,
#     a4_h = 29.7
#   )
#
#   testthat::expect_identical(qr_inputs[[1]]$encoded, "https://example.org/species")
#   testthat::expect_true(any(grepl("Paubrasilia echinata", text_labels, fixed = TRUE)))
#   testthat::expect_true(any(grepl("Lam\\.", text_labels)))
#   testthat::expect_true(any(grepl("Fabaceae", text_labels, fixed = TRUE)))
#   testthat::expect_true(any(grepl("pau-brasil", text_labels, fixed = TRUE)))
#   testthat::expect_true(any(grepl("Brazil endemic", text_labels, ignore.case = TRUE)))
#   testthat::expect_true(any(grepl("JBRJ-001", text_labels, fixed = TRUE)))
#   testthat::expect_true(length(raster_calls) >= 2)
# })
#
# testthat::test_that("qr_to_raster adds quiet zone and colors dark modules", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   qr_mat <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2, byrow = TRUE)
#   out <- aRboretum:::.qr_to_raster(qr_mat, dark_color = "#123456", quiet_zone = 1L)
#
#   testthat::expect_true(is.matrix(out))
#   testthat::expect_identical(dim(out), c(4L, 4L))
#   testthat::expect_identical(out[1, 1], "#FFFFFF")
#   testthat::expect_identical(out[2, 2], "#123456")
#   testthat::expect_identical(out[2, 3], "#FFFFFF")
#   testthat::expect_identical(out[3, 3], "#123456")
# })
#
# testthat::test_that("arboretum_qrcodes uses base_url, column id_code, and complete layout", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   df <- data.frame(
#     taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
#     family = c("Fabaceae", "Arecaceae"),
#     scientificNameAuthorship = c("Lam.", "Mart."),
#     FFB.vernacularName = c("pau-brasil", "juçara"),
#     endemism = c("Endemic", "Not endemic"),
#     country = c("Brazil", "Brazil"),
#     accession_id = c("JBRJ-001", "JBRJ-002"),
#     stringsAsFactors = FALSE
#   )
#
#   complete_calls <- list()
#   device_calls <- list()
#
#   out_dir <- file.path(tempdir(), "qrcode-complete-out")
#   unlink(out_dir, recursive = TRUE, force = TRUE)
#
#   testthat::local_mocked_bindings(
#     .arg_check_dir = function(x) x,
#     .read_species_data = function(data_path, verbose) df,
#     .draw_minimalist_qr = function(...) stop("minimalist layout should not be used"),
#     .draw_complete_qr = function(species_name, authorship, family_name,
#                                  vernacular_name, endemism, country,
#                                  qr_url, id_code, path_to_logo,
#                                  x_cm, y_cm, w_cm, h_cm,
#                                  qr_color, font_family, printed_lang, a4_w, a4_h) {
#       complete_calls[[length(complete_calls) + 1L]] <<- list(
#         species_name = species_name,
#         authorship = authorship,
#         family_name = family_name,
#         vernacular_name = vernacular_name,
#         endemism = endemism,
#         country = country,
#         qr_url = qr_url,
#         id_code = id_code,
#         path_to_logo = path_to_logo,
#         printed_lang = printed_lang,
#         qr_color = qr_color,
#         font_family = font_family
#       )
#       invisible(NULL)
#     },
#     .package = "aRboretum"
#   )
#   testthat::local_mocked_bindings(
#     pdf = function(file, ...) {
#       device_calls[[length(device_calls) + 1L]] <<- list(type = "pdf", file = file)
#       dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
#       file.create(file)
#       invisible(NULL)
#     },
#     dev.off = function(...) invisible(NULL),
#     .package = "grDevices"
#   )
#   testthat::local_mocked_bindings(
#     grid.newpage = function(...) invisible(NULL),
#     grid.rect = function(...) invisible(NULL),
#     .package = "grid"
#   )
#
#   out <- aRboretum::arboretum_qrcodes(
#     data_path = "fake.xlsx",
#     layout = "complete",
#     printed_lang = c("pt", "en"),
#     id_code = "accession_id",
#     base_url = "https://example.org/minisite///",
#     path_to_logo = "logo.png",
#     color = "#123456",
#     font_family = "sans",
#     verbose = FALSE,
#     dir = out_dir
#   )
#
#   testthat::expect_length(out, 1)
#   testthat::expect_equal(length(complete_calls), 2)
#   testthat::expect_identical(complete_calls[[1]]$printed_lang, "pt")
#   testthat::expect_identical(complete_calls[[1]]$id_code, "JBRJ-001")
#   testthat::expect_identical(complete_calls[[2]]$id_code, "JBRJ-002")
#   testthat::expect_identical(
#     complete_calls[[1]]$qr_url,
#     "https://example.org/minisite/FABACEAE_Paubrasilia_echinata_label.html"
#   )
#   testthat::expect_identical(
#     complete_calls[[2]]$qr_url,
#     "https://example.org/minisite/ARECACEAE_Euterpe_edulis_label.html"
#   )
#   testthat::expect_identical(complete_calls[[1]]$qr_color, "#123456")
#   testthat::expect_identical(complete_calls[[1]]$font_family, "sans")
#   testthat::expect_true(file.exists(out))
# })
