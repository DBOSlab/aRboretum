# End-to-end rendering of species labels, using the package's own WGSRPD map
# and an offline replacement for geobr::read_state()

testthat::test_that("arboretum_labels renders complete labels with audios, photos, logo and custom language", {
  testthat::skip_if_not_installed("geobr")
  testthat::local_mocked_bindings(read_state = function(...) .fixture_br_states(),
                                  .package = "geobr")

  .with_tmp_wd("labels-render-full", {
    .write_fixture_csv("data.csv")

    # Personal audio for one species/language
    audio_sp <- file.path("arboretum_audios", "FABACEAE_Paubrasilia_echinata_EN")
    dir.create(audio_sp, recursive = TRUE)
    writeBin(as.raw(1:50), file.path(audio_sp, "paubrasilia.mp3"))

    # A photo larger than 1 MB (random noise) that must be compressed, plus a PNG
    photo_sp <- file.path("arboretum_photos", "FABACEAE_Paubrasilia_echinata_photos")
    dir.create(photo_sp, recursive = TRUE)
    big <- array(stats::runif(900 * 900 * 3), dim = c(900, 900, 3))
    jpeg::writeJPEG(big, file.path(photo_sp, "big.jpg"), quality = 1)
    testthat::expect_gt(file.info(file.path(photo_sp, "big.jpg"))$size, 1024^2)
    .write_fixture_png(file.path(photo_sp, "small.png"))

    .write_fixture_png("logo.png")

    testthat::expect_message(
      out <- suppressWarnings(aRboretum::arboretum_labels(
        data_path = "data.csv",
        printed_lang = c("en", "pt"),
        add_lang = "PANARA",
        path_to_logo = "logo.png",
        logo_url = "https://example.org",
        back_to_index = TRUE,
        index_file = "index.html",
        verbose = TRUE,
        dir = "labels"
      )),
      "Completed! Generated 2 HTML files"
    )

    testthat::expect_identical(
      sort(out),
      c("FABACEAE_Paubrasilia_echinata_label.html", "RUBIACEAE_Coffea_arabica_label.html")
    )
    testthat::expect_true(all(file.exists(file.path("labels", out))))

    # Copied and compressed personal media
    copied_photo <- file.path("labels", "__arboretum_photos",
                              "FABACEAE_Paubrasilia_echinata_photos", "big.jpg")
    testthat::expect_true(file.exists(copied_photo))
    testthat::expect_lte(file.info(copied_photo)$size, 1024^2)
    testthat::expect_true(dir.exists(file.path("labels", "__arboretum_audios")))

    # Assets and maps
    assets <- list.files(file.path("labels", "__assets"))
    testthat::expect_true("EN_IUCN.svg" %in% assets)
    testthat::expect_true("logo.png" %in% assets)
    maps <- list.files(file.path("labels", "__maps"), pattern = "\\.html$")
    testthat::expect_setequal(maps, c("Paubrasilia_echinata_world_map.html",
                                      "Paubrasilia_echinata_brazil_map.html",
                                      "Coffea_arabica_world_map.html",
                                      "Coffea_arabica_brazil_map.html"))

    html <- paste(readLines(file.path("labels", "FABACEAE_Paubrasilia_echinata_label.html"),
                            warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_match(html, "Paubrasilia echinata", fixed = TRUE)
    testthat::expect_match(html, "Texto em panará", fixed = TRUE)
    testthat::expect_match(html, "Dye and timber", fixed = TRUE)
    testthat::expect_match(html, "Árvore nacional do Brasil", fixed = TRUE)
    testthat::expect_match(html, "__arboretum_audios/FABACEAE_Paubrasilia_echinata_EN/paubrasilia.mp3",
                           fixed = TRUE)
    testthat::expect_match(html, '<img class="slide__img"', fixed = TRUE)
    testthat::expect_match(html, "https://example.org", fixed = TRUE)
    testthat::expect_match(html, 'href="index.html"', fixed = TRUE)
    testthat::expect_match(html, "__maps/Paubrasilia_echinata_brazil_map.html", fixed = TRUE)
    testthat::expect_match(html, "https://floradobrasil.jbrj.gov.br/FB1", fixed = TRUE)
    testthat::expect_match(html, "EN_IUCN.svg", fixed = TRUE)

    coffea <- paste(readLines(file.path("labels", "RUBIACEAE_Coffea_arabica_label.html"),
                              warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    testthat::expect_match(coffea, "Source of coffee", fixed = TRUE)
    testthat::expect_no_match(coffea, '<img class="slide__img"', fixed = TRUE)
  })
})

testthat::test_that("arboretum_labels skips empty media folders and supports world-only maps", {
  testthat::skip_if_not_installed("geobr")
  testthat::local_mocked_bindings(read_state = function(...) .fixture_br_states(),
                                  .package = "geobr")

  .with_tmp_wd("labels-render-empty", {
    df <- .fixture_species_df()[2, ]
    df$FFB.stateProvince <- NA
    df$introduced_to <- NA
    .write_fixture_csv("data.csv", df)
    dir.create("arboretum_audios")
    dir.create("arboretum_photos")

    msgs <- testthat::capture_messages(
      out <- suppressWarnings(aRboretum::arboretum_labels(
        data_path = "data.csv",
        printed_lang = "es",
        back_to_index = FALSE,
        verbose = TRUE,
        dir = "labels/"
      ))
    )

    testthat::expect_true(any(grepl("Created directory: labels", msgs)))
    testthat::expect_true(any(grepl("Audio folder is empty", msgs)))
    testthat::expect_true(any(grepl("Photo folder is empty", msgs)))
    testthat::expect_identical(out, "RUBIACEAE_Coffea_arabica_label.html")
    testthat::expect_identical(list.files(file.path("labels", "__maps"), pattern = "\\.html$"),
                               "Coffea_arabica_world_map.html")

    html <- paste(readLines(file.path("labels", out), warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    testthat::expect_no_match(html, 'href="index.html"', fixed = TRUE)
    testthat::expect_match(html, "es-ES", fixed = TRUE)
  })
})

testthat::test_that("arboretum_labels warns when copied media folders cannot be renamed", {
  .with_tmp_wd("labels-render-rename", {
    dir.create(file.path("arboretum_audios", "x"), recursive = TRUE)
    writeBin(as.raw(1), file.path("arboretum_audios", "x", "a.mp3"))
    dir.create(file.path("arboretum_photos", "x"), recursive = TRUE)
    writeBin(as.raw(1), file.path("arboretum_photos", "x", "a.jpg"))

    seen <- new.env()
    testthat::local_mocked_bindings(
      .copy_folder_progress = function(from, to, verbose) invisible(0),
      .build_arboretum_phrases = function(data_path, printed_lang, add_lang, verbose) {
        list(df = .fixture_species_df()[1, ], printed_lang = printed_lang,
             html_phrases = list(en = list("Paubrasilia echinata" = "Phrase")))
      },
      .generate_species_html = function(..., audio_dir, photo_dir) {
        seen$audio_dir <- audio_dir
        seen$photo_dir <- photo_dir
        "label.html"
      },
      .package = "aRboretum"
    )
    testthat::local_mocked_bindings(read_state = function(...) NULL, .package = "geobr")

    warnings <- testthat::capture_warnings(
      aRboretum::arboretum_labels(data_path = "data.csv", printed_lang = "en",
                                  verbose = FALSE, dir = "labels")
    )
    testthat::expect_length(grep("Source folder to rename not found", warnings), 2)
    testthat::expect_identical(seen$audio_dir, "labels/__arboretum_audios")
    testthat::expect_identical(seen$photo_dir, file.path("labels", "__arboretum_photos"))
  })
})

# Image compression ------------------------------------------------------------

testthat::test_that("compress_single_image shrinks jpeg and png files above the limit", {
  dir <- file.path(tempdir(), "compress-single")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  img <- array(stats::runif(200 * 200 * 3), dim = c(200, 200, 3))
  jpg <- file.path(dir, "a.jpg")
  png_file <- file.path(dir, "b.png")
  jpeg::writeJPEG(img, jpg, quality = 1)
  png::writePNG(img, png_file)
  jpg_size <- file.info(jpg)$size
  png_size <- file.info(png_file)$size

  testthat::expect_message(
    testthat::expect_true(aRboretum:::.compress_single_image(jpg, max_size_mb = 0.05)),
    "Compressed image: a.jpg"
  )
  testthat::expect_lt(file.info(jpg)$size, jpg_size)

  testthat::expect_true(
    aRboretum:::.compress_single_image(png_file, max_size_mb = png_size * 0.5 / 1024^2,
                                       verbose = FALSE)
  )
  testthat::expect_lt(file.info(png_file)$size, png_size)

  # Missing files and files already under the limit are left alone
  testthat::expect_false(aRboretum:::.compress_single_image(file.path(dir, "none.jpg")))
  testthat::expect_false(aRboretum:::.compress_single_image(jpg, max_size_mb = 10))
})

testthat::test_that("compress_single_image rejects unsupported or incompressible images", {
  dir <- file.path(tempdir(), "compress-reject")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  gif <- file.path(dir, "a.gif")
  writeBin(as.raw(sample(0:255, 5000, replace = TRUE)), gif)
  testthat::expect_warning(
    testthat::expect_false(aRboretum:::.compress_single_image(gif, max_size_mb = 0.001)),
    "Unsupported image format"
  )

  jpg <- file.path(dir, "a.jpg")
  jpeg::writeJPEG(array(stats::runif(60 * 60 * 3), dim = c(60, 60, 3)), jpg)

  # The target size can never be reached
  testthat::expect_false(aRboretum:::.compress_single_image(jpg, max_size_mb = 1e-6))

  # A "compressed" file that is not smaller is not copied back
  testthat::local_mocked_bindings(
    .compress_jpeg_to_target = function(img, out_path, size_limit) {
      writeBin(as.raw(rep(1, 1e5)), out_path)
      TRUE
    },
    .read_image_generic = function(path, ext) array(0, dim = c(2, 2, 3)),
    .package = "aRboretum"
  )
  testthat::expect_false(aRboretum:::.compress_single_image(jpg, max_size_mb = 1e-6))

  # Other extensions with a readable image fall through
  bmp <- file.path(dir, "a.bmp")
  writeBin(as.raw(rep(1, 5000)), bmp)
  testthat::expect_false(aRboretum:::.compress_single_image(bmp, max_size_mb = 0.001))
})

testthat::test_that("compress_png_to_target returns FALSE when the limit cannot be met", {
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  img <- array(stats::runif(40 * 40 * 3), dim = c(40, 40, 3))

  testthat::expect_false(aRboretum:::.compress_png_to_target(img, out, size_limit = 1))
  testthat::expect_true(aRboretum:::.compress_png_to_target(img, out, size_limit = 1e7))
})

testthat::test_that("compress_images_in_dir compresses large images and warns on unreadable ones", {
  dir <- file.path(tempdir(), "compress-dir")
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "sub"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  jpeg::writeJPEG(array(stats::runif(200 * 200 * 3), dim = c(200, 200, 3)),
                  file.path(dir, "sub", "ok.jpg"), quality = 1)
  writeBin(as.raw(sample(0:255, 50000, replace = TRUE)), file.path(dir, "broken.jpg"))
  writeLines("not an image", file.path(dir, "notes.txt"))

  warnings <- testthat::capture_warnings(
    out <- aRboretum:::.compress_images_in_dir(dir, max_size_mb = 0.03, verbose = FALSE)
  )

  testthat::expect_identical(basename(out), "ok.jpg")
  testthat::expect_true(any(grepl("Could not compress image", warnings)))
})

# HTML helpers -------------------------------------------------------------------

testthat::test_that("create_logo_tag validates the logo and optionally links it", {
  out_dir <- file.path(tempdir(), "logo-tag")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  logo <- .write_fixture_png(file.path(tempdir(), "logo-tag.png"))
  txt <- file.path(tempdir(), "logo-tag.txt")
  writeLines("x", txt)

  testthat::expect_null(aRboretum:::.create_logo_tag(NULL, NULL, out_dir))
  testthat::expect_error(aRboretum:::.create_logo_tag("missing.png", NULL, out_dir),
                         "Logo file not found")
  testthat::expect_error(aRboretum:::.create_logo_tag(txt, NULL, out_dir),
                         "Unsupported logo format")

  img <- as.character(aRboretum:::.create_logo_tag(logo, NULL, out_dir))
  testthat::expect_match(img, '^<img src="__assets/logo-tag.png"')

  linked <- as.character(aRboretum:::.create_logo_tag(logo, "https://example.org", out_dir))
  testthat::expect_match(linked, '<a href="https://example.org"', fixed = TRUE)
  testthat::expect_true(file.exists(file.path(out_dir, "__assets", "logo-tag.png")))
})

testthat::test_that("copy_asset_for_html and path_for_html handle edge cases", {
  out_dir <- file.path(tempdir(), "asset-paths")
  unlink(out_dir, recursive = TRUE)
  dir.create(file.path(out_dir, "media"), recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  testthat::expect_null(aRboretum:::.copy_asset_for_html(NULL, out_dir))
  testthat::expect_null(aRboretum:::.copy_asset_for_html("", out_dir))
  testthat::expect_error(aRboretum:::.copy_asset_for_html("missing.png", out_dir),
                         "Asset file not found")

  testthat::expect_null(aRboretum:::.path_for_html(NULL, out_dir))
  inside <- file.path(out_dir, "media", "a.mp3")
  writeBin(as.raw(1), inside)
  testthat::expect_identical(aRboretum:::.path_for_html(inside, out_dir), "media/a.mp3")
  testthat::expect_identical(aRboretum:::.path_for_html(file.path(tempdir(), "x", "b.mp3"), out_dir),
                             "b.mp3")
})

testthat::test_that("map helpers handle empty inputs, missing columns and introduced ranges", {
  testthat::expect_null(aRboretum:::.save_map_widget_for_html(NULL, tempdir(), "x"))
  testthat::expect_null(aRboretum:::.ggplot_map(NULL))
  testthat::expect_null(aRboretum:::.ggplot_map_br(NULL, data.frame()))

  br <- .fixture_br_states()
  testthat::expect_warning(
    testthat::expect_null(aRboretum:::.ggplot_map_br(br[, "abbrev_state"], data.frame())),
    "name_state"
  )

  # Naturalized/cultivated species are drawn in the non-native colour
  df_sp <- data.frame(FFB.stateProvince = "SP", FFB.establishmentMeans = NA,
                      stringsAsFactors = FALSE)
  br_plant <- aRboretum:::.get_pr_ab_br(df_sp, br)
  map <- aRboretum:::.ggplot_map_br(br_plant, df_sp)
  testthat::expect_s3_class(map, "leaflet")
  testthat::expect_match(jsonlite::toJSON(map$x$calls, auto_unbox = TRUE, force = TRUE),
                         "#905795", fixed = TRUE)

  world <- get("WGSRPD", envir = asNamespace("aRboretum"))
  none <- aRboretum:::.get_pr_ab_world(
    data.frame(botanical_country = "NA", introduced_to = "", stringsAsFactors = FALSE),
    world
  )
  testthat::expect_true(all(none$Freq_Native == 0))
  testthat::expect_true(all(none$Freq_Introduced == 0))

  some <- aRboretum:::.get_pr_ab_world(
    data.frame(botanical_country = "Brazil Northeast", introduced_to = "Cuba",
               stringsAsFactors = FALSE),
    world
  )
  testthat::expect_equal(sum(some$Freq_Native), 1)
  testthat::expect_equal(sum(some$Freq_Introduced), 1)
})
