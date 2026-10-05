# file: tests/testthat/test-arboretum_labels.R

testthat::test_that("arboretum_labels does not add custom language when custom phrases are absent", {
  testthat::skip_if_not_installed("aRboretum")
  testthat::skip_if_not_installed("geobr")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    full_phrases_PT = "Frase",
    full_phrases_EN = "Phrase",
    full_phrases_ADD_LANGUAGE = "   ",
    stringsAsFactors = FALSE
  )

  seen_printed_lang <- NULL

  gen_stub <- function(species_data,
                       phrases,
                       world,
                       br_states,
                       printed_lang,
                       add_lang,
                       path_to_logo,
                       logo_url,
                       output_dir,
                       audio_dir,
                       photo_dir,
                       back_to_index,
                       index_file,
                       verbose) {
    seen_printed_lang <<- printed_lang
    out <- "species.html"
    writeLines("<html></html>", file.path(output_dir, out))
    out
  }

  out_dir <- file.path(tempdir(), "labels-out-no-add")
  unlink(out_dir, recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .arg_check_dir = function(x) x,
    .arg_check_printed_lang = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .copy_folder_progress = function(from, to, verbose) invisible(NULL),
    .generate_species_html = gen_stub,
    .package = "aRboretum"
  )
  testthat::local_mocked_bindings(
    read_state = function(...) data.frame(abbrev_state = character(0), stringsAsFactors = FALSE),
    .package = "geobr"
  )

  aRboretum::arboretum_labels(
    data_path = "fake.xlsx",
    printed_lang = c("pt", "en"),
    add_lang = "PANARA",
    verbose = FALSE,
    dir = out_dir
  )

  testthat::expect_identical(seen_printed_lang, c("pt", "en"))
})

testthat::test_that("check_species_photos returns only supported images", {
  testthat::skip_if_not_installed("aRboretum")

  species_data <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    stringsAsFactors = FALSE
  )

  base_dir <- file.path(tempdir(), "photo-test")
  folder <- file.path(base_dir, "FABACEAE_Paubrasilia_echinata_photos")
  unlink(base_dir, recursive = TRUE, force = TRUE)
  dir.create(folder, recursive = TRUE)

  writeBin(as.raw(1:10), file.path(folder, "a.jpg"))
  writeBin(as.raw(1:10), file.path(folder, "b.PNG"))
  writeLines("ignore", file.path(folder, "notes.txt"))

  out <- aRboretum:::.check_species_photos(species_data, photo_dir = base_dir)

  testthat::expect_length(out, 2)
  testthat::expect_true(all(grepl("\\.(jpg|PNG)$", basename(out))))
  testthat::expect_identical(
    aRboretum:::.check_species_photos(species_data, photo_dir = file.path(tempdir(), "missing-photo-dir")),
    character(0)
  )
})

testthat::test_that("check_personal_audio returns first matching audio file per language", {
  testthat::skip_if_not_installed("aRboretum")

  species_data <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    stringsAsFactors = FALSE
  )

  base_dir <- file.path(tempdir(), "audio-test")
  unlink(base_dir, recursive = TRUE, force = TRUE)
  dir.create(file.path(base_dir, "FABACEAE_Paubrasilia_echinata_EN"), recursive = TRUE)
  dir.create(file.path(base_dir, "FABACEAE_Paubrasilia_echinata_PT"), recursive = TRUE)
  writeBin(as.raw(1:10), file.path(base_dir, "FABACEAE_Paubrasilia_echinata_EN", "voice.mp3"))
  writeBin(as.raw(1:10), file.path(base_dir, "FABACEAE_Paubrasilia_echinata_PT", "voice.wav"))
  writeLines("ignore", file.path(base_dir, "FABACEAE_Paubrasilia_echinata_PT", "readme.txt"))

  out <- aRboretum:::.check_personal_audio(
    species_data = species_data,
    printed_lang = c("en", "pt", "fr"),
    audio_dir = base_dir
  )

  testthat::expect_match(basename(out$en), "voice.mp3", fixed = TRUE)
  testthat::expect_match(basename(out$pt), "voice.wav", fixed = TRUE)
  testthat::expect_null(out$fr)
})

testthat::test_that("mk_map_dist returns world-only or world-plus-brazil branches", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::local_mocked_bindings(
    .get_pr_ab_world = function(df_sp, world) "world-data",
    .ggplot_map = function(world_plant) paste("world-plot", world_plant),
    .get_pr_ab_br = function(df_sp, br_states) data.frame(Freq = c(0, 1)),
    .ggplot_map_br = function(br_plant, df_sp) paste("br-plot", sum(br_plant$Freq)),
    .package = "aRboretum"
  )

  world_only <- aRboretum:::.mk_map_dist(
    df_sp = data.frame(FFB.stateProvince = NA_character_, stringsAsFactors = FALSE),
    world = data.frame(),
    br_states = data.frame()
  )
  both_maps <- aRboretum:::.mk_map_dist(
    df_sp = data.frame(FFB.stateProvince = "BA | MG", stringsAsFactors = FALSE),
    world = data.frame(),
    br_states = data.frame()
  )

  testthat::expect_identical(world_only$world_map, "world-plot world-data")
  testthat::expect_null(world_only$brazil_map)
  testthat::expect_identical(both_maps$world_map, "world-plot world-data")
  testthat::expect_identical(both_maps$brazil_map, "br-plot 1")
})


testthat::test_that("resize_image_array rescales 2D and 3D arrays and validates scale", {
  testthat::skip_if_not_installed("aRboretum")

  mat <- matrix(seq_len(100), nrow = 10, ncol = 10)
  arr <- array(runif(10 * 10 * 3), dim = c(10, 10, 3))

  small_mat <- aRboretum:::.resize_image_array(mat, scale = 0.5)
  small_arr <- aRboretum:::.resize_image_array(arr, scale = 0.5)

  testthat::expect_true(is.matrix(small_mat))
  testthat::expect_identical(dim(small_mat), c(5L, 5L))
  testthat::expect_true(is.array(small_arr))
  testthat::expect_identical(dim(small_arr), c(5L, 5L, 3L))

  testthat::expect_error(
    aRboretum:::.resize_image_array(arr, scale = 0),
    "`scale` must be a single number in \\(0, 1\\]"
  )
  testthat::expect_error(
    aRboretum:::.resize_image_array(arr, scale = 1.2),
    "`scale` must be a single number in \\(0, 1\\]"
  )
  testthat::expect_error(
    aRboretum:::.resize_image_array(1:5, scale = 0.5),
    "`img` must be a 2D or 3D image array\\."
  )
})

testthat::test_that("read_image_generic reads jpg and png and returns NULL for unsupported formats", {
  testthat::skip_if_not_installed("aRboretum")

  img_rgb <- array(runif(50 * 50 * 3), dim = c(50, 50, 3))
  png_file <- tempfile(fileext = ".png")
  jpg_file <- tempfile(fileext = ".jpg")

  png::writePNG(img_rgb, target = png_file)
  jpeg::writeJPEG(img_rgb, target = jpg_file, quality = 0.8)

  png_img <- aRboretum:::.read_image_generic(png_file, "png")
  jpg_img <- aRboretum:::.read_image_generic(jpg_file, "jpg")
  bad_img <- aRboretum:::.read_image_generic(jpg_file, "gif")

  testthat::expect_true(is.array(png_img))
  testthat::expect_true(is.array(jpg_img))
  testthat::expect_null(bad_img)
})

testthat::test_that("compress_jpeg_to_target creates a jpeg under size limit", {
  testthat::skip_if_not_installed("aRboretum")

  img_rgb <- array(runif(1600 * 1600 * 3), dim = c(1600, 1600, 3))
  out_file <- tempfile(fileext = ".jpg")

  ok <- aRboretum:::.compress_jpeg_to_target(
    img = img_rgb,
    out_path = out_file,
    size_limit = 100 * 1024
  )

  testthat::expect_true(ok)
  testthat::expect_true(file.exists(out_file))
  testthat::expect_lte(file.info(out_file)$size, 100 * 1024)
})

testthat::test_that("compress_images_in_dir handles NULL, missing, and empty directories", {
  testthat::skip_if_not_installed("aRboretum")

  empty_dir <- file.path(tempdir(), "compress-images-empty-dir")
  unlink(empty_dir, recursive = TRUE, force = TRUE)
  dir.create(empty_dir, recursive = TRUE)

  testthat::expect_identical(
    aRboretum:::.compress_images_in_dir(NULL, max_size_mb = 1, verbose = FALSE),
    character(0)
  )
  testthat::expect_identical(
    aRboretum:::.compress_images_in_dir(file.path(tempdir(), "missing-dir"), max_size_mb = 1, verbose = FALSE),
    character(0)
  )
  testthat::expect_identical(
    aRboretum:::.compress_images_in_dir(empty_dir, max_size_mb = 1, verbose = FALSE),
    character(0)
  )
})


# NOT WORKING TESTS ------------------------------------------------------------

# testthat::test_that("generate_species_html builds JSON payloads with extra text and custom language", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   species_data <- data.frame(
#     taxonName = "Paubrasilia echinata",
#     family = "Fabaceae",
#     scientificNameAuthorship = "Lam.",
#     plant_uses_EN = "Wood use",
#     free_notes_EN = "Endemic species",
#     IUCN.status = NA_character_,
#     FFB.url = "https://example.org/ffb",
#     POWO.url = "https://example.org/powo",
#     botanical_country = NA_character_,
#     introduced_to = NA_character_,
#     FFB.stateProvince = NA_character_,
#     stringsAsFactors = FALSE
#   )
#
#   phrases <- list(
#     en = list("Paubrasilia echinata" = "<i>Tree species</i>"),
#     PANARA = list("Paubrasilia echinata" = "Texto Panará")
#   )
#
#   captured <- new.env(parent = emptyenv())
#   captured$visible <- NULL
#   captured$spoken <- NULL
#   captured$langs <- NULL
#
#   build_stub <- function(species_name,
#                          family_name,
#                          authorship,
#                          logo_tag,
#                          package_logos,
#                          js_printed_labels,
#                          js_visible_distributions,
#                          js_spoken_texts,
#                          js_voice_langs,
#                          js_rate,
#                          js_pitch,
#                          js_volume,
#                          js_initial_lang,
#                          js_audio_files,
#                          printed_lang,
#                          top_logos,
#                          world_map_html,
#                          brazil_map_html,
#                          slideshow_tag,
#                          back_to_index,
#                          index_file) {
#     captured$visible <- jsonlite::fromJSON(js_visible_distributions)
#     captured$spoken <- jsonlite::fromJSON(js_spoken_texts)
#     captured$langs <- printed_lang
#     htmltools::tags$html(htmltools::tags$body("ok"))
#   }
#
#   out_dir <- file.path(tempdir(), "species-html-out")
#   unlink(out_dir, recursive = TRUE, force = TRUE)
#   dir.create(out_dir, recursive = TRUE)
#
#   testthat::local_mocked_bindings(
#     .mk_map_dist = function(df_sp, world, br_states) list(world_map = NULL, brazil_map = NULL),
#     .create_logo_tag = function(path_to_logo, logo_url) NULL,
#     .check_personal_audio = function(species_data, printed_lang, audio_dir) {
#       stats::setNames(as.list(rep(list(NULL), length(printed_lang))), printed_lang)
#     },
#     .check_species_photos = function(species_data, photo_dir) character(0),
#     .build_html_page = build_stub,
#     .package = "aRboretum"
#   )
#
#   out <- aRboretum:::.generate_species_html(
#     species_data = species_data,
#     phrases = phrases,
#     world = data.frame(),
#     br_states = data.frame(),
#     printed_lang = c("en", "PANARA"),
#     path_to_logo = NULL,
#     logo_url = NULL,
#     output_dir = out_dir,
#     audio_dir = NULL,
#     photo_dir = NULL,
#     back_to_index = TRUE,
#     index_file = "index.html",
#     verbose = FALSE
#   )
#
#   testthat::expect_true(file.exists(file.path(out_dir, out)))
#   testthat::expect_identical(captured$langs, c("en", "PANARA"))
#   testthat::expect_match(captured$visible$en, "Tree species")
#   testthat::expect_match(captured$visible$en, "Wood use", fixed = TRUE)
#   testthat::expect_match(captured$visible$en, "Endemic species", fixed = TRUE)
#   testthat::expect_identical(captured$visible$PANARA, "Texto Panará")
#   testthat::expect_match(captured$spoken$en, "Overview of the species", fixed = TRUE)
#   testthat::expect_match(captured$spoken$en, "Wood use", fixed = TRUE)
# })
#
# testthat::test_that("build_html_page renders custom language button, back link, maps, and slideshow", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   page <- aRboretum:::.build_html_page(
#     species_name = "Paubrasilia echinata",
#     family_name = "Fabaceae",
#     authorship = "Lam.",
#     logo_tag = htmltools::tags$img(src = "logo.png"),
#     package_logos = list(htmltools::tags$img(src = "pkg.png")),
#     js_printed_labels = jsonlite::toJSON(list(en = list(back_index = "Back to main page")), auto_unbox = TRUE),
#     js_visible_distributions = jsonlite::toJSON(list(en = "English text"), auto_unbox = TRUE),
#     js_spoken_texts = jsonlite::toJSON(list(en = "Spoken text"), auto_unbox = TRUE),
#     js_voice_langs = jsonlite::toJSON(list(en = "en-US"), auto_unbox = TRUE),
#     js_rate = jsonlite::toJSON(1, auto_unbox = TRUE),
#     js_pitch = jsonlite::toJSON(1, auto_unbox = TRUE),
#     js_volume = jsonlite::toJSON(1, auto_unbox = TRUE),
#     js_initial_lang = jsonlite::toJSON("en", auto_unbox = TRUE),
#     js_audio_files = jsonlite::toJSON(list(en = NULL), auto_unbox = TRUE),
#     printed_lang = c("en", "PANARA"),
#     top_logos = list(htmltools::tags$img(src = "status.png")),
#     world_map_html = htmltools::tags$div("world map"),
#     brazil_map_html = htmltools::tags$div("brazil map"),
#     slideshow_tag = htmltools::tags$div("photos block"),
#     back_to_index = TRUE,
#     index_file = "index.html"
#   )
#
#   html <- htmltools::renderTags(page)$html
#
#   testthat::expect_match(html, "Paubrasilia echinata", fixed = TRUE)
#   testthat::expect_match(html, 'data-lang="PANARA"', fixed = TRUE)
#   testthat::expect_match(html, ">PANARA<")
#   testthat::expect_match(html, "index.html", fixed = TRUE)
#   testthat::expect_match(html, "world map", fixed = TRUE)
#   testthat::expect_match(html, "brazil map", fixed = TRUE)
#   testthat::expect_match(html, "photos block", fixed = TRUE)
# })
#
# testthat::test_that("create_logo_tag handles NULL, missing, valid, and unsupported files", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   testthat::expect_null(aRboretum:::.create_logo_tag(NULL, NULL))
#
#   testthat::expect_error(
#     aRboretum:::.create_logo_tag("missing-logo.png", NULL),
#     "Logo file not found"
#   )
#
#   svg_file <- tempfile(fileext = ".svg")
#   writeLines(
#     c(
#       '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
#       '<rect width="10" height="10" /></svg>'
#     ),
#     svg_file
#   )
#
#   tag <- aRboretum:::.create_logo_tag(svg_file, "https://example.org")
#   rendered <- htmltools::renderTags(tag)$html
#   testthat::expect_match(rendered, "<a")
#   testthat::expect_match(rendered, "https://example.org", fixed = TRUE)
#   testthat::expect_match(rendered, "data:image/svg+xml;base64,", fixed = TRUE)
#
#   txt_file <- tempfile(fileext = ".txt")
#   writeLines("plain text", txt_file)
#   testthat::expect_error(
#     aRboretum:::.create_logo_tag(txt_file, NULL),
#     "Unsupported logo format"
#   )
# })
#
# testthat::test_that("arboretum_labels adds custom language when full_phrases_ADD_LANGUAGE is present", {
#   testthat::skip_if_not_installed("aRboretum")
#   testthat::skip_if_not_installed("geobr")
#
#   df <- data.frame(
#     taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
#     family = c("Fabaceae", "Arecaceae"),
#     full_phrases_ADD_LANGUAGE = c("Texto Panará 1", "Texto Panará 2"),
#     stringsAsFactors = FALSE
#   )
#
#   calls <- list()
#
#   phrase_stub <- function(df, dict, lang, verbose) {
#     stats::setNames(
#       as.list(paste("Phrase", toupper(lang), "for", df$taxonName)),
#       df$taxonName
#     )
#   }
#
#   gen_stub <- function(species_data,
#                        phrases,
#                        world,
#                        br_states,
#                        printed_lang,
#                        path_to_logo,
#                        logo_url,
#                        output_dir,
#                        audio_dir,
#                        photo_dir,
#                        back_to_index,
#                        index_file,
#                        verbose) {
#     calls[[length(calls) + 1L]] <<- list(
#       species = species_data$taxonName[1],
#       printed_lang = printed_lang,
#       add_phrase = phrases[["PANARA"]][[species_data$taxonName[1]]],
#       audio_dir = audio_dir,
#       photo_dir = photo_dir
#     )
#     out <- paste0(gsub("\\s+", "_", species_data$taxonName[1]), ".html")
#     writeLines("<html><body>ok</body></html>", file.path(output_dir, out))
#     out
#   }
#
#   old_wd <- getwd()
#   tmp <- tempdir()
#   out_dir <- file.path(tmp, "labels-out")
#   on.exit(setwd(old_wd), add = TRUE)
#   setwd(tmp)
#   unlink(out_dir, recursive = TRUE, force = TRUE)
#
#   testthat::local_mocked_bindings(
#     .arg_check_dir = function(x) x,
#     .arg_check_printed_lang = function(x) x,
#     .read_species_data = function(data_path, verbose) df,
#     .copy_folder_progress = function(from, to, verbose) invisible(NULL),
#     .phrase_generator = phrase_stub,
#     .dict = function() list(),
#     .get_world_map = function() data.frame(LEVEL3_NAM = character(0), stringsAsFactors = FALSE),
#     .generate_species_html = gen_stub,
#     .package = "aRboretum"
#   )
#   testthat::local_mocked_bindings(
#     read_state = function(...) data.frame(abbrev_state = character(0), stringsAsFactors = FALSE),
#     .package = "geobr"
#   )
#
#   out <- aRboretum::arboretum_labels(
#     data_path = "fake.xlsx",
#     printed_lang = c("pt", "en"),
#     add_lang = "PANARA",
#     audio_dir = "missing_audios",
#     photo_dir = "missing_photos",
#     verbose = FALSE,
#     dir = out_dir
#   )
#
#   testthat::expect_length(out, 2)
#   testthat::expect_equal(length(calls), 2)
#   testthat::expect_true(all(vapply(calls, function(x) "PANARA" %in% x$printed_lang, logical(1))))
#   testthat::expect_identical(calls[[1]]$add_phrase, "Texto Panará 1")
#   testthat::expect_identical(calls[[2]]$add_phrase, "Texto Panará 2")
#   testthat::expect_true(all(file.exists(file.path(out_dir, out))))
# })


# ------------------------------------------------------------------------------
# End-to-end label rendering (package WGSRPD map, offline geobr::read_state())
# ------------------------------------------------------------------------------

testthat::test_that("arboretum_labels renders complete labels with audios, photos, logo and custom language", {
  testthat::skip_if_not_installed("geobr")
  testthat::local_mocked_bindings(read_state = function(...) .fixture_br_states(),
                                  .package = "geobr")

  .with_tmp_wd("labels-render-full", {
    .write_fixture_csv("data.csv")

    # Phrases are generated, then one is edited by hand and the custom
    # language is filled in, as done through the phrase guide
    aRboretum::arboretum_phrases("data.csv", printed_lang = c("en", "pt"),
                                 add_lang = "PANARA", verbose = FALSE)
    df <- utils::read.csv("data.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
    df$full_phrases_EN[1] <- "My own <i>Paubrasilia</i> text."
    df$full_phrases_ADD_LANGUAGE[1] <- "Texto em panar\u00e1"
    .write_fixture_csv("data.csv", df)

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
    # The manually edited phrase is used as is (also in the spoken text)
    testthat::expect_match(html, "My own Paubrasilia text.", fixed = TRUE)
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
    .write_fixture_csv("data.csv", .fixture_phrases_df(df))
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
      .read_species_data = function(data_path, verbose) .fixture_species_df()[1, ],
      .stored_phrases = function(df, printed_lang, add_lang, verbose) {
        list(printed_lang = printed_lang,
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

testthat::test_that("arboretum_labels requires the phrases created by arboretum_phrases", {
  .with_tmp_wd("labels-no-phrases", {
    .write_fixture_csv("data.csv")
    testthat::expect_error(
      aRboretum::arboretum_labels(data_path = "data.csv", printed_lang = "en",
                                  verbose = FALSE, dir = "labels"),
      "Run arboretum_phrases\\(\\) on your data file first"
    )
    testthat::expect_false(dir.exists(file.path("labels", "__maps")))
  })
})

testthat::test_that("arboretum_labels warns about species without a stored phrase", {
  testthat::skip_if_not_installed("geobr")
  testthat::local_mocked_bindings(read_state = function(...) .fixture_br_states(),
                                  .package = "geobr")

  .with_tmp_wd("labels-empty-phrase", {
    df <- .fixture_phrases_df()
    df$full_phrases_EN[2] <- "  "
    .write_fixture_csv("data.csv", df)

    warnings <- testthat::capture_warnings(
      out <- aRboretum::arboretum_labels(data_path = "data.csv", printed_lang = "en",
                                         verbose = FALSE, dir = "labels")
    )
    testthat::expect_true(any(grepl("No stored phrase in 'full_phrases_EN' for: Coffea arabica",
                                    warnings)))
    testthat::expect_length(out, 2)
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
