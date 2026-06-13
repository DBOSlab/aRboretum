# file: tests/testthat/test-arboretum_labels.R

testthat::test_that("arboretum_labels adds custom language when full_phrases_ADD_LANGUAGE is present", {
  testthat::skip_if_not_installed("aRboretum")
  testthat::skip_if_not_installed("geobr")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    full_phrases_ADD_LANGUAGE = c("Texto Panará 1", "Texto Panará 2"),
    stringsAsFactors = FALSE
  )

  calls <- list()

  phrase_stub <- function(df, dict, lang, verbose) {
    stats::setNames(
      as.list(paste("Phrase", toupper(lang), "for", df$taxonName)),
      df$taxonName
    )
  }

  gen_stub <- function(species_data,
                       phrases,
                       world,
                       br_states,
                       printed_lang,
                       path_to_logo,
                       logo_url,
                       output_dir,
                       audio_dir,
                       photo_dir,
                       back_to_index,
                       index_file,
                       verbose) {
    calls[[length(calls) + 1L]] <<- list(
      species = species_data$taxonName[1],
      printed_lang = printed_lang,
      add_phrase = phrases[["PANARA"]][[species_data$taxonName[1]]],
      audio_dir = audio_dir,
      photo_dir = photo_dir
    )
    out <- paste0(gsub("\\s+", "_", species_data$taxonName[1]), ".html")
    writeLines("<html><body>ok</body></html>", file.path(output_dir, out))
    out
  }

  old_wd <- getwd()
  tmp <- tempdir()
  out_dir <- file.path(tmp, "labels-out")
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)
  unlink(out_dir, recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .arg_check_dir = function(x) x,
    .arg_check_printed_lang = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .copy_folder_progress = function(from, to, verbose) invisible(NULL),
    .phrase_generator = phrase_stub,
    .dict = function() list(),
    .get_world_map = function() data.frame(LEVEL3_NAM = character(0), stringsAsFactors = FALSE),
    .generate_species_html = gen_stub,
    .package = "aRboretum"
  )
  testthat::local_mocked_bindings(
    read_state = function(...) data.frame(abbrev_state = character(0), stringsAsFactors = FALSE),
    .package = "geobr"
  )

  out <- aRboretum::arboretum_labels(
    data_path = "fake.xlsx",
    printed_lang = c("pt", "en"),
    add_lang = "PANARA",
    audio_dir = "missing_audios",
    photo_dir = "missing_photos",
    verbose = FALSE,
    dir = out_dir
  )

  testthat::expect_length(out, 2)
  testthat::expect_equal(length(calls), 2)
  testthat::expect_true(all(vapply(calls, function(x) "PANARA" %in% x$printed_lang, logical(1))))
  testthat::expect_identical(calls[[1]]$add_phrase, "Texto Panará 1")
  testthat::expect_identical(calls[[2]]$add_phrase, "Texto Panará 2")
  testthat::expect_true(all(file.exists(file.path(out_dir, out))))
})

testthat::test_that("arboretum_labels does not add custom language when custom phrases are absent", {
  testthat::skip_if_not_installed("aRboretum")
  testthat::skip_if_not_installed("geobr")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    full_phrases_ADD_LANGUAGE = "   ",
    stringsAsFactors = FALSE
  )

  seen_printed_lang <- NULL

  phrase_stub <- function(df, dict, lang, verbose) {
    stats::setNames(as.list(paste("Phrase", lang)), df$taxonName)
  }

  gen_stub <- function(species_data,
                       phrases,
                       world,
                       br_states,
                       printed_lang,
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
    .phrase_generator = phrase_stub,
    .dict = function() list(),
    .get_world_map = function() data.frame(LEVEL3_NAM = character(0), stringsAsFactors = FALSE),
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

testthat::test_that("create_logo_tag handles NULL, missing, valid, and unsupported files", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_null(aRboretum:::.create_logo_tag(NULL, NULL))

  testthat::expect_error(
    aRboretum:::.create_logo_tag("missing-logo.png", NULL),
    "Logo file not found"
  )

  svg_file <- tempfile(fileext = ".svg")
  writeLines(
    c(
      '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
      '<rect width="10" height="10" /></svg>'
    ),
    svg_file
  )

  tag <- aRboretum:::.create_logo_tag(svg_file, "https://example.org")
  rendered <- htmltools::renderTags(tag)$html
  testthat::expect_match(rendered, "<a")
  testthat::expect_match(rendered, "https://example.org", fixed = TRUE)
  testthat::expect_match(rendered, "data:image/svg+xml;base64,", fixed = TRUE)

  txt_file <- tempfile(fileext = ".txt")
  writeLines("plain text", txt_file)
  testthat::expect_error(
    aRboretum:::.create_logo_tag(txt_file, NULL),
    "Unsupported logo format"
  )
})

testthat::test_that("build_html_page renders custom language button, back link, maps, and slideshow", {
  testthat::skip_if_not_installed("aRboretum")

  page <- aRboretum:::.build_html_page(
    species_name = "Paubrasilia echinata",
    family_name = "Fabaceae",
    authorship = "Lam.",
    logo_tag = htmltools::tags$img(src = "logo.png"),
    package_logos = list(htmltools::tags$img(src = "pkg.png")),
    js_printed_labels = jsonlite::toJSON(list(en = list(back_index = "Back to main page")), auto_unbox = TRUE),
    js_visible_distributions = jsonlite::toJSON(list(en = "English text"), auto_unbox = TRUE),
    js_spoken_texts = jsonlite::toJSON(list(en = "Spoken text"), auto_unbox = TRUE),
    js_voice_langs = jsonlite::toJSON(list(en = "en-US"), auto_unbox = TRUE),
    js_rate = jsonlite::toJSON(1, auto_unbox = TRUE),
    js_pitch = jsonlite::toJSON(1, auto_unbox = TRUE),
    js_volume = jsonlite::toJSON(1, auto_unbox = TRUE),
    js_initial_lang = jsonlite::toJSON("en", auto_unbox = TRUE),
    js_audio_files = jsonlite::toJSON(list(en = NULL), auto_unbox = TRUE),
    printed_lang = c("en", "PANARA"),
    top_logos = list(htmltools::tags$img(src = "status.png")),
    world_map_html = htmltools::tags$div("world map"),
    brazil_map_html = htmltools::tags$div("brazil map"),
    slideshow_tag = htmltools::tags$div("photos block"),
    back_to_index = TRUE,
    index_file = "index.html"
  )

  html <- htmltools::renderTags(page)$html

  testthat::expect_match(html, "Paubrasilia echinata", fixed = TRUE)
  testthat::expect_match(html, 'data-lang="PANARA"', fixed = TRUE)
  testthat::expect_match(html, ">PANARA<")
  testthat::expect_match(html, "index.html", fixed = TRUE)
  testthat::expect_match(html, "world map", fixed = TRUE)
  testthat::expect_match(html, "brazil map", fixed = TRUE)
  testthat::expect_match(html, "photos block", fixed = TRUE)
})

testthat::test_that("generate_species_html builds JSON payloads with extra text and custom language", {
  testthat::skip_if_not_installed("aRboretum")

  species_data <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    scientificNameAuthorship = "Lam.",
    plant_uses_EN = "Wood use",
    free_notes_EN = "Endemic species",
    IUCN.status = NA_character_,
    FFB.url = "https://example.org/ffb",
    POWO.url = "https://example.org/powo",
    botanical_country = NA_character_,
    introduced_to = NA_character_,
    FFB.stateProvince = NA_character_,
    stringsAsFactors = FALSE
  )

  phrases <- list(
    en = list("Paubrasilia echinata" = "<i>Tree species</i>"),
    PANARA = list("Paubrasilia echinata" = "Texto Panará")
  )

  captured <- new.env(parent = emptyenv())
  captured$visible <- NULL
  captured$spoken <- NULL
  captured$langs <- NULL

  build_stub <- function(species_name,
                         family_name,
                         authorship,
                         logo_tag,
                         package_logos,
                         js_printed_labels,
                         js_visible_distributions,
                         js_spoken_texts,
                         js_voice_langs,
                         js_rate,
                         js_pitch,
                         js_volume,
                         js_initial_lang,
                         js_audio_files,
                         printed_lang,
                         top_logos,
                         world_map_html,
                         brazil_map_html,
                         slideshow_tag,
                         back_to_index,
                         index_file) {
    captured$visible <- jsonlite::fromJSON(js_visible_distributions)
    captured$spoken <- jsonlite::fromJSON(js_spoken_texts)
    captured$langs <- printed_lang
    htmltools::tags$html(htmltools::tags$body("ok"))
  }

  out_dir <- file.path(tempdir(), "species-html-out")
  unlink(out_dir, recursive = TRUE, force = TRUE)
  dir.create(out_dir, recursive = TRUE)

  testthat::local_mocked_bindings(
    .mk_map_dist = function(df_sp, world, br_states) list(world_map = NULL, brazil_map = NULL),
    .create_logo_tag = function(path_to_logo, logo_url) NULL,
    .check_personal_audio = function(species_data, printed_lang, audio_dir) {
      stats::setNames(as.list(rep(list(NULL), length(printed_lang))), printed_lang)
    },
    .check_species_photos = function(species_data, photo_dir) character(0),
    .build_html_page = build_stub,
    .package = "aRboretum"
  )

  out <- aRboretum:::.generate_species_html(
    species_data = species_data,
    phrases = phrases,
    world = data.frame(),
    br_states = data.frame(),
    printed_lang = c("en", "PANARA"),
    path_to_logo = NULL,
    logo_url = NULL,
    output_dir = out_dir,
    audio_dir = NULL,
    photo_dir = NULL,
    back_to_index = TRUE,
    index_file = "index.html",
    verbose = FALSE
  )

  testthat::expect_true(file.exists(file.path(out_dir, out)))
  testthat::expect_identical(captured$langs, c("en", "PANARA"))
  testthat::expect_match(captured$visible$en, "Tree species")
  testthat::expect_match(captured$visible$en, "Wood use", fixed = TRUE)
  testthat::expect_match(captured$visible$en, "Endemic species", fixed = TRUE)
  testthat::expect_identical(captured$visible$PANARA, "Texto Panará")
  testthat::expect_match(captured$spoken$en, "Overview of the species", fixed = TRUE)
  testthat::expect_match(captured$spoken$en, "Wood use", fixed = TRUE)
})

testthat::test_that("mk_map_dist returns world-only or world-plus-brazil branches", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::local_mocked_bindings(
    .get_pr_ab_world = function(df_sp, world) "world-data",
    .ggplot_map = function(world_plant) paste("world-plot", world_plant),
    .get_pr_ab_br = function(df_sp, br_states) "br-data",
    .ggplot_map_br = function(br_plant, df_sp) paste("br-plot", br_plant),
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
  testthat::expect_identical(both_maps$brazil_map, "br-plot br-data")
})
