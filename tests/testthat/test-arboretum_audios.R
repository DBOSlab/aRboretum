# file: tests/testthat/test-arboretum_audios.R

testthat::test_that("arboretum_audios creates default language folders and HTML guide", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    plant_uses_EN = c("Wood use", "Palm heart"),
    free_notes_EN = c("Endemic species", "Atlantic Forest"),
    stringsAsFactors = FALSE
  )

  phrase_stub <- function(df, lang, dict, verbose) {
    stats::setNames(
      as.list(paste("Phrase", toupper(lang), "for", df$taxonName)),
      df$taxonName
    )
  }

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  if (dir.exists("arboretum_audios")) {
    unlink("arboretum_audios", recursive = TRUE, force = TRUE)
  }

  testthat::local_mocked_bindings(
    .arg_check_printed_lang = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .phrase_generator = phrase_stub,
    .dict = function() list(),
    .package = "aRboretum"
  )

  aRboretum::arboretum_audios(
    data_path = "fake.xlsx",
    printed_lang = c("pt", "en", "fr", "es"),
    verbose = FALSE
  )

  testthat::expect_true(dir.exists("arboretum_audios"))

  folders <- list.dirs("arboretum_audios", recursive = FALSE, full.names = FALSE)
  testthat::expect_length(folders, nrow(df) * 4)

  testthat::expect_true(file.exists("arboretum_audios/__personal_audio_recording_guide.html"))

  html <- paste(readLines("arboretum_audios/__personal_audio_recording_guide.html", warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "Personal Audio Recording Guide")
  testthat::expect_match(html, "Paubrasilia echinata")
  testthat::expect_match(html, "Euterpe edulis")
  testthat::expect_match(html, "Phrase EN for Paubrasilia echinata")
})

testthat::test_that("arboretum_audios creates extra folders for add_lang", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    stringsAsFactors = FALSE
  )

  phrase_stub <- function(df, lang, dict, verbose) {
    stats::setNames(as.list(paste("Phrase", lang)), df$taxonName)
  }

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  if (dir.exists("arboretum_audios")) {
    unlink("arboretum_audios", recursive = TRUE, force = TRUE)
  }

  testthat::local_mocked_bindings(
    .arg_check_printed_lang = function(x) x,
    .read_species_data = function(data_path, verbose) df,
    .phrase_generator = phrase_stub,
    .dict = function() list(),
    .package = "aRboretum"
  )

  aRboretum::arboretum_audios(
    data_path = "fake.xlsx",
    printed_lang = c("pt", "en"),
    add_lang = "PANARA",
    verbose = FALSE
  )

  folders <- list.dirs("arboretum_audios", recursive = FALSE, full.names = FALSE)
  testthat::expect_true(any(grepl("_PANARA$", folders)))
  testthat::expect_true(any(grepl("_PT$", folders)))
  testthat::expect_true(any(grepl("_EN$", folders)))
})

testthat::test_that("get_extra_phrase_text combines plant uses and notes with normalization", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Species a",
    family = "Family a",
    plant_uses_EN = "Useful wood",
    free_notes_EN = " Important note\nwith spaces ",
    stringsAsFactors = FALSE
  )

  out <- aRboretum:::.get_extra_phrase_text(df, row_index = 1, lang = "en")

  testthat::expect_identical(out, "Useful wood. Important note with spaces.")
})

testthat::test_that("get_extra_phrase_text returns NULL when extra columns are absent or empty", {
  testthat::skip_if_not_installed("aRboretum")

  df_empty <- data.frame(
    taxonName = "Species a",
    family = "Family a",
    plant_uses_EN = NA_character_,
    free_notes_EN = "   ",
    stringsAsFactors = FALSE
  )

  df_missing <- data.frame(
    taxonName = "Species a",
    family = "Family a",
    stringsAsFactors = FALSE
  )

  testthat::expect_null(aRboretum:::.get_extra_phrase_text(df_empty, 1, "en"))
  testthat::expect_null(aRboretum:::.get_extra_phrase_text(df_missing, 1, "en"))
})

testthat::test_that("text helper functions normalize, escape, and slugify correctly", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_true(aRboretum:::.is_missing_text(NULL))
  testthat::expect_true(aRboretum:::.is_missing_text(NA_character_))
  testthat::expect_true(aRboretum:::.is_missing_text("   "))
  testthat::expect_false(aRboretum:::.is_missing_text("text"))

  testthat::expect_identical(
    aRboretum:::.normalize_text(" line  one \n\t line two ", ensure_period = TRUE),
    "line one line two."
  )

  testthat::expect_true(is.na(aRboretum:::.normalize_text("   ")))

  testthat::expect_identical(
    aRboretum:::.escape_html('a & <b> "c"'),
    "a &amp; &lt;b&gt; &quot;c&quot;"
  )

  testthat::expect_identical(
    aRboretum:::.slugify("Árvore do Brasil!"),
    "arvore-do-brasil"
  )
})

testthat::test_that("language helper functions return expected labels and suffixes", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_identical(aRboretum:::.lang_label("pt"), "Português")
  testthat::expect_identical(aRboretum:::.lang_label("en"), "English")
  testthat::expect_identical(aRboretum:::.lang_suffix("fr"), "FR")
  testthat::expect_identical(aRboretum:::.lang_suffix("es"), "ES")
  testthat::expect_null(aRboretum:::.lang_label("PANARA"))
  testthat::expect_null(aRboretum:::.lang_suffix("PANARA"))
})

testthat::test_that("build_language_buttons returns empty for one language and active state for many", {
  testthat::skip_if_not_installed("aRboretum")

  labels <- c(en = "English", pt = "Português", fr = "Français")

  testthat::expect_identical(
    aRboretum:::.build_language_buttons("en", "en", labels),
    ""
  )

  out <- aRboretum:::.build_language_buttons(c("en", "pt"), "pt", labels)
  testthat::expect_match(out, 'data-lang="en"')
  testthat::expect_match(out, 'data-lang="pt"')
  testthat::expect_match(out, 'class="lang-btn active"')
})

testthat::test_that("build_species_index includes taxon, family, and searchable extra text", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    plant_uses_EN = c("Wood use", NA),
    free_notes_EN = c("Endemic", "Palm species"),
    stringsAsFactors = FALSE
  )

  out <- aRboretum:::.build_species_index(df)

  testthat::expect_match(out, 'href="#paubrasilia-echinata"')
  testthat::expect_match(out, 'href="#euterpe-edulis"')
  testthat::expect_match(out, "wood use", ignore.case = TRUE)
  testthat::expect_match(out, "palm species", ignore.case = TRUE)
})

testthat::test_that("build_species_cards renders visible and hidden language blocks with extra text", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    plant_uses_EN = "Wood use",
    free_notes_EN = "Endemic",
    stringsAsFactors = FALSE
  )

  html_phrases <- list(
    en = list("Paubrasilia echinata" = "<i>Tree species</i>"),
    pt = list("Paubrasilia echinata" = "Espécie arbórea")
  )

  out <- aRboretum:::.build_species_cards(
    df = df,
    printed_lang = c("en", "pt"),
    html_phrases = html_phrases,
    initial_lang = "en"
  )

  testthat::expect_match(out, '<article class="species-card"')
  testthat::expect_match(out, 'data-lang="en"')
  testthat::expect_match(out, 'data-lang="pt"')
  testthat::expect_match(out, 'lang-content hidden')
  testthat::expect_match(out, "Tree species")
  testthat::expect_match(out, "Wood use\\. Endemic\\.")
})

testthat::test_that("save_phrase_html writes a complete HTML file", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    plant_uses_EN = "Wood use",
    free_notes_EN = "Endemic species",
    stringsAsFactors = FALSE
  )

  audio_ui_strings <- list(
    en = list(
      html_lang = "en",
      title = "Personal Audio Recording Guide",
      subtitle = "Use this file to record species audios.",
      search_placeholder = "Search species...",
      index_title = "Index",
      no_results = "No matches",
      back_to_top = "Back to top",
      footer_note = "Footer note",
      family = "Family",
      generated_with = "Generated with"
    ),
    pt = list(
      html_lang = "pt",
      title = "Guia",
      subtitle = "Use este arquivo.",
      search_placeholder = "Pesquisar...",
      index_title = "Índice",
      no_results = "Sem resultados",
      back_to_top = "Voltar ao topo",
      footer_note = "Nota",
      family = "Família",
      generated_with = "Gerado com"
    )
  )

  html_phrases <- list(
    en = list("Paubrasilia echinata" = "English phrase"),
    pt = list("Paubrasilia echinata" = "Frase em português")
  )

  out_file <- file.path(tempdir(), "arboretum_audio_test.html")
  if (file.exists(out_file)) {
    unlink(out_file)
  }

  aRboretum:::.save_phrase_html(
    df = df,
    audio_ui_strings = audio_ui_strings,
    lang_button_label = c(en = "English", pt = "Português"),
    printed_lang = c("en", "pt"),
    html_phrases = html_phrases,
    output_path = out_file,
    verbose = FALSE
  )

  testthat::expect_true(file.exists(out_file))

  html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "<!DOCTYPE html>", fixed = TRUE)
  testthat::expect_match(html, "Personal Audio Recording Guide")
  testthat::expect_match(html, "Paubrasilia echinata")
  testthat::expect_match(html, "English phrase")
  testthat::expect_match(html, "Frase em português")
  testthat::expect_match(html, "Generated with")
  testthat::expect_match(html, "aRboretum")
})
