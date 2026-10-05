# file: tests/testthat/test-auxiliary_fxns.R

testthat::test_that("get_accepted_name_id returns accepted ID for synonyms and original ID otherwise", {
  testthat::skip_if_not_installed("aRboretum")

  taxon_data <- data.frame(
    id = c(1, 2, 3),
    taxonomicStatus = c("SINONIMO", "ACEITO", "ACEITO"),
    acceptedNameUsageID = c(2, NA, NA),
    taxonName = c("Old species", "Accepted species", "Another species"),
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    out_syn <- aRboretum:::.get_accepted_name_id(
      taxon_id = 1,
      taxon_data = taxon_data,
      verbose = TRUE
    ),
    "Retrieving data from the currently accepted name"
  )
  out_acc <- aRboretum:::.get_accepted_name_id(
    taxon_id = 2,
    taxon_data = taxon_data,
    verbose = FALSE
  )

  testthat::expect_identical(out_syn, 2)
  testthat::expect_identical(out_acc, 2)
})

testthat::test_that("read_species_data reads csv and xlsx and validates required columns", {
  testthat::skip_if_not_installed("aRboretum")
  testthat::skip_if_not_installed("openxlsx")

  df <- data.frame(
    family = "Fabaceae",
    taxonName = "Paubrasilia echinata",
    scientificNameAuthorship = "Lam.",
    FFB.vernacularName = "pau-brasil",
    country = "Brazil",
    endemism = "Endemic",
    FFB.establishmentMeans = "Native",
    FFB.stateProvince = "Bahia",
    FFB.phytogeographicDomain = "Atlantic Forest",
    FFB.vegetationType = "Rainforest",
    botanical_country = "Brazil",
    introduced_to = NA_character_,
    IUCN.status = "EN",
    plant_uses_EN = "Wood use",
    plant_uses_PT = "Uso madeireiro",
    plant_uses_ES = "Uso maderero",
    plant_uses_FR = "Usage du bois",
    free_notes_EN = "Important species",
    free_notes_PT = "Espécie importante",
    free_notes_ES = "Especie importante",
    free_notes_FR = "Espèce importante",
    POWO.url = "https://powo.example",
    FFB.url = "https://ffb.example",
    stringsAsFactors = FALSE
  )

  csv_file <- tempfile(fileext = ".csv")
  xlsx_file <- tempfile(fileext = ".xlsx")
  bad_file <- tempfile(fileext = ".txt")

  utils::write.csv(df, csv_file, row.names = FALSE)
  openxlsx::write.xlsx(df, xlsx_file, rowNames = FALSE)
  writeLines("plain text", bad_file)

  testthat::expect_message(
    out_csv <- aRboretum:::.read_species_data(csv_file, verbose = TRUE),
    "Reading CSV file"
  )
  testthat::expect_message(
    out_xlsx <- aRboretum:::.read_species_data(xlsx_file, verbose = TRUE),
    "Reading Excel file"
  )

  testthat::expect_identical(out_csv$taxonName, df$taxonName)
  testthat::expect_identical(out_xlsx$family, df$family)

  testthat::expect_error(
    aRboretum:::.read_species_data(NULL),
    "'data_path' must be provided."
  )
  testthat::expect_error(
    aRboretum:::.read_species_data(c(csv_file, xlsx_file)),
    "'data_path' must be a single character string."
  )
  testthat::expect_error(
    aRboretum:::.read_species_data("missing-file.csv"),
    "File not found"
  )
  testthat::expect_error(
    aRboretum:::.read_species_data(bad_file),
    "Unsupported file format"
  )

  df_missing <- df[, setdiff(names(df), "FFB.url")]
  bad_csv <- tempfile(fileext = ".csv")
  utils::write.csv(df_missing, bad_csv, row.names = FALSE)

  testthat::expect_error(
    aRboretum:::.read_species_data(bad_csv, verbose = FALSE),
    "Missing required columns: FFB.url"
  )
})

testthat::test_that("read_species_data reads empty columns as character and adds no phrase columns", {
  df <- data.frame(
    family = "Fabaceae", taxonName = "Paubrasilia echinata",
    scientificNameAuthorship = "Lam.", FFB.vernacularName = NA, country = "Brazil",
    endemism = "Endemic", FFB.establishmentMeans = "Native", FFB.stateProvince = "BA",
    FFB.phytogeographicDomain = "Atlantic Forest", FFB.vegetationType = NA,
    botanical_country = "Brazil Northeast", introduced_to = NA, IUCN.status = NA,
    plant_uses_EN = NA, plant_uses_PT = NA, plant_uses_ES = NA, plant_uses_FR = NA,
    free_notes_EN = NA, free_notes_PT = NA, free_notes_ES = NA, free_notes_FR = NA,
    POWO.url = NA, FFB.url = NA,
    stringsAsFactors = FALSE
  )
  csv_file <- tempfile(fileext = ".csv")
  utils::write.csv(df, csv_file, row.names = FALSE)

  out <- aRboretum:::.read_species_data(csv_file, verbose = FALSE)

  testthat::expect_type(out$FFB.vegetationType, "character")
  testthat::expect_false(any(aRboretum:::.phrase_cols() %in% names(out)))
})

testthat::test_that("save_csv writes a UTF-8 csv without row names", {
  dir <- file.path(tempdir(), "aux-save-csv")
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  df <- data.frame(taxonName = "Euterpe edulis", FFB.vernacularName = "juçara",
                   stringsAsFactors = FALSE)
  aRboretum:::.save_csv(df, verbose = FALSE, filename = "out", dir = dir)

  out <- utils::read.csv(file.path(dir, "out.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  testthat::expect_identical(names(out), names(df))
  testthat::expect_identical(out$FFB.vernacularName, "juçara")
})

testthat::test_that("generate_phrases fills empty phrases, keeps stored ones and can overwrite", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    free_notes_FR = NA,
    POWO.url = NA,
    full_phrases_EN = c("Manual phrase.", "  "),
    stringsAsFactors = FALSE
  )
  seen <- list()

  testthat::local_mocked_bindings(
    .dict = function() list(),
    .phrase_generator = function(df, dict, lang, verbose) {
      seen[[lang]] <<- df$taxonName
      stats::setNames(as.list(paste("Generated", lang, df$taxonName)), df$taxonName)
    },
    .package = "aRboretum"
  )

  out <- aRboretum:::.generate_phrases(df, printed_lang = c("en", "pt"), verbose = FALSE)

  # Only rows without a stored phrase are generated
  testthat::expect_identical(seen$en, "Euterpe edulis")
  testthat::expect_identical(seen$pt, c("Paubrasilia echinata", "Euterpe edulis"))
  testthat::expect_identical(out$n_generated, 3L)
  testthat::expect_identical(out$df$full_phrases_EN,
                             c("Manual phrase.", "Generated en Euterpe edulis"))
  testthat::expect_identical(out$df$full_phrases_PT,
                             c("Generated pt Paubrasilia echinata", "Generated pt Euterpe edulis"))

  # All phrase columns exist, placed after the annotation columns
  testthat::expect_identical(
    names(out$df),
    c("taxonName", "family", "free_notes_FR", aRboretum:::.phrase_cols(), "POWO.url")
  )
  testthat::expect_true(all(is.na(out$df$full_phrases_FR)))

  # A second pass with everything stored generates nothing
  testthat::expect_message(
    again <- aRboretum:::.generate_phrases(out$df, printed_lang = c("en", "pt"), verbose = TRUE),
    "Keeping the stored phrases for language: EN"
  )
  testthat::expect_identical(again$n_generated, 0L)

  # overwrite = TRUE regenerates every phrase
  seen <- list()
  forced <- aRboretum:::.generate_phrases(out$df, printed_lang = "en", overwrite = TRUE,
                                          verbose = FALSE)
  testthat::expect_identical(seen$en, c("Paubrasilia echinata", "Euterpe edulis"))
  testthat::expect_identical(forced$df$full_phrases_EN[1], "Generated en Paubrasilia echinata")
})

testthat::test_that("add_phrase_cols appends the columns when there is no annotation column", {
  out <- aRboretum:::.add_phrase_cols(data.frame(taxonName = "a", family = "b"))
  testthat::expect_identical(names(out), c("taxonName", "family", aRboretum:::.phrase_cols()))
})

testthat::test_that("stored_phrases reads phrases, adds the custom language and never generates", {
  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    full_phrases_EN = c("Phrase 1", NA),
    full_phrases_PT = c("Frase 1", "Frase 2"),
    full_phrases_ADD_LANGUAGE = c("Texto 1", NA),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    .phrase_generator = function(...) stop("phrases must not be generated"),
    .package = "aRboretum"
  )

  testthat::expect_warning(
    out <- aRboretum:::.stored_phrases(df, printed_lang = c("en", "pt"), add_lang = "PANARA",
                                       verbose = FALSE),
    "No stored phrase in 'full_phrases_EN' for: Euterpe edulis"
  )
  testthat::expect_identical(out$printed_lang, c("en", "pt", "PANARA"))
  testthat::expect_identical(out$html_phrases$en[["Euterpe edulis"]], "")
  testthat::expect_identical(out$html_phrases$pt[["Euterpe edulis"]], "Frase 2")
  testthat::expect_identical(out$html_phrases$PANARA[["Paubrasilia echinata"]], "Texto 1")
  testthat::expect_identical(out$html_phrases$PANARA[["Euterpe edulis"]], "")

  # The custom language is ignored when it has no text
  df$full_phrases_ADD_LANGUAGE <- NA
  out2 <- aRboretum:::.stored_phrases(df, printed_lang = "pt", add_lang = "PANARA",
                                      verbose = FALSE)
  testthat::expect_identical(out2$printed_lang, "pt")

  testthat::expect_error(
    aRboretum:::.stored_phrases(df, printed_lang = c("fr", "es"), verbose = FALSE),
    "full_phrases_FR, full_phrases_ES"
  )
})

testthat::test_that("phrase_generator handles species without Brazilian data and adds IUCN status", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    family = c("Rubiaceae", "Fabaceae"),
    taxonName = c("Coffea arabica", "Paubrasilia echinata"),
    FFB.vernacularName = c(NA, NA),
    country = c("Ethiopia | Kenya", "Brazil"),
    botanical_country = c("Ethiopia | Kenya", "Brazil Northeast"),
    endemism = c("Non-endemic", "Endemic"),
    FFB.establishmentMeans = c(NA, "Native"),
    FFB.stateProvince = c(NA, "BA | PE"),
    FFB.phytogeographicDomain = c(NA, "Atlantic Forest"),
    FFB.vegetationType = c(NA, NA),
    introduced_to = c(NA, NA),
    IUCN.status = c("Endangered (EN)", "EN"),
    stringsAsFactors = FALSE
  )

  out <- aRboretum:::.phrase_generator(df, aRboretum:::.dict(), lang = "en", verbose = FALSE)

  testthat::expect_named(out, df$taxonName)
  testthat::expect_match(out[["Coffea arabica"]], "Rubiaceae family")
  testthat::expect_match(out[["Coffea arabica"]], "endangered \\(EN\\)")
  testthat::expect_match(out[["Paubrasilia echinata"]], "endangered \\(EN\\)")

  out_pt <- aRboretum:::.phrase_generator(df, aRboretum:::.dict(), lang = "pt", verbose = FALSE)
  testthat::expect_match(out_pt[["Coffea arabica"]], "IUCN")
})

testthat::test_that("capitalize formats the first letter only", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_identical(aRboretum:::.capitalize("fabaceae"), "Fabaceae")
  testthat::expect_identical(aRboretum:::.capitalize("eUTERPE"), "Euterpe")
})

testthat::test_that("copy_folder_progress copies recursive contents and validates source", {
  testthat::skip_if_not_installed("aRboretum")

  from_dir <- file.path(tempdir(), "aux-copy-from")
  to_dir <- file.path(tempdir(), "aux-copy-to")
  unlink(from_dir, recursive = TRUE, force = TRUE)
  unlink(to_dir, recursive = TRUE, force = TRUE)

  dir.create(file.path(from_dir, "nested"), recursive = TRUE)
  writeLines("alpha", file.path(from_dir, "a.txt"))
  writeLines("beta", file.path(from_dir, "nested", "b.txt"))

  copied_n <- aRboretum:::.copy_folder_progress(
    from = from_dir,
    to = to_dir,
    overwrite = TRUE,
    verbose = FALSE
  )

  testthat::expect_true(file.exists(file.path(to_dir, basename(from_dir), "a.txt")))
  testthat::expect_true(file.exists(file.path(to_dir, basename(from_dir), "nested", "b.txt")))
  testthat::expect_true(copied_n >= 1)

  testthat::expect_error(
    aRboretum:::.copy_folder_progress(
      from = file.path(tempdir(), "missing-source"),
      to = to_dir,
      verbose = FALSE
    ),
    "Source folder does not exist"
  )
})

testthat::test_that("add_genus_curiosity_notes appends rarity and diversity notes across languages", {
  testthat::skip_if_not_installed("aRboretum")

  result_merged <- data.frame(
    FFB.genusRichness = c(1, 3, 6, 0, NA),
    free_notes_EN = c("", "Existing EN", "", "", ""),
    free_notes_PT = c("", "Existing PT", "", "", ""),
    free_notes_ES = c("", "Existing ES", "", "", ""),
    free_notes_FR = c("", "Existing FR", "", "", ""),
    stringsAsFactors = FALSE
  )

  dict_stub <- data.frame(
    key = c(
      "only_species_of_genus",
      "one_of_n_species_of_genus",
      "genus_with_highest_diversity",
      "one_of_genera_with_highest_diversity"
    ),
    en = c(
      "The only species of its genus.",
      "One of {n} species in its genus.",
      "Its genus has the highest diversity.",
      "One of the genera with the highest diversity."
    ),
    pt = c("PT only", "PT one of {n}", "PT top genus", "PT one of top genera"),
    es = c("ES only", "ES one of {n}", "ES top genus", "ES one of top genera"),
    fr = c("FR only", "FR one of {n}", "FR top genus", "FR one of top genera"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .dict = function() dict_stub,
    .package = "aRboretum"
  )

  out <- aRboretum:::.add_genus_curiosity_notes(
    result_merged = result_merged,
    max_diversity = 6,
    n_max_genera = 1
  )

  testthat::expect_match(out$free_notes_EN[1], "only species", ignore.case = TRUE)
  testthat::expect_match(out$free_notes_EN[2], "One of 3 species", fixed = TRUE)
  testthat::expect_match(out$free_notes_EN[2], "Existing EN", fixed = TRUE)
  testthat::expect_match(out$free_notes_EN[3], "highest diversity", ignore.case = TRUE)
  testthat::expect_identical(out$free_notes_EN[4], "")
  testthat::expect_identical(out$free_notes_EN[5], "")
  testthat::expect_match(out$free_notes_PT[2], "PT one of 3", fixed = TRUE)
  testthat::expect_match(out$free_notes_FR[3], "FR top genus", fixed = TRUE)
})

testthat::test_that("get_lab returns named vectors and flattens nested list cells", {
  testthat::skip_if_not_installed("aRboretum")

  dict <- list(
    key = c("hello", "nested"),
    en = list("Hello", list("A", "B")),
    pt = list("Olá", list("X", "Y")),
    es = list("Hola", list("M", "N")),
    fr = list("Bonjour", list("P", "Q"))
  )

  out <- aRboretum:::.get_lab("en", dict)

  testthat::expect_identical(names(out), c("hello", "nested"))
  testthat::expect_identical(out$hello, "Hello")
  testthat::expect_identical(out$nested, c("A", "B"))
})

testthat::test_that("tr_dict and tr_dict_vec translate keys and fall back to English", {
  testthat::skip_if_not_installed("aRboretum")

  dict <- data.frame(
    key = c("family", "belongs_to"),
    en = c("family", "belongs to"),
    pt = c("família", "pertence à"),
    es = c("familia", "pertenece a"),
    fr = c("famille", "appartient à"),
    stringsAsFactors = FALSE
  )

  testthat::expect_identical(aRboretum:::.tr_dict("family", "pt", dict), "família")
  testthat::expect_identical(aRboretum:::.tr_dict("belongs_to", "XX", dict), "belongs to")
  testthat::expect_identical(
    aRboretum:::.tr_dict_vec(c("family", "belongs_to"), "fr", dict),
    c("famille", "appartient à")
  )
})

testthat::test_that("convert_acronym_br_state converts acronyms and names and preserves unknown values", {
  testthat::skip_if_not_installed("aRboretum")

  out <- aRboretum:::.convert_acronym_br_state(
    c("BA", "Sao Paulo", "Pará", "XX")
  )

  testthat::expect_identical(out[1], "Bahia")
  testthat::expect_identical(out[2], "São Paulo")
  testthat::expect_identical(out[3], "Pará")
  testthat::expect_identical(out[4], "XX")
})


# NOT WORKING TESTS ------------------------------------------------------------

# testthat::test_that("botdiv_to_countries maps full and trimmed botanical divisions", {
#   testthat::skip_if_not_installed("aRboretum")
#
#   botregions <- data.frame(
#     botanical_division = c("Brazil North", "Venezuela", "Very Long Botanical Region Name"),
#     country = c("Brazil", "Venezuela", "Colombia"),
#     stringsAsFactors = FALSE
#   )
#
#   pkg <- as.environment("package:aRboretum")
#   old_botregions <- if (exists("botregions", envir = pkg, inherits = FALSE)) get("botregions", envir = pkg) else NULL
#   had_botregions <- exists("botregions", envir = pkg, inherits = FALSE)
#   assign("botregions", botregions, envir = pkg)
#   withr::defer({
#     if (had_botregions) {
#       assign("botregions", old_botregions, envir = pkg)
#     } else if (exists("botregions", envir = pkg, inherits = FALSE)) {
#       rm("botregions", envir = pkg)
#     }
#   })
#
#   x1 <- c("Brazil North | Venezuela")
#   x2 <- c("Very Long Botanical R")
#
#   out1 <- aRboretum:::.botdiv_to_countries(x1, 1)
#   out2 <- aRboretum:::.botdiv_to_countries(x2, 1)
#   out3 <- aRboretum:::.botdiv_to_countries(c(NA_character_), 1)
#   out4 <- aRboretum:::.botdiv_to_countries(c(""), 1)
#
#   testthat::expect_identical(out1, "Brazil | Venezuela")
#   testthat::expect_identical(out2, "Colombia")
#   testthat::expect_true(is.na(out3))
#   testthat::expect_true(is.na(out4))
# })
#
# testthat::test_that("save_csv and save_xlsx create directory and write files", {
#   testthat::skip_if_not_installed("aRboretum")
#   testthat::skip_if_not_installed("openxlsx")
#
#   df <- data.frame(
#     taxonName = "Paubrasilia echinata",
#     family = "Fabaceae",
#     stringsAsFactors = FALSE
#   )
#
#   out_dir <- file.path(tempdir(), "aux-save-test")
#   unlink(out_dir, recursive = TRUE, force = TRUE)
#
#   testthat::expect_message(
#     aRboretum:::.save_csv(df, verbose = TRUE, filename = "species", dir = out_dir),
#     "Writing the csv-formatted spreadsheet"
#   )
#   testthat::expect_true(file.exists(file.path(out_dir, "species.csv")))
#
#   testthat::expect_message(
#     aRboretum:::.save_xlsx(df, verbose = TRUE, filename = "species", dir = out_dir),
#     "Writing the xlsx-formatted spreadsheet"
#   )
#   testthat::expect_true(file.exists(file.path(out_dir, "species.xlsx")))
# })

# Phrase generator branches ---------------------------------------------------------

.phrase_df <- function(...) {
  base <- list(
    family = "Fabaceae", taxonName = "Testus specius", FFB.vernacularName = NA,
    country = "Brazil", botanical_country = "Brazil Northeast", endemism = NA,
    FFB.establishmentMeans = NA, FFB.stateProvince = "BA", FFB.phytogeographicDomain = NA,
    FFB.vegetationType = NA, introduced_to = NA, IUCN.status = NA
  )
  args <- list(...)
  base[names(args)] <- args
  as.data.frame(base, stringsAsFactors = FALSE)
}

.phrase <- function(df, lang = "en") {
  aRboretum:::.phrase_generator(df, aRboretum:::.dict(), lang = lang, verbose = FALSE)[[1]]
}

testthat::test_that("phrase_generator describes endemism across several botanical countries", {
  testthat::skip_if_not_installed("aRboretum")

  out <- .phrase(.phrase_df(endemism = "Endemic", country = "Brazil",
                            botanical_country = "Brazil Northeast | Brazil Southeast"))
  testthat::expect_match(out, "This species is endemic to Brazil.", fixed = TRUE)
})

testthat::test_that("phrase_generator explains naturalized species", {
  testthat::skip_if_not_installed("aRboretum")

  out <- .phrase(.phrase_df(endemism = "Non-endemic", country = "Brazil | Peru",
                            FFB.establishmentMeans = "Naturalized"))
  expl <- aRboretum:::.tr_dict("cultivated_brazil_explanation", "en", aRboretum:::.dict())
  testthat::expect_match(out, expl, fixed = TRUE)
})

testthat::test_that("phrase_generator covers several biomes, all biomes and vegetation types", {
  testthat::skip_if_not_installed("aRboretum")

  several <- .phrase(.phrase_df(FFB.phytogeographicDomain = "Amazon | Atlantic Forest",
                                FFB.vegetationType = "Carrasco | Floresta Ombrófila"))
  testthat::expect_match(several, "colonizes various habitats such as", fixed = TRUE)
  testthat::expect_match(several, "where it grows mainly in particular vegetation layers such as",
                         fixed = TRUE)

  all_six <- .phrase(.phrase_df(
    FFB.phytogeographicDomain = "Amazon | Atlantic Forest | Caatinga | Cerrado | Pampa | Pantanal",
    FFB.vegetationType = "Carrasco"
  ))
  testthat::expect_match(all_six,
                         aRboretum:::.tr_dict("found_in_every_biome_in_brazil", "en",
                                              aRboretum:::.dict()),
                         fixed = TRUE)
  testthat::expect_match(all_six, "shrubland Carrasco", fixed = TRUE)

  no_biome <- .phrase(.phrase_df(FFB.vegetationType = "Carrasco | Floresta Ciliar ou Galeria"),
                      lang = "pt")
  testthat::expect_match(no_biome, " ou ", fixed = TRUE)

  one_veg <- .phrase(.phrase_df(FFB.vegetationType = "Carrasco"), lang = "fr")
  testthat::expect_match(one_veg, "^<i>Testus specius</i> appartient")
  testthat::expect_match(one_veg, "Elle pousse", fixed = TRUE)
})

testthat::test_that("tr_or_raw keeps names missing from the dictionary", {
  dict <- data.frame(key = c("a", "b"), en = c("Alpha", "Beta"), pt = c("Alfa", NA),
                     es = NA, fr = NA, stringsAsFactors = FALSE)
  testthat::expect_identical(aRboretum:::.tr_or_raw(c("Alpha", "Beta", "Gamma"), "pt", dict),
                             c("Alfa", "Beta", "Gamma"))
})

testthat::test_that("botdiv_to_countries handles truncated and multi-country divisions", {
  testthat::skip_if_not_installed("aRboretum")

  testthat::expect_true(is.na(aRboretum:::.botdiv_to_countries(list(NA), 1)))
  out <- aRboretum:::.botdiv_to_countries(list("Transcaucasus | Central American Pac"), 1)
  testthat::expect_match(out, "Armenia")
  testthat::expect_match(out, "Georgia")
  testthat::expect_no_match(out, "Central American Pac", fixed = TRUE)
})

testthat::test_that("save_csv reports the written file when verbose", {
  dir <- file.path(tempdir(), "aux-save-csv-verbose")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  testthat::expect_message(
    aRboretum:::.save_csv(data.frame(a = 1), verbose = TRUE, filename = "x", dir = dir),
    "Writing the csv-formatted spreadsheet 'x.csv'"
  )
})

# IUCN helpers -------------------------------------------------------------------------

testthat::test_that("get_iucn_status reports progress and handles empty input", {
  testthat::local_mocked_bindings(
    .iucn_from_gbif = function(taxon) "LC",
    .package = "aRboretum"
  )

  testthat::expect_identical(
    aRboretum:::.get_iucn_status(c(NA, NA), source = "gbif", verbose = FALSE),
    c(NA_character_, NA_character_)
  )

  msgs <- testthat::capture_messages(
    out <- aRboretum:::.get_iucn_status("Euterpe edulis", source = "gbif", verbose = TRUE)
  )
  testthat::expect_identical(out, "Least Concern (LC)")
  testthat::expect_true(any(grepl("from GBIF", msgs)))
  testthat::expect_true(any(grepl("IUCN status found for 1 of 1 species", msgs)))
})

testthat::test_that("get_iucn_status requires rredlist for the Red List API", {
  testthat::local_mocked_bindings(
    .has_rredlist = function() FALSE,
    .package = "aRboretum"
  )
  testthat::expect_error(
    aRboretum:::.get_iucn_status("Euterpe edulis", source = "redlist", verbose = FALSE),
    "Package 'rredlist' is required"
  )
})

testthat::test_that("gbif_get parses JSON responses", {
  json <- tempfile(fileext = ".json")
  writeLines('{"usageKey": 5, "matchType": "EXACT"}', json)
  out <- aRboretum:::.gbif_get(json)
  testthat::expect_identical(out$usageKey, 5L)
  testthat::expect_identical(out$matchType, "EXACT")
})

testthat::test_that("iucn_from_redlist returns NA for incomplete names or assessments", {
  testthat::skip_if_not_installed("rredlist")

  testthat::expect_identical(aRboretum:::.iucn_from_redlist("Euterpe", key = "k"), NA_character_)

  responses <- list(
    list(assessments = data.frame()),
    list(assessments = data.frame(latest = FALSE, red_list_category_code = "EN"))
  )
  k <- 0
  testthat::local_mocked_bindings(
    rl_species = function(genus, species, key, ...) {
      k <<- k + 1
      responses[[k]]
    },
    .package = "rredlist"
  )
  testthat::expect_identical(aRboretum:::.iucn_from_redlist("Euterpe edulis", key = "k"),
                             NA_character_)
  testthat::expect_identical(aRboretum:::.iucn_from_redlist("Euterpe edulis", key = "k"),
                             NA_character_)
})
