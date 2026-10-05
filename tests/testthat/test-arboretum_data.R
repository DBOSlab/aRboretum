# file: tests/testthat/test-arboretum_data.R

testthat::test_that("arboretum_data rejects non-binomial names early", {
  testthat::expect_error(
    aRboretum::arboretum_data(
      spp_list = c("Euterpe", "Coffea arabica"),
      save = FALSE,
      verbose = FALSE
    )
  )
})

# Fake FFB DwC-A shared by the arboretum_data() tests
.fake_dwca <- function() {
  list(list(data = list(
    "taxon.txt" = data.frame(
      id = c("id-1", "id-2", "id-3"),
      taxonRank = c("ESPECIE", "ESPECIE", "GENUS"),
      taxonName = c("Euterpe edulis", "Coffea arabica", "Euterpe"),
      taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO"),
      family = c("Arecaceae", "Rubiaceae", "Arecaceae"),
      scientificNameAuthorship = c("Mart.", "L.", ""),
      references = c("ffb/euterpe", "ffb/coffea", NA_character_),
      genus = c("Euterpe", "Coffea", "Euterpe"),
      stringsAsFactors = FALSE
    ),
    "distribution.txt" = data.frame(
      id = c("id-1", "id-2"),
      dataset = c("x", "x"),
      countryCode = c("BR", "BR"),
      establishmentMeans = c("NATIVA", "CULTIVADA"),
      locationID = c("BR-BA", "BR-SP"),
      endemism = c("Não endemica", "Endemica"),
      phytogeographicDomain = c("Mata Atlântica", "Amazônia"),
      stringsAsFactors = FALSE
    ),
    "vernacularname.txt" = data.frame(
      id = c("id-1", "id-1", "id-2"),
      vernacularName = c("juçara", "palmito", "café"),
      stringsAsFactors = FALSE
    ),
    "speciesprofile.txt" = data.frame(
      id = c("id-1", "id-2"),
      vegetationType = c("Cerrado", "Caatinga"),
      stringsAsFactors = FALSE
    )
  )))
}

# Mock every external data source used by arboretum_data()
.local_mock_data_sources <- function(iucn_calls = NULL, env = parent.frame(),
                                     fake_dwca = .fake_dwca(),
                                     accepted_id = function(taxon_id) taxon_id,
                                     extract = NULL) {
  testthat::local_mocked_bindings(
    .get_accepted_name_id = function(taxon_id, taxon_data, verbose) accepted_id(taxon_id),
    .load_wcvp = function() list(),
    .extract_wcvp_data = if (!is.null(extract)) extract else function(result_POWO, sp, i, wcvp) {
      if (identical(sp, "Euterpe edulis")) {
        result_POWO$family[i] <- "Arecaceae"
        result_POWO$taxonName[i] <- "Euterpe edulis"
        result_POWO$scientificNameAuthorship[i] <- "Mart."
        result_POWO$country[i] <- "Brazil | Peru"
        result_POWO$botanical_country[i] <- "Brazil North | Peru"
        result_POWO$introduced_to[i] <- "Cuba"
        result_POWO$references[i] <- "powo/euterpe"
      }
      result_POWO
    },
    .get_iucn_status = function(taxa, verbose = TRUE) {
      if (!is.null(iucn_calls)) iucn_calls$n <- iucn_calls$n + 1L
      ifelse(taxa == "Euterpe edulis", "Least Concern (LC)", NA_character_)
    },
    .add_genus_curiosity_notes = function(result_merged, max_diversity, n_max_genera) {
      result_merged$free_notes_EN <- paste("Genus note for", result_merged$genus)
      result_merged
    },
    .load_ffb = function(ffb_dir, verbose) fake_dwca,
    .package = "aRboretum",
    .env = env
  )
}

testthat::test_that("arboretum_data merges FFB and WCVP data, adds IUCN status and writes csv output", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_csv_test")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  .local_mock_data_sources()

  result <- aRboretum::arboretum_data(
    spp_list = c("Euterpe edulis", "Coffea arabica"),
    verbose = FALSE,
    save = TRUE,
    format = "csv",
    filename = "arboretum_data_test",
    dir = temp_dir
  )

  csv_path <- file.path(temp_dir, "arboretum_data_test.csv")
  testthat::expect_true(file.exists(csv_path))

  # Phrases are only created later, by arboretum_phrases()
  testthat::expect_false(any(grepl("^full_phrases_", names(result))))
  testthat::expect_false(file.exists(file.path(temp_dir, "__phrase_generating_guide.html")))

  # The data reviewing guide is written next to the data file
  guide <- file.path(temp_dir, "__data_reviewing_guide.html")
  testthat::expect_true(file.exists(guide))
  html <- paste(readLines(guide, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_match(html, "Data Reviewing Guide", fixed = TRUE)
  testthat::expect_match(html, 'const dataFilename = "arboretum_data_test.csv"', fixed = TRUE)
  for (col in c("FFB.vernacularName", "country", "endemism", "botanical_country",
                "introduced_to", "FFB.establishmentMeans", "FFB.stateProvince",
                "FFB.phytogeographicDomain", "FFB.vegetationType",
                "IUCN.status", "plant_uses_EN", "plant_uses_PT",
                "plant_uses_ES", "plant_uses_FR", "free_notes_EN", "free_notes_PT",
                "free_notes_ES", "free_notes_FR")) {
    testthat::expect_match(html, paste0('data-col="', col, '"'), fixed = TRUE, info = col)
  }
  testthat::expect_equal(result$genus, c("Euterpe", "Coffea"))

  euterpe <- result[result$taxonName == "Euterpe edulis", , drop = FALSE]
  coffea <- result[result$taxonName == "Coffea arabica", , drop = FALSE]

  testthat::expect_equal(euterpe$country, "Brazil | Peru")
  testthat::expect_equal(euterpe$endemism, "Non-endemic")
  testthat::expect_equal(euterpe$FFB.vernacularName, "juçara | palmito")
  testthat::expect_equal(euterpe$FFB.stateProvince, "BA")
  testthat::expect_equal(euterpe$FFB.phytogeographicDomain, "Atlantic Forest")
  testthat::expect_equal(euterpe$FFB.vegetationType, "Cerrado sensu lato")
  testthat::expect_equal(euterpe$introduced_to, "Cuba")
  testthat::expect_equal(euterpe$IUCN.status, "Least Concern (LC)")
  testthat::expect_equal(euterpe$POWO.url, "powo/euterpe")

  testthat::expect_equal(coffea$country, "Brazil")
  testthat::expect_equal(coffea$endemism, "Endemic")
  testthat::expect_equal(coffea$FFB.establishmentMeans, "Cultivated")
  testthat::expect_equal(coffea$FFB.vegetationType, "Caatinga sensu stricto")
  testthat::expect_true(is.na(coffea$POWO.url))
  testthat::expect_true(is.na(coffea$IUCN.status))
  testthat::expect_false(any(is.na(result$FFB.genusRank)))

  saved <- utils::read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  testthat::expect_identical(names(saved), names(result))
  testthat::expect_equal(saved$IUCN.status, result$IUCN.status)
})

testthat::test_that("arboretum_data reads an existing data file instead of querying again", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_rerun_test")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  iucn_calls <- new.env()
  iucn_calls$n <- 0L
  .local_mock_data_sources(iucn_calls = iucn_calls)

  first <- aRboretum::arboretum_data(
    spp_list = c("Euterpe edulis", "Coffea arabica"),
    verbose = FALSE,
    dir = temp_dir
  )
  testthat::expect_equal(iucn_calls$n, 1L)

  # The user reviews and corrects the data file
  csv_path <- file.path(temp_dir, "arboretum_data.csv")
  edited <- utils::read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  edited$FFB.vernacularName[edited$taxonName == "Coffea arabica"] <- "cafeeiro"
  utils::write.csv(edited, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  testthat::expect_message(
    second <- aRboretum::arboretum_data(
      spp_list = c("Euterpe edulis", "Coffea arabica"),
      verbose = TRUE,
      dir = temp_dir
    ),
    "already exists and is read instead of retrieving the data again"
  )

  testthat::expect_equal(iucn_calls$n, 1L)
  testthat::expect_equal(second$FFB.vernacularName[second$taxonName == "Coffea arabica"],
                         "cafeeiro")
  testthat::expect_false(any(grepl("^full_phrases_", names(second))))

  # The reviewing guide is rebuilt from the corrected file
  html <- paste(readLines(file.path(temp_dir, "__data_reviewing_guide.html"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_match(html, ">cafeeiro</textarea>", fixed = TRUE)
})

testthat::test_that("arboretum_data shows the annotation fields of printed_lang only", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_printed_lang")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  .local_mock_data_sources()

  result <- aRboretum::arboretum_data(spp_list = "Euterpe edulis", printed_lang = c("es", "fr"),
                                      verbose = FALSE, dir = temp_dir)

  html <- paste(readLines(file.path(temp_dir, "__data_reviewing_guide.html"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_match(html, 'data-col="plant_uses_ES"', fixed = TRUE)
  testthat::expect_match(html, 'data-col="free_notes_FR"', fixed = TRUE)
  testthat::expect_no_match(html, 'data-col="plant_uses_EN"', fixed = TRUE)
  testthat::expect_match(html, '<html lang="es">', fixed = TRUE)

  # The data keep the annotation columns of every language
  testthat::expect_true(all(c("plant_uses_EN", "free_notes_PT") %in% names(result)))

  testthat::expect_error(
    aRboretum::arboretum_data(spp_list = "Euterpe edulis", printed_lang = "de",
                              verbose = FALSE, dir = temp_dir),
    "Invalid language code"
  )
})

testthat::test_that("the data reviewing form offers menus for values used by the phrases", {
  df <- .fixture_species_df()
  df$IUCN.status[2] <- "Weird status"

  out <- aRboretum:::.build_species_cards(
    function_use = "_review",
    df = df,
    printed_lang = c("en", "pt"),
    html_phrases = list(),
    initial_lang = "en"
  )

  # No phrase blocks, but an always-open form
  testthat::expect_no_match(out, "lang-content", fixed = TRUE)
  testthat::expect_no_match(out, "edit-toggle-btn", fixed = TRUE)
  testthat::expect_match(out, '<div class="edit-panel review-panel" id="edit-panel-0">', fixed = TRUE)

  testthat::expect_match(out, '<select class="edit-field" data-row="0" data-col="endemism"',
                         fixed = TRUE)
  testthat::expect_match(out, '<option value="Endemic" selected>Endemic</option>', fixed = TRUE)
  testthat::expect_match(out, '<option value="Cultivated" selected>Cultivated</option>',
                         fixed = TRUE)
  testthat::expect_match(out, '<option value="Endangered (EN)" selected>', fixed = TRUE)
  # Unexpected values already in the data are kept as an option
  testthat::expect_match(out, '<option value="Weird status" selected>Weird status</option>',
                         fixed = TRUE)
  # Empty values give empty fields
  testthat::expect_match(
    out,
    'data-row="0" data-col="FFB.vegetationType" data-kind="data"></textarea>',
    fixed = TRUE
  )
  testthat::expect_match(out, '>pau-brasil | ibirapitanga</textarea>', fixed = TRUE)
  testthat::expect_match(out, 'data-col="plant_uses_PT"', fixed = TRUE)
  testthat::expect_no_match(out, 'data-col="plant_uses_FR"', fixed = TRUE)
  testthat::expect_no_match(out, 'full_phrases_', fixed = TRUE)

  # Genus statistics are not edited, and endemism is labelled as Brazil-only
  testthat::expect_no_match(out, 'data-col="FFB.genusRichness"', fixed = TRUE)
  testthat::expect_no_match(out, 'data-col="FFB.genusRank"', fixed = TRUE)
  testthat::expect_match(out, "<label>endemism (in Brazil only)</label>", fixed = TRUE)
  testthat::expect_match(out, "endemic to Brazil, as recorded by Flora e Funga do Brasil",
                         fixed = TRUE)
})

testthat::test_that("arboretum_data does not write files when save = FALSE", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_nosave_test")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  .local_mock_data_sources()

  result <- aRboretum::arboretum_data(
    spp_list = "Euterpe edulis",
    verbose = FALSE,
    save = FALSE,
    dir = temp_dir
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_false(dir.exists(temp_dir))
})

testthat::test_that("arboretum_data keeps species found only in WCVP and drops unmatched names", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_wcvp_only")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Theobroma cacao is absent from FFB; Miconia albicans is absent everywhere;
  # Caesalpinia echinata and Paubrasilia echinata resolve to the same WCVP name
  .local_mock_data_sources(extract = function(result_POWO, sp, i, wcvp) {
    if (sp == "Theobroma cacao") {
      result_POWO$family[i] <- "Malvaceae"
      result_POWO$taxonName[i] <- "Theobroma cacao"
      result_POWO$scientificNameAuthorship[i] <- "L."
      result_POWO$country[i] <- "Brazil | Peru"
      result_POWO$botanical_country[i] <- "Brazil North | Peru"
      result_POWO$references[i] <- "powo/cacao"
    }
    if (sp %in% c("Caesalpinia echinata", "Paubrasilia echinata")) {
      result_POWO$family[i] <- "Fabaceae"
      result_POWO$taxonName[i] <- "Paubrasilia echinata"
      result_POWO$country[i] <- "Brazil"
      result_POWO$botanical_country[i] <- "Brazil Northeast"
      result_POWO$references[i] <- "powo/paubrasilia"
    }
    result_POWO
  })

  msgs <- testthat::capture_messages(
    result <- aRboretum::arboretum_data(
      spp_list = c("Theobroma cacao", "Miconia albicans",
                   "Caesalpinia echinata", "Paubrasilia echinata"),
      verbose = TRUE,
      save = TRUE,
      format = "xlsx",
      dir = temp_dir
    )
  )

  testthat::expect_true(any(grepl("1/4: retrieving information from 'Theobroma cacao'", msgs)))
  testthat::expect_true(any(grepl("successfully loaded and parsed", msgs)))
  testthat::expect_true(file.exists(file.path(temp_dir, "arboretum_data.xlsx")))

  testthat::expect_setequal(result$taxonName, c("Theobroma cacao", "Paubrasilia echinata"))
  cacao <- result[result$taxonName == "Theobroma cacao", ]
  testthat::expect_equal(cacao$endemism, "Non-endemic")
  testthat::expect_equal(cacao$POWO.url, "powo/cacao")
  testthat::expect_true(is.na(cacao$FFB.url))
  testthat::expect_true(all(c("FFB.phytogeographicDomain", "FFB.url") %in% names(result)))
  paub <- result[result$taxonName == "Paubrasilia echinata", ]
  testthat::expect_equal(paub$endemism, "Endemic")

  # The saved xlsx is read back on the next run
  again <- aRboretum::arboretum_data(
    spp_list = "Theobroma cacao", verbose = FALSE, format = "xlsx", dir = temp_dir
  )
  testthat::expect_setequal(again$taxonName, result$taxonName)
})

testthat::test_that("arboretum_data matches WCVP data to FFB synonyms through the original query", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_synonym")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # FFB treats Euterpe edulis as a synonym of Euterpe espiritosantensis,
  # while WCVP keeps Euterpe edulis as accepted
  dwca <- .fake_dwca()
  tax <- dwca[[1]]$data[["taxon.txt"]]
  tax <- rbind(tax, data.frame(
    id = "id-4", taxonRank = "ESPECIE", taxonName = "Euterpe espiritosantensis",
    taxonomicStatus = "NOME_ACEITO", family = "Arecaceae",
    scientificNameAuthorship = "Fernald", references = "ffb/espiritosantensis",
    genus = "Euterpe", stringsAsFactors = FALSE
  ))
  dwca[[1]]$data[["taxon.txt"]] <- tax
  dist <- dwca[[1]]$data[["distribution.txt"]]
  dist <- rbind(dist, data.frame(
    id = "id-4", dataset = "x", countryCode = "BR", establishmentMeans = "NATIVA",
    locationID = "BR-ES", endemism = "Não endemica",
    phytogeographicDomain = "Mata Atlântica", stringsAsFactors = FALSE
  ))
  dwca[[1]]$data[["distribution.txt"]] <- dist

  .local_mock_data_sources(
    fake_dwca = dwca,
    accepted_id = function(taxon_id) if (identical(taxon_id, "id-1")) "id-4" else taxon_id
  )

  result <- aRboretum::arboretum_data(
    spp_list = "Euterpe edulis", verbose = FALSE, save = FALSE, dir = temp_dir
  )

  testthat::expect_equal(result$taxonName, "Euterpe espiritosantensis")
  testthat::expect_equal(result$FFB.url, "ffb/espiritosantensis")
  # WCVP fields come from the original query
  testthat::expect_equal(result$POWO.url, "powo/euterpe")
  testthat::expect_equal(result$introduced_to, "Cuba")
  testthat::expect_equal(result$country, "Brazil | Peru")
  testthat::expect_equal(result$endemism, "Non-endemic")
})

# Flora e Funga do Brasil dataset -----------------------------------------------

# Create fake FFB version folders inside a temporary ffb_dir
.fake_ffb_dir <- function(name, folders) {
  ffb_dir <- file.path(tempdir(), name)
  unlink(ffb_dir, recursive = TRUE)
  for (f in folders) {
    dir.create(file.path(ffb_dir, f), recursive = TRUE)
    writeLines("id", file.path(ffb_dir, f, "taxon.txt"))
  }
  dir.create(ffb_dir, showWarnings = FALSE)
  ffb_dir
}

# Mock floraR, recording the calls
.local_mock_floraR <- function(calls, download = function(...) invisible(TRUE),
                               env = parent.frame()) {
  testthat::local_mocked_bindings(
    flora_download = function(version, verbose, dir) {
      calls$download <- c(calls$download, dir)
      download(version = version, verbose = verbose, dir = dir)
    },
    flora_parse = function(path, version, verbose) {
      calls$parse <- c(calls$parse, list(list(path = path, version = version)))
      "parsed"
    },
    .package = "floraR",
    .env = env
  )
}

testthat::test_that(".load_ffb uses the newest downloaded version without contacting FFB", {
  testthat::skip_if_not_installed("floraR")
  ffb_dir <- .fake_ffb_dir("ffb-local", c("dwca_ffb_v393_99", "dwca_ffb_v393_430_latest",
                                          "not_a_dwca", "dwca_ffb_v393_500"))
  # A version folder without taxon.txt is ignored
  unlink(file.path(ffb_dir, "dwca_ffb_v393_500", "taxon.txt"))
  on.exit(unlink(ffb_dir, recursive = TRUE), add = TRUE)

  calls <- new.env()
  .local_mock_floraR(calls, download = function(...) stop("should not download"))

  testthat::expect_message(
    out <- aRboretum:::.load_ffb(paste0(ffb_dir, "/"), verbose = TRUE),
    "Using the FFB dataset previously downloaded"
  )
  testthat::expect_identical(out, "parsed")
  testthat::expect_null(calls$download)
  testthat::expect_identical(calls$parse, list(list(path = ffb_dir, version = "393.430")))
})

testthat::test_that(".load_ffb uses older versions too when no latest folder exists", {
  testthat::skip_if_not_installed("floraR")
  ffb_dir <- .fake_ffb_dir("ffb-no-latest", c("dwca_ffb_v393_430", "dwca_ffb_v393_99"))
  on.exit(unlink(ffb_dir, recursive = TRUE), add = TRUE)

  calls <- new.env()
  .local_mock_floraR(calls, download = function(...) stop("should not download"))

  aRboretum:::.load_ffb(ffb_dir, verbose = FALSE)
  testthat::expect_identical(calls$parse, list(list(path = ffb_dir, version = "393.430")))
})

testthat::test_that(".load_ffb downloads the latest version when nothing was downloaded yet", {
  testthat::skip_if_not_installed("floraR")
  ffb_dir <- .fake_ffb_dir("ffb-first-download", character(0))
  on.exit(unlink(ffb_dir, recursive = TRUE), add = TRUE)

  calls <- new.env()
  .local_mock_floraR(calls, download = function(version, verbose, dir) {
    dir.create(file.path(dir, "dwca_ffb_v393_431_latest"))
    writeLines("id", file.path(dir, "dwca_ffb_v393_431_latest", "taxon.txt"))
  })

  aRboretum:::.load_ffb(ffb_dir, verbose = FALSE)
  testthat::expect_identical(calls$download, ffb_dir)
  testthat::expect_identical(calls$parse, list(list(path = ffb_dir, version = "393.431")))
})

testthat::test_that(".load_ffb uses a single version folder as is, without downloading", {
  testthat::skip_if_not_installed("floraR")
  ffb_dir <- .fake_ffb_dir("ffb-single", c("dwca_ffb_v393_420", "dwca_ffb_v393_430_latest"))
  on.exit(unlink(ffb_dir, recursive = TRUE), add = TRUE)

  calls <- new.env()
  .local_mock_floraR(calls, download = function(...) stop("should not download"))

  testthat::expect_message(
    aRboretum:::.load_ffb(file.path(ffb_dir, "dwca_ffb_v393_420"), verbose = TRUE),
    "Using the FFB dataset in"
  )
  testthat::expect_null(calls$download)
  testthat::expect_identical(calls$parse, list(list(path = ffb_dir, version = "393.420")))
})

testthat::test_that(".load_ffb fails clearly without internet nor local data", {
  testthat::skip_if_not_installed("floraR")
  ffb_dir <- .fake_ffb_dir("ffb-empty", character(0))
  on.exit(unlink(ffb_dir, recursive = TRUE), add = TRUE)

  calls <- new.env()
  .local_mock_floraR(calls, download = function(...) stop("No internet connection"))

  testthat::expect_error(aRboretum:::.load_ffb(ffb_dir, verbose = FALSE),
                         "no previously downloaded version was found")
  testthat::expect_error(aRboretum:::.load_ffb(c("a", "b")), "single character string")

  # A folder that does not exist yet is simply where the download goes
  testthat::expect_identical(aRboretum:::.ffb_local_versions(file.path(ffb_dir, "nope")),
                             character(0))
  testthat::expect_error(aRboretum:::.load_ffb(file.path(ffb_dir, "nope"), verbose = FALSE),
                         "no previously downloaded version was found")
  testthat::expect_identical(calls$download[2], file.path(ffb_dir, "nope"))

  # flora_download() returning without leaving a dataset behind
  calls2 <- new.env()
  .local_mock_floraR(calls2)
  testthat::expect_error(aRboretum:::.load_ffb(ffb_dir, verbose = FALSE),
                         "No FFB dataset was found")
})

testthat::test_that("arboretum_data keeps the FFB download and passes ffb_dir along", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_ffb_dir")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  seen <- new.env()
  .local_mock_data_sources()
  testthat::local_mocked_bindings(
    .load_ffb = function(ffb_dir, verbose) {
      seen$ffb_dir <- ffb_dir
      .fake_dwca()
    },
    .package = "aRboretum"
  )

  aRboretum::arboretum_data(spp_list = "Euterpe edulis", verbose = FALSE, save = FALSE,
                            dir = temp_dir, ffb_dir = "my_ffb")
  testthat::expect_identical(seen$ffb_dir, "my_ffb")
})

# WCVP extraction --------------------------------------------------------------

.fake_wcvp <- function() {
  names <- data.frame(
    plant_name_id = c(1, 2, 3, 4, 5),
    taxon_rank = c("Species", "Species", "Species", "Species", "Variety"),
    taxon_status = c("Accepted", "Synonym", "Illegitimate", "Accepted", "Accepted"),
    taxon_name = c("Paubrasilia echinata", "Caesalpinia echinata", "Euterpe edulis",
                   "Euterpe edulis", "Inga edulis var. minor"),
    taxon_authors = c("(Lam.) Gagnon, H.C.Lima & G.P.Lewis", "Lam.", "Hort.", "Mart.", "Benth."),
    family = c("Fabaceae", "Fabaceae", "Arecaceae", "Arecaceae", "Fabaceae"),
    accepted_plant_name_id = c(1, 1, NA, 4, 5),
    powo_id = c("paub-1", "caes-1", "eut-old", "eut-1", "inga-1"),
    stringsAsFactors = FALSE
  )
  distributions <- data.frame(
    plant_name_id = c(1, 4, 4, 4),
    area = c("Brazil Northeast", "Brazil Southeast", "Argentina Northeast", "Cuba"),
    introduced = c(0, 0, 0, 1),
    stringsAsFactors = FALSE
  )
  list(names = names,
       species = names[names$taxon_rank == "Species", ],
       distributions = distributions)
}

.empty_result_powo <- function(sp) {
  data.frame(
    original_query = sp,
    family = NA_character_,
    taxonName = NA_character_,
    scientificNameAuthorship = NA_character_,
    country = NA_character_,
    botanical_country = NA_character_,
    introduced_to = NA_character_,
    references = NA_character_,
    stringsAsFactors = FALSE
  )
}

testthat::test_that(".extract_wcvp_data returns unchanged result when the name is not in WCVP", {
  result_POWO <- .empty_result_powo("Miconia albicans")
  out <- aRboretum:::.extract_wcvp_data(result_POWO, "Miconia albicans", 1, .fake_wcvp())
  testthat::expect_identical(out, result_POWO)
})

testthat::test_that(".extract_wcvp_data resolves synonyms to the accepted name", {
  out <- aRboretum:::.extract_wcvp_data(.empty_result_powo("Caesalpinia echinata"),
                                        "Caesalpinia echinata", 1, .fake_wcvp())

  testthat::expect_equal(out$taxonName, "Paubrasilia echinata")
  testthat::expect_equal(out$family, "Fabaceae")
  testthat::expect_equal(out$scientificNameAuthorship, "(Lam.) Gagnon, H.C.Lima & G.P.Lewis")
  testthat::expect_equal(out$botanical_country, "Brazil Northeast")
  testthat::expect_equal(out$country, "Brazil")
  testthat::expect_true(is.na(out$introduced_to))
  testthat::expect_equal(out$references,
                         "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:paub-1")
})

testthat::test_that(".extract_wcvp_data ignores synonyms without a usable accepted name", {
  wcvp <- .fake_wcvp()
  wcvp$species <- rbind(wcvp$species, data.frame(
    plant_name_id = c(6, 7), taxon_rank = "Species", taxon_status = c("Synonym", "Synonym"),
    taxon_name = c("Orphanus synonymus", "Lostus acceptedus"), taxon_authors = "X.",
    family = "Fabaceae", accepted_plant_name_id = c(NA, 999), powo_id = c("o-1", "l-1"),
    stringsAsFactors = FALSE
  ))

  for (sp in c("Orphanus synonymus", "Lostus acceptedus")) {
    empty <- .empty_result_powo(sp)
    testthat::expect_identical(aRboretum:::.extract_wcvp_data(empty, sp, 1, wcvp), empty)
  }

  # A synonym whose accepted name is infraspecific is still resolved
  wcvp$species <- rbind(wcvp$species, data.frame(
    plant_name_id = 8, taxon_rank = "Species", taxon_status = "Synonym",
    taxon_name = "Inga minor", taxon_authors = "X.", family = "Fabaceae",
    accepted_plant_name_id = 5, powo_id = "im-1", stringsAsFactors = FALSE
  ))
  out <- aRboretum:::.extract_wcvp_data(.empty_result_powo("Inga minor"), "Inga minor", 1, wcvp)
  testthat::expect_equal(out$taxonName, "Inga edulis var. minor")
  testthat::expect_true(is.na(out$botanical_country))
  testthat::expect_true(is.na(out$country))
})

testthat::test_that(".load_wcvp returns names, species and distributions tables", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rWCVPdata")

  wcvp <- aRboretum:::.load_wcvp()
  testthat::expect_named(wcvp, c("names", "species", "distributions"))
  testthat::expect_true(all(wcvp$species$taxon_rank == "Species"))
  testthat::expect_false(anyNA(wcvp$distributions$area))

  out <- aRboretum:::.extract_wcvp_data(.empty_result_powo("Caesalpinia echinata"),
                                        "Caesalpinia echinata", 1, wcvp)
  testthat::expect_equal(out$taxonName, "Paubrasilia echinata")
  testthat::expect_match(out$country, "Brazil")
})

testthat::test_that(".extract_wcvp_data prefers the accepted record among homonyms and splits native from introduced", {
  out <- aRboretum:::.extract_wcvp_data(.empty_result_powo("Euterpe edulis"),
                                        "Euterpe edulis", 1, .fake_wcvp())

  testthat::expect_equal(out$taxonName, "Euterpe edulis")
  testthat::expect_equal(out$scientificNameAuthorship, "Mart.")
  testthat::expect_equal(out$botanical_country, "Brazil Southeast | Argentina Northeast")
  testthat::expect_equal(out$country, "Argentina | Brazil")
  testthat::expect_equal(out$introduced_to, "Cuba")
})

# IUCN status ------------------------------------------------------------------

testthat::test_that(".iucn_code and .iucn_label normalise Red List categories", {
  testthat::expect_identical(
    aRboretum:::.iucn_code(c("Endangered (EN)", "LC", "vulnerable", "LR/cd", NA, "unknown")),
    c("EN", "LC", "VU", "NT", NA, NA)
  )
  testthat::expect_identical(
    aRboretum:::.iucn_label(c("CR", "Near Threatened (NT)", NA)),
    c("Critically Endangered (CR)", "Near Threatened (NT)", NA)
  )
})

testthat::test_that(".get_iucn_status queries GBIF once per unique name when no token is set", {
  urls <- character(0)
  testthat::local_mocked_bindings(
    .gbif_get = function(url) {
      urls <<- c(urls, url)
      if (grepl("match", url)) {
        if (grepl("Unknown", url)) return(list(matchType = "NONE"))
        key <- if (grepl("Paubrasilia", url)) 1 else 2
        return(list(usageKey = key, matchType = "EXACT"))
      }
      if (grepl("/1/", url)) list(code = "EN") else list()
    },
    .package = "aRboretum"
  )

  out <- aRboretum:::.get_iucn_status(
    c("Paubrasilia echinata", "Euterpe edulis", "Paubrasilia echinata", "Unknown plant", NA),
    source = "gbif",
    verbose = FALSE
  )

  testthat::expect_identical(out, c("Endangered (EN)", NA, "Endangered (EN)", NA, NA))
  testthat::expect_equal(sum(grepl("match", urls)), 3)
})

testthat::test_that(".get_iucn_status returns NA when the request fails", {
  testthat::local_mocked_bindings(
    .gbif_get = function(url) stop("offline"),
    .package = "aRboretum"
  )
  out <- aRboretum:::.get_iucn_status("Paubrasilia echinata", source = "gbif", verbose = FALSE)
  testthat::expect_identical(out, NA_character_)
})

testthat::test_that(".iucn_from_redlist keeps the latest global assessment", {
  testthat::skip_if_not_installed("rredlist")

  testthat::local_mocked_bindings(
    rl_species = function(genus, species, key, ...) {
      list(assessments = data.frame(
        latest = c(FALSE, TRUE, TRUE),
        red_list_category_code = c("VU", "NT", "EN"),
        scopes = I(list(list(code = "1"), list(code = "2"), list(code = "1")))
      ))
    },
    .package = "rredlist"
  )

  testthat::expect_identical(
    aRboretum:::.iucn_from_redlist("Paubrasilia echinata", key = "token"),
    "EN"
  )
})

testthat::test_that(".get_iucn_status uses the Red List API when a token is available", {
  testthat::skip_if_not_installed("rredlist")

  testthat::local_mocked_bindings(
    .iucn_from_redlist = function(taxon, key) "VU",
    .iucn_from_gbif = function(taxon) stop("GBIF should not be used"),
    .package = "aRboretum"
  )

  out <- aRboretum:::.get_iucn_status("Paubrasilia echinata", key = "token", verbose = FALSE)
  testthat::expect_identical(out, "Vulnerable (VU)")
})
