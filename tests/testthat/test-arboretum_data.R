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
    .package = "aRboretum",
    .env = env
  )

  testthat::local_mocked_bindings(
    flora_download = function(version, dir) invisible(TRUE),
    flora_parse = function(path, version) fake_dwca,
    .package = "floraR",
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
  testthat::expect_true(file.exists(file.path(temp_dir, "__phrase_generating_guide.html")))
  testthat::expect_true(all(c("full_phrases_EN", "full_phrases_PT", "full_phrases_ES",
                              "full_phrases_FR", "full_phrases_ADD_LANGUAGE") %in% names(result)))
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

  # Phrases are generated for every built-in language and stored in the data
  for (col in c("full_phrases_EN", "full_phrases_PT", "full_phrases_ES", "full_phrases_FR")) {
    testthat::expect_true(all(nzchar(result[[col]])), info = col)
  }
  testthat::expect_match(euterpe$full_phrases_EN, "Euterpe edulis")
  testthat::expect_match(euterpe$full_phrases_EN, "least concern \\(LC\\)")
  testthat::expect_true(all(is.na(result$full_phrases_ADD_LANGUAGE)))

  saved <- utils::read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  testthat::expect_equal(saved$full_phrases_EN, result$full_phrases_EN)
  testthat::expect_equal(saved$IUCN.status, result$IUCN.status)
})

testthat::test_that("arboretum_data keeps manually edited phrases and only fills empty ones", {
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
    filename = "arboretum_data",
    dir = temp_dir
  )
  testthat::expect_equal(iucn_calls$n, 1L)

  # The user edits one phrase and clears another directly in the data file
  csv_path <- file.path(temp_dir, "arboretum_data.csv")
  edited <- utils::read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  edited$full_phrases_EN[edited$taxonName == "Euterpe edulis"] <- "My own phrase about the palm."
  edited$full_phrases_PT[edited$taxonName == "Coffea arabica"] <- NA
  utils::write.csv(edited, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  second <- aRboretum::arboretum_data(
    spp_list = c("Euterpe edulis", "Coffea arabica"),
    verbose = FALSE,
    filename = "arboretum_data",
    dir = temp_dir
  )

  # The existing file is reused instead of querying the databases again
  testthat::expect_equal(iucn_calls$n, 1L)
  testthat::expect_equal(
    second$full_phrases_EN[second$taxonName == "Euterpe edulis"],
    "My own phrase about the palm."
  )
  testthat::expect_equal(
    second$full_phrases_EN[second$taxonName == "Coffea arabica"],
    first$full_phrases_EN[first$taxonName == "Coffea arabica"]
  )
  regenerated <- second$full_phrases_PT[second$taxonName == "Coffea arabica"]
  testthat::expect_true(!is.na(regenerated) && nzchar(regenerated))

  # The regenerated phrase was written back while the manual edit was kept
  saved <- utils::read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  testthat::expect_equal(saved$full_phrases_PT, second$full_phrases_PT)
  testthat::expect_equal(
    saved$full_phrases_EN[saved$taxonName == "Euterpe edulis"],
    "My own phrase about the palm."
  )

  # The manual phrase is shown in the HTML guide
  html <- paste(readLines(file.path(temp_dir, "__phrase_generating_guide.html"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_match(html, "My own phrase about the palm.", fixed = TRUE)
  testthat::expect_match(html, 'data-col="full_phrases_EN"', fixed = TRUE)
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
  testthat::expect_true(any(grepl("successfully donwloaded and parsed", msgs)))
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

testthat::test_that("arboretum_data adds the custom language stored in the data file", {
  testthat::skip_if_not_installed("floraR")

  temp_dir <- file.path(tempdir(), "arboretum_data_add_lang")
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  .local_mock_data_sources()

  aRboretum::arboretum_data(spp_list = "Euterpe edulis", verbose = FALSE, dir = temp_dir)
  csv_path <- file.path(temp_dir, "arboretum_data.csv")
  edited <- utils::read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  edited$full_phrases_ADD_LANGUAGE <- "Texto em panará"
  utils::write.csv(edited, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  result <- aRboretum::arboretum_data(spp_list = "Euterpe edulis", add_lang = "PANARA",
                                      verbose = FALSE, dir = temp_dir)

  html <- paste(readLines(file.path(temp_dir, "__phrase_generating_guide.html"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_match(html, 'data-lang="PANARA"', fixed = TRUE)
  testthat::expect_match(html, "Texto em panará", fixed = TRUE)
  testthat::expect_equal(result$full_phrases_ADD_LANGUAGE, "Texto em panará")
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
