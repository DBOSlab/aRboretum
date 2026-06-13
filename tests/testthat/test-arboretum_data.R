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

testthat::test_that("arboretum_data merges FFB and POWO data and writes csv output", {
  temp_dir <- file.path(tempdir(), "arboretum_data_csv_test")
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  local_state <- new.env(parent = emptyenv())
  local_state$csv_path <- NULL

  fake_dwca <- list(list(data = list(
    "taxon.txt" = data.frame(
      id = c("id-1", "id-2", "id-3"),
      taxonRank = c("ESPECIE", "ESPECIE", "GENUS"),
      taxonName = c("Euterpe edulis", "Coffea arabica", "Euterpe"),
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

  testthat::local_mocked_bindings(
    flora_download = function(version, dir) invisible(TRUE),
    flora_parse = function(path, version) fake_dwca,
    .get_accepted_name_id = function(taxon_id, taxon_data, verbose) taxon_id,
    .extract_powo_data = function(result_POWO, sp, i) {
      if (identical(sp, "Euterpe edulis")) {
        result_POWO$family[i] <- "Arecaceae"
        result_POWO$taxonName[i] <- "Euterpe edulis"
        result_POWO$scientificNameAuthorship[i] <- "Mart."
        result_POWO$country[i] <- "Brazil | Peru"
        result_POWO$botanical_country[i] <- "Brazil North | Peru"
        result_POWO$introduced_to[i] <- "Cuba"
        result_POWO$IUCN.status[i] <- "Least Concern (LC)"
        result_POWO$references[i] <- "powo/euterpe"
      }
      result_POWO
    },
    .add_genus_curiosity_notes = function(result_merged, max_diversity, n_max_genera) {
      result_merged$free_notes_EN <- paste("Genus note for", result_merged$genus)
      result_merged
    },
    .save_csv = function(df, verbose, filename, dir) {
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
      path <- file.path(dir, paste0(filename, ".csv"))
      utils::write.csv(df, path, row.names = FALSE)
      local_state$csv_path <- path
      invisible(path)
    },
    .package = "aRboretum"
  )

  testthat::local_mocked_bindings(
    flora_download = function(version, dir) invisible(TRUE),
    flora_parse = function(path, version) fake_dwca,
    .package = "floraR"
  )

  result <- aRboretum::arboretum_data(
    spp_list = c("Euterpe edulis", "Coffea arabica"),
    verbose = FALSE,
    save = TRUE,
    format = "csv",
    filename = "arboretum_data_test",
    dir = temp_dir
  )

  testthat::expect_true(file.exists(local_state$csv_path))
  testthat::expect_true("full_phrases_ADD_LANGUAGE" %in% names(result))
  testthat::expect_equal(result$genus, c("Euterpe", "Coffea")[order(c("Arecaceae", "Rubiaceae"))])

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
  testthat::expect_false(any(is.na(result$FFB.genusRank)))

  unlink(temp_dir, recursive = TRUE)
})

testthat::test_that(".extract_powo_data returns unchanged result when no exact POWO match exists", {
  result_POWO <- data.frame(
    original_query = "Miconia albicans",
    family = NA_character_,
    taxonName = NA_character_,
    scientificNameAuthorship = NA_character_,
    country = NA_character_,
    botanical_country = NA_character_,
    introduced_to = NA_character_,
    IUCN.status = NA_character_,
    references = NA_character_,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    pow_search = function(sci_com) {
      list(data = data.frame(
        name = "Another species",
        accepted = TRUE,
        url = "https://powo.science.kew.org/taxon/urn:lsid:1",
        stringsAsFactors = FALSE
      ))
    },
    .package = "taxize"
  )

  out <- aRboretum:::.extract_powo_data(result_POWO, "Miconia albicans", 1)

  testthat::expect_identical(out, result_POWO)
})

testthat::test_that(".extract_powo_data resolves synonyms and formats distribution and IUCN fields", {
  result_POWO <- data.frame(
    original_query = "Old name species",
    family = NA_character_,
    taxonName = NA_character_,
    scientificNameAuthorship = NA_character_,
    country = NA_character_,
    botanical_country = NA_character_,
    introduced_to = NA_character_,
    IUCN.status = NA_character_,
    references = NA_character_,
    stringsAsFactors = FALSE
  )

  search_calls <- new.env(parent = emptyenv())
  search_calls$n <- 0L

  testthat::local_mocked_bindings(
    .botdiv_to_countries = function(botanical_country, i) "Brazil | Peru",
    .package = "aRboretum"
  )

  testthat::local_mocked_bindings(
    pow_search = function(sci_com) {
      search_calls$n <- search_calls$n + 1L
      if (search_calls$n == 1L) {
        list(data = list(
          name = "Old name species",
          accepted = FALSE,
          url = "https://powo.science.kew.org/taxon/urn:lsid:old",
          synonymOf = data.frame(name = "Accepted species", stringsAsFactors = FALSE)
        ))
      } else {
        list(data = list(
          name = "Accepted species",
          accepted = TRUE,
          url = "https://powo.science.kew.org/taxon/urn:lsid:accepted",
          synonymOf = data.frame(name = NA_character_, stringsAsFactors = FALSE)
        ))
      }
    },
    pow_lookup = function(id, include) {
      list(meta = list(
        family = "Fabaceae",
        name = "Accepted species",
        authors = "A.Auth.",
        distribution = list(
          natives = list(name = c("Brazil North", "Peru")),
          introduced = list(name = c("India", "Sri Lanka"))
        ),
        descriptions = list(
          IUCN = list(
            descriptions = list(
              conservation = list(description = "LC - Least Concern")
            )
          )
        )
      ))
    },
    .package = "taxize"
  )

  out <- aRboretum:::.extract_powo_data(result_POWO, "Old name species", 1)

  testthat::expect_equal(out$family, "Fabaceae")
  testthat::expect_equal(out$taxonName, "Accepted species")
  testthat::expect_equal(out$scientificNameAuthorship, "A.Auth.")
  testthat::expect_equal(out$botanical_country, "Brazil North | Peru")
  testthat::expect_equal(out$country, "Brazil | Peru")
  testthat::expect_equal(out$introduced_to, "India | Sri Lanka")
  testthat::expect_equal(out$IUCN.status, "Least Concern (LC)")
  testthat::expect_equal(
    out$references,
    "https://powo.science.kew.org/taxon/urn:lsid:accepted"
  )
})

testthat::test_that(".extract_powo_data handles accepted names without introduced range or IUCN", {
  result_POWO <- data.frame(
    original_query = "Direct accepted species",
    family = NA_character_,
    taxonName = NA_character_,
    scientificNameAuthorship = NA_character_,
    country = NA_character_,
    botanical_country = NA_character_,
    introduced_to = "placeholder",
    IUCN.status = "placeholder",
    references = NA_character_,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .botdiv_to_countries = function(botanical_country, i) "Brazil",
    .package = "aRboretum"
  )

  testthat::local_mocked_bindings(
    pow_search = function(sci_com) {
      list(data = list(
        name = "Direct accepted species",
        accepted = TRUE,
        url = "https://powo.science.kew.org/taxon/urn:lsid:direct",
        synonymOf = data.frame(name = NA_character_, stringsAsFactors = FALSE)
      ))
    },
    pow_lookup = function(id, include) {
      list(meta = list(
        family = "Myrtaceae",
        name = "Direct accepted species",
        authors = "B.Auth.",
        distribution = list(
          natives = list(name = "Brazil"),
          introduced = list(name = character(0))
        ),
        descriptions = list(
          IUCN = list(
            descriptions = list(
              conservation = list(description = NULL)
            )
          )
        )
      ))
    },
    .package = "taxize"
  )

  out <- aRboretum:::.extract_powo_data(result_POWO, "Direct accepted species", 1)

  testthat::expect_equal(out$family, "Myrtaceae")
  testthat::expect_equal(out$taxonName, "Direct accepted species")
  testthat::expect_equal(out$country, "Brazil")
  testthat::expect_equal(out$botanical_country, "Brazil")
  testthat::expect_true(is.na(out$introduced_to))
  testthat::expect_equal(out$IUCN.status, "placeholder")
  testthat::expect_equal(
    out$references,
    "https://powo.science.kew.org/taxon/urn:lsid:direct"
  )
})
