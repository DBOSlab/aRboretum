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
