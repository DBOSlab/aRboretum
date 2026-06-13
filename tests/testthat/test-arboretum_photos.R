# file: tests/testthat/test-arboretum_photos.R

testthat::test_that("arboretum_photos creates base directory and species photo folders", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  if (dir.exists("arboretum_photos")) {
    unlink("arboretum_photos", recursive = TRUE, force = TRUE)
  }

  testthat::local_mocked_bindings(
    .read_species_data = function(data_path, verbose) df,
    .package = "aRboretum"
  )

  aRboretum::arboretum_photos(
    data_path = "fake.xlsx",
    verbose = FALSE
  )

  testthat::expect_true(dir.exists("arboretum_photos"))

  folders <- sort(list.dirs("arboretum_photos", recursive = FALSE, full.names = FALSE))
  testthat::expect_identical(
    folders,
    sort(c(
      "ARECACEAE_Euterpe_edulis_photos",
      "FABACEAE_Paubrasilia_echinata_photos"
    ))
  )
})

testthat::test_that("arboretum_photos preserves existing base directory and creates only missing folders", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
    family = c("Fabaceae", "Arecaceae"),
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  unlink("arboretum_photos", recursive = TRUE, force = TRUE)
  dir.create("arboretum_photos", recursive = TRUE)
  dir.create(file.path("arboretum_photos", "FABACEAE_Paubrasilia_echinata_photos"))

  existing_file <- file.path("arboretum_photos", "FABACEAE_Paubrasilia_echinata_photos", "keep.txt")
  writeLines("keep me", existing_file)

  testthat::local_mocked_bindings(
    .read_species_data = function(data_path, verbose) df,
    .package = "aRboretum"
  )

  testthat::expect_message(
    aRboretum::arboretum_photos(
      data_path = "fake.xlsx",
      verbose = TRUE
    ),
    "Base directory already exists"
  )

  testthat::expect_true(file.exists(existing_file))
  testthat::expect_true(dir.exists(file.path("arboretum_photos", "ARECACEAE_Euterpe_edulis_photos")))
})

testthat::test_that("arboretum_photos reports skipped folder when species folder already exists", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  unlink("arboretum_photos", recursive = TRUE, force = TRUE)
  dir.create(file.path("arboretum_photos", "FABACEAE_Paubrasilia_echinata_photos"), recursive = TRUE)

  testthat::local_mocked_bindings(
    .read_species_data = function(data_path, verbose) df,
    .package = "aRboretum"
  )

  testthat::expect_message(
    aRboretum::arboretum_photos(
      data_path = "fake.xlsx",
      verbose = TRUE
    ),
    "Folder already exists \\(skipping\\)"
  )
})

testthat::test_that("arboretum_photos normalizes spaces and capitalizes family names in folder names", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = c("Inga edulis", "Handroanthus impetiginosus"),
    family = c("fabaceae", "Bignoniaceae"),
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  unlink("arboretum_photos", recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .read_species_data = function(data_path, verbose) df,
    .package = "aRboretum"
  )

  aRboretum::arboretum_photos(
    data_path = "fake.xlsx",
    verbose = FALSE
  )

  folders <- sort(list.dirs("arboretum_photos", recursive = FALSE, full.names = FALSE))
  testthat::expect_true("FABACEAE_Inga_edulis_photos" %in% folders)
  testthat::expect_true("BIGNONIACEAE_Handroanthus_impetiginosus_photos" %in% folders)
})

testthat::test_that("arboretum_photos returns invisibly NULL", {
  testthat::skip_if_not_installed("aRboretum")

  df <- data.frame(
    taxonName = "Paubrasilia echinata",
    family = "Fabaceae",
    stringsAsFactors = FALSE
  )

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  unlink("arboretum_photos", recursive = TRUE, force = TRUE)

  testthat::local_mocked_bindings(
    .read_species_data = function(data_path, verbose) df,
    .package = "aRboretum"
  )

  out <- aRboretum::arboretum_photos(
    data_path = "fake.xlsx",
    verbose = FALSE
  )

  testthat::expect_null(out)
})

testthat::test_that("arboretum_photos propagates data-reading errors", {
  testthat::skip_if_not_installed("aRboretum")

  old_wd <- getwd()
  tmp <- tempdir()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  testthat::local_mocked_bindings(
    .read_species_data = function(data_path, verbose) {
      stop("Input file not found")
    },
    .package = "aRboretum"
  )

  testthat::expect_error(
    aRboretum::arboretum_photos(
      data_path = "missing.xlsx",
      verbose = FALSE
    ),
    "Input file not found"
  )
})
