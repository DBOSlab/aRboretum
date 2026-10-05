# Auxiliary functions to support main functions
# Authors: Martin Boucknooghe & Domingos Cardoso


# Side function to get ID for accepted name of a synonym from FFB ####
.get_accepted_name_id <- function(taxon_id,
                                  taxon_data,
                                  verbose = TRUE){
  status <- taxon_data$taxonomicStatus[taxon_data$id %in% taxon_id]
  original_name_query <- taxon_data$taxonName[taxon_data$id %in% taxon_id]
  if (status == "SINONIMO") {
    id_acc <- taxon_data$acceptedNameUsageID[taxon_data$id %in% taxon_id]
    sp_acc <- taxon_data$taxonName[taxon_data$id %in% id_acc]
    if (verbose) {
      message("The original queried species name '", original_name_query, "' is '", status, "'")
      message("Retrieving data from the currently accepted name '", sp_acc, "'")
    }
    return(id_acc)
  } else {
    return(taxon_id)
  }
}

# Function to save csv file ####
.save_csv <- function(df,
                      verbose = TRUE,
                      filename,
                      dir){

  # Save the data frame if param save is TRUE
  # Create a new directory to save the results with current date
  # If there is no directory... make one!

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  filename <- paste0(filename, ".csv")
  # Create and save the spreadsheet in .csv format
  if (verbose) {
    message(paste0("Writing the csv-formatted spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  utils::write.csv(df, file = paste0(dir, "/", filename), row.names = FALSE,
                   fileEncoding = "UTF-8")
}

# Function to save xlsx file ####
.save_xlsx <- function(df,
                       verbose = TRUE,
                       filename,
                       dir){

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  filename <- paste0(filename, ".xlsx")
  # Create and save the spreadsheet in .xlsx format
  if (verbose) {
    message(paste0("Writing the xlsx-formatted spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  openxlsx::write.xlsx(df, file = paste0(dir, "/", filename), rowNames = FALSE)
}

# Read df function ###
.read_species_data <- function(data_path, verbose = TRUE){
  if (is.null(data_path)) stop("'data_path' must be provided.", call. = FALSE)
  if (!is.character(data_path) || length(data_path) != 1L) stop("'data_path' must be a single character string.", call. = FALSE)
  if (!file.exists(data_path)) stop("File not found: ", data_path, call. = FALSE)

  file_ext <- tolower(tools::file_ext(data_path))
  df <- tryCatch({
    if (file_ext == "csv") {
      if (verbose) message("Reading CSV file: ", basename(data_path))
      utils::read.csv(data_path, stringsAsFactors = FALSE)
    } else if (file_ext == "xlsx") {
      if (verbose) message("Reading Excel file: ", basename(data_path))
      openxlsx::read.xlsx(data_path)
    } else {
      stop("Unsupported file format. Use .csv or .xlsx", call. = FALSE)
    }
  }, error = function(e){
    stop("Failed to read input file: ", e$message, call. = FALSE)
  })

  required_cols <- c("family", "taxonName", "scientificNameAuthorship",
                     "FFB.vernacularName", "country", "endemism",
                     "FFB.establishmentMeans", "FFB.stateProvince",
                     "FFB.phytogeographicDomain", "FFB.vegetationType",
                     "botanical_country", "introduced_to", "IUCN.status",
                     "plant_uses_EN", "plant_uses_PT", "plant_uses_ES", "plant_uses_FR",
                     "free_notes_EN", "free_notes_PT", "free_notes_ES", "free_notes_FR",
                     "POWO.url", "FFB.url")
  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Columns left empty in the file are read as logical; text columns must stay
  # character for the phrase generator
  is_lgl <- vapply(df, is.logical, logical(1))
  df[is_lgl] <- lapply(df[is_lgl], as.character)

  # Phrase columns are optional so that data files created before they were
  # introduced can still be read
  for (col in .phrase_cols()) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    } else {
      df[[col]] <- as.character(df[[col]])
    }
  }

  if (verbose) message("Loaded data with ", nrow(df), " species and ", ncol(df), " columns.")
  return(df)
}

# Names of the columns storing full phrases per language ####
.phrase_cols <- function() {
  c("full_phrases_EN", "full_phrases_PT", "full_phrases_ES", "full_phrases_FR",
    "full_phrases_ADD_LANGUAGE")
}

# Upper case function ####
.capitalize <- function(x){
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

.copy_folder_progress <- function(from, to, overwrite = TRUE, verbose = TRUE){
  if (!dir.exists(from)) stop("Source folder does not exist: ", from)
  total_files <- length(list.files(from, recursive = TRUE))
  if (verbose) {
    message("Copying ", total_files, " files...")
    pb <- utils::txtProgressBar(min = 0, max = total_files, style = 3)
  }
  if (!dir.exists(to)) dir.create(to, recursive = TRUE)
  copied <- file.copy(from, to, recursive = TRUE, overwrite = overwrite)
  if (verbose) {
    close(pb)
    message("\n\u2705 Copied ", sum(copied), " files successfully")
  }
  return(invisible(sum(copied)))
}

# Phrase generator function ####
.build_arboretum_phrases <- function(data_path = NULL,
                                     df = NULL,
                                     printed_lang = c("pt", "en", "fr", "es"),
                                     add_lang = NULL,
                                     verbose = TRUE) {

  printed_lang <- .arg_check_printed_lang(printed_lang)

  if (is.null(df)) {
    df <- .read_species_data(data_path, verbose)
  }

  dict <- .dict()

  has_add_lang_phrases <- !is.null(add_lang) &&
    "full_phrases_ADD_LANGUAGE" %in% names(df) &&
    any(nzchar(trimws(stats::na.omit(df$full_phrases_ADD_LANGUAGE))))

  if (has_add_lang_phrases) {
    printed_lang <- unique(c(printed_lang, add_lang))
    if (verbose) {
      message("Added custom language to phrases: ", toupper(add_lang))
    }
  }

  html_phrases <- list()
  base_langs <- setdiff(printed_lang, add_lang)
  n_generated <- 0L

  for (lang in base_langs) {
    col <- paste0("full_phrases_", .lang_suffix(lang))
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
    stored <- as.character(df[[col]])
    missing <- is.na(stored) | !nzchar(trimws(stored))

    # Only generate phrases for species without a stored phrase, so manual
    # edits made in the data file are preserved across runs
    if (any(missing)) {
      generated <- .phrase_generator(
        df = df[missing, , drop = FALSE],
        dict = dict,
        lang = lang,
        verbose = verbose
      )
      stored[missing] <- trimws(unlist(generated, use.names = FALSE))
      n_generated <- n_generated + sum(missing)
      if (verbose) {
        message("Generated phrases for language: ", toupper(lang))
      }
    } else if (verbose) {
      message("Using stored phrases for language: ", toupper(lang))
    }

    df[[col]] <- stored
    html_phrases[[lang]] <- stats::setNames(as.list(stored), df$taxonName)
  }

  if (has_add_lang_phrases) {
    add_phrases <- as.list(ifelse(
      is.na(df$full_phrases_ADD_LANGUAGE) |
        !nzchar(trimws(df$full_phrases_ADD_LANGUAGE)),
      "",
      df$full_phrases_ADD_LANGUAGE
    ))
    names(add_phrases) <- df$taxonName
    html_phrases[[add_lang]] <- add_phrases

    if (verbose) {
      message("Loaded custom phrases for language: ", toupper(add_lang))
    }
  }

  list(
    df = df,
    printed_lang = printed_lang,
    html_phrases = html_phrases,
    has_add_lang_phrases = has_add_lang_phrases,
    n_generated = n_generated
  )
}

.phrase_generator <- function(df,
                              dict,
                              lang = "en",
                              verbose = TRUE){

  # Load botanical subdivisions from internal package data
  botregions <- get("botregions", envir = as.environment("package:aRboretum"))

  # Geographic Dictionaries
  continent_dict <- tibble::tibble(
    key = unique(botregions$continent),
    en = unique(botregions$continent_EN),
    pt = unique(botregions$continent_PT),
    es = unique(botregions$continent_ES),
    fr = unique(botregions$continent_FR)
  )

  country_dict <- tibble::tibble(
    key = unique(botregions$country),
    en = unique(botregions$country),
    pt = unique(botregions$country_PT),
    es = unique(botregions$country_ES),
    fr = unique(botregions$country_FR)
  )

  bot_cntr_dict <- tibble::tibble(
    key = unique(botregions$botanical_division),
    en = unique(botregions$botanical_division),
    pt = unique(botregions$botanical_division_PT),
    es = unique(botregions$botanical_division_ES),
    fr = unique(botregions$botanical_division_FR)
  )

  data_phrase <- list()
  for (i in 1:nrow(df)) {
    family <- df$family[i]
    taxonName <- df$taxonName[i]
    ver_name <- df$FFB.vernacularName[i]
    country <- df$country[i]
    botanical_country <- df$botanical_country[i]
    endemism <- df$endemism[i]
    origins <- df$FFB.establishmentMeans[i]
    state <- df$FFB.stateProvince[i]
    phyto <- df$FFB.phytogeographicDomain[i]
    vege_type <- df$FFB.vegetationType[i]
    introduced <- df$introduced_to[i]
    status <- df$IUCN.status[i]

    # ============================================================
    # PART 1: Taxonomic classification ####
    # ============================================================
    # e.g., "Euterpe edulis belongs to Arecaceae family."

    if (lang != "en") {
      phrase_tax <- paste0(
        "<i>", taxonName, "</i>", " ",
        .tr_dict("belongs_to", lang, dict), " ", .tr_dict("family", lang, dict), " ", family, ".",
        " "
      )

    } else {
      phrase_tax <- paste0(
        "<i>", taxonName, "</i>", " ",
        .tr_dict("belongs_to", lang, dict), " ", family, " ",
        .tr_dict("family", lang, dict), ". "
      )
    }

    # ============================================================
    # PART 2: Common names (vernacular names) ####
    # ============================================================
    # Handle multiple vernacular names separated by " | "

    if (is.na(ver_name)) {
      phrase_ver <- ""

    } else {

      n <- lengths(strsplit(ver_name, "\\|"))
      if (n == 1) {
        phrase_ver <- paste0(.tr_dict("commonly_known_as", lang, dict),
                             " ",
                             ver_name[1], ". ")

      } else {
        and <- paste0(" ", .tr_dict("and", lang, dict), " ")
        alt_name <- paste0(sample(strsplit(ver_name, " \\| ")[[1]], 2), collapse = and)
        phrase_ver <- paste0(.tr_dict("commonly_known", lang, dict),
                             " ",
                             .tr_dict("by_at_least", lang, dict),
                             " ",
                             n,
                             " ",
                             .tr_dict("common_names", lang, dict),
                             " ",
                             .tr_dict("such_as", lang, dict),
                             " ",
                             alt_name,
                             ". ")
      }
    }

    # ============================================================
    # PART 3: Endemism and distribution ####
    # ============================================================
    # e.g., ""

    n_cntr <- lengths(strsplit(country, "\\|"))
    n_btcl_cntr <- lengths(strsplit(botanical_country, "\\|"))
    n_state <- lengths(strsplit(state, "\\|"))
    n_phyto <- if (is.na(phyto)) 0L else lengths(strsplit(phyto, "\\|"))

    if (is.na(endemism)) {
      phrase_dis <- ""

    } else {
      if (endemism == "Endemic") {
        if (n_btcl_cntr == 1) {
          endemic_area <- botanical_country
          if (country %in% "Brazil") {
            temp_keys <- dict$key[dict$en %in% botanical_country]
            if (length(temp_keys) > 0) {
              endemic_area <- .tr_dict(temp_keys[1], lang, dict)
            }
          }
          if (n_state == 1) {
            phrase_dis <- paste0(.capitalize(.tr_dict("endemic", lang, dict)),
                                 " ",
                                 .tr_dict("to", lang, dict),
                                 " ",
                                 endemic_area,
                                 ", ",
                                 .tr_dict("it_is_only_found_in", lang, dict),
                                 " ",
                                 .convert_acronym_br_state(state), ".", " ",

                                 .capitalize(.tr_dict("true_rare_gem", lang, dict)),
                                 "!", " "
            )

          } else {
            phrase_dis <- paste0(
              .tr_dict("this_species_is", lang, dict),
              " ",
              .tr_dict("endemic", lang, dict)
              , " ",
              .tr_dict("to", lang, dict),
              " ",
              endemic_area, ".", " "
            )
          }
        } else if (n_btcl_cntr > 1) {

          phrase_dis <- paste0(
            .tr_dict("this_species_is", lang, dict), " ",
            .tr_dict("endemic", lang, dict), " ",
            .tr_dict("to", lang, dict), " ",
            country, ".", " "
          )
        }

      } else {
        temp <- strsplit(country, " \\| ")[[1]]
        alt_country <- .tr_or_raw(temp, lang, country_dict)

        or <- paste0(" ", .tr_dict("or", lang, dict), " ")
        alt_country <- paste0(sample(alt_country, min(2, length(alt_country))),
                              collapse = or)
        phrase_dis <- paste0(
          .tr_dict("this_species_is_found_in", lang, dict), " ",
          alt_country,
          "." , " "
        )
      }
    }

    # ========================================================================
    # PART 4: Establishment means (Native, Naturalized, Cultivated) ####
    # ========================================================================
    # e.g., ""

    if (is.na(origins)) {
      phrase_est <- ""

    } else if (endemism %in% "Endemic") {
      phrase_est <- paste0(
        .tr_dict("only_place_earth", lang, dict),
        ".", " "
      )
    } else if (origins %in% "Native" && endemism %in% "Non-endemic") {
      phrase_est <- paste0(
        .tr_dict("native_brazil_explanation", lang, dict),
        ".", " "
      )

    } else if (origins %in% "Naturalized") {
      phrase_est <- paste0(
        .tr_dict("cultivated_brazil_explanation", lang, dict),
        ".", " "
      )
    } else {
      phrase_est <- paste0(
        .tr_dict("naturalised_brazil_explanation", lang, dict),
        ".", " "
      )
    }

    # ============================================================
    # PART 5: Introduced ####
    # ============================================================
    # e.g., ""

    if (is.na(introduced)) {
      phrase_int <- ""

    } else {
      n <- lengths(strsplit(introduced, "\\|"))
      if (n == 1) {
        phrase_int <- paste0(
          .tr_dict("this_plant_has_also_been_introduced_to_single", lang, dict),
          " ",
          .tr_or_raw(introduced, lang, bot_cntr_dict),
          ".", " "
        )
      } else {

        temp <- strsplit(introduced, " \\| ")[[1]]
        alt_introduced <- .tr_or_raw(temp, lang, bot_cntr_dict)

        and <- paste0(" ", .tr_dict("and", lang, dict), " ")
        alt_introduced <- paste0(sample(alt_introduced, 2), collapse = and)

        phrase_int <- paste0(
          .tr_dict("this_plant_has_also_been_introduced_to_several", lang, dict),
          " ", alt_introduced, ".", " "
        )
      }
    }

    # ============================================================
    # PART 6: Phytogeographic domains (biomes) ####
    # ============================================================

    if (is.na(phyto)) {
      phrase_biom <- ""

    } else {
      n_phyto <- lengths(strsplit(phyto, "\\|"))

      if (n_phyto == 1) {
        temp_keys <- dict$key[dict$en == phyto]

        phrase_biom <- paste0(
          .tr_dict("it_inhabits_the", lang, dict), " ",
          .tr_dict(temp_keys, lang, dict), " "
        )
      } else if (n_phyto %in% 2:5) {

        and <- paste0(" ", .tr_dict("and", lang, dict), " ")

        temp_keys <- vector()
        temp <- strsplit(phyto, " \\| ")[[1]]
        for (l in seq_along(temp)) {
          temp_keys[l] <- dict$key[which(dict$en %in% temp[l])]
        }
        temp_phyto <- .tr_dict_vec(temp_keys, lang, dict)

        alt_phyto <- paste0(sample(temp_phyto, 2), collapse = and)

        phrase_biom <- paste0(
          .tr_dict("it_colonizes_various_habitats", lang, dict)
          , " ",
          .tr_dict("such_as", lang, dict),
          " ",
          alt_phyto, " "
        )
      } else if (n_phyto == 6) {
        phrase_biom <- paste0(
          .tr_dict("found_in_every_biome_in_brazil", lang, dict), ",",
          " ", .tr_dict("how_lucky", lang, dict), " ", "!", " "
        )
      }
    }

    # ============================================================
    # PART 7: Vegetation type ####
    # ============================================================
    # e.g., ""

    if (is.na(vege_type)) {
      phrase_veg <- ""

    } else {
      n_vt <- lengths(strsplit(vege_type, "\\|"))

      if (n_vt == 1 & (n_phyto >= 1 & n_phyto <= 5) & !is.na(phyto)) {
        temp_keys <- dict$key[dict$pt == vege_type]
        phrase_veg <- paste0(
          .tr_dict("where", lang, dict), " ",
          .tr_dict("grow_vegetype", lang, dict), ",", " ",
          .tr_dict(temp_keys, lang, dict), ".", " "
        )
      } else if (n_vt == 1 & (is.na(phyto) | n_phyto == 6)) {
        temp_keys <- dict$key[dict$pt == vege_type]
        phrase_veg <- paste0(
          .tr_dict("grow_vegetype", lang, dict), ",", " ",
          .tr_dict(temp_keys, lang, dict), ".", " "
        )
      } else {
        or <- paste0(" ", .tr_dict("or", lang, dict), " ")

        temp_keys <- vector()
        temp <- strsplit(vege_type, " \\| ")[[1]]
        for (l in seq_along(temp)) {
          temp_keys[l] <- dict$key[which(dict$pt %in% temp[l])]
        }
        temp_vege <- .tr_dict_vec(temp_keys, lang, dict)
        alt_vege_type <- paste0(sample(temp_vege, 2), collapse = or)

        if (is.na(phyto) | n_phyto == 6) {
          phrase_veg <- paste0(
            .tr_dict("grow_vegetypes", lang, dict), " ",
            .tr_dict("such_as", lang, dict),
            " ",
            alt_vege_type, ".", " "
          )
        } else {
          phrase_veg <- paste0(
            .tr_dict("where", lang, dict), " ",
            .tr_dict("grow_vegetypes", lang, dict), " ",
            .tr_dict("such_as", lang, dict),
            " ",
            alt_vege_type, ".", " "
          )
        }
      }
    }

    # Upper-case only the first letter, keeping proper names such as
    # vegetation types intact
    if (phrase_biom == "" | n_phyto == 6) {
      phrase_veg <- paste0(toupper(substr(phrase_veg, 1, 1)),
                           substr(phrase_veg, 2, nchar(phrase_veg)))
    }

    # ============================================================
    # PART 8: IUCN's status ####
    # ============================================================
    # e.g., ""

    # Status is stored as e.g. "Endangered (EN)"; the category code is the key
    iucn_code <- .iucn_code(status)
    if (is.na(iucn_code) || !iucn_code %in% dict$key) {
      phrase_IUCN <- ""

    } else {
      temp_keys <- iucn_code
      phrase_IUCN <- paste0(
        .tr_dict("iucn_classified_as", lang, dict),
        " ",
        .tr_dict(temp_keys, lang, dict), ".", " "
      )
    }

    all_phrases <- paste0(phrase_tax, phrase_ver, phrase_dis, phrase_est, phrase_int,
                          phrase_biom, phrase_veg, phrase_IUCN)
    if (verbose) {
      cat(sprintf("Sentence created for '%s'\n", taxonName))
    }
    data_phrase[[i]] <- all_phrases
  }
  names(data_phrase) <- df$taxonName
  return(data_phrase)
}

#Phrases dictionnary

.dict <- function() {

  dict <- tibble::tibble(
    key = c(

      # Taxon
      "belongs_to", "family", "no_common_name", "commonly_known_as",
      "commonly_known", "by_at_least", "common_names", "such_as", "and",

      # Distribution
      "true_rare_gem", "it_is_only_found_in", "this_species_is_found_in",
      "this_species_is", "to", "non_endemic", "endemic", "or", "BZN", "BNE",
      "BZS", "BSE", "BWC",

      # Establishment means
      "only_place_earth",
      "native_brazil_explanation",
      "cultivated_brazil_explanation",
      "naturalised_brazil_explanation", "it_is",

      # Introduced
      "this_plant_has_also_been_introduced_to_single",
      "this_plant_has_also_been_introduced_to_several",

      # Phytogeographic domain
      "amazon", "atlantic_forest", "pampa", "pantanal", "caatinga_domain", "cerrado_domain",
      "it_inhabits_the", "it_colonizes_various_habitats",
      "found_in_every_biome_in_brazil", "how_lucky",

      # Vegetation types
      "where", "grow_vegetype",
      "grow_vegetypes",
      "carrasco_shrubland", "seasonal_deciduous_forest", "terra_firme_forest",
      "rainforest_ombrophilous_forest", "clean_grassland", "cerrado_vegetation",
      "floodplain_forest", "anthropogenic_area", "seasonal_evergreen_forest",
      "seasonal_semideciduous_forest", "gallery_forest", "mixed_ombrophilous_forest",
      "igapo_forest", "restinga", "caatinga_vegetation", "rupestrian_grassland",
      "rocky_outcrop_vegetation", "amazonian_savanna", "high_altitude_grassland",
      "floodplain_grassland", "mangrove", "palm_grove", "campinarana_white_sand_vegetation",
      "aquatic_vegetation",

      # IUCN
      "EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE", "iucn_classified_as",

      # Genus
      "only_species_of_genus", "one_of_n_species_of_genus",
      "genus_with_highest_diversity", "one_of_genera_with_highest_diversity"

    ),

    en = c(

      # Taxon
      "belongs to", "family", "This species has no known common name", "It's commonly known as",
      "It's commonly known", "by at least", "common names", "such as", "and",

      # Distribution
      "A true rare gem", "it is only found in", "This species is found in several countries such as",
      "This species is", "to", "non-endemic", "endemic", "or", "Brazil North", "Brazil Northeast",
      "Brazil South", "Brazil Southeast", "Brazil West-Central",

      # Establishment means
      "This means that it is not found in its natural state anywhere else on Earth",
      "It is native to Brazil, meaning that it grows there naturally without human intervention",
      "In Brazil, it has been intentionally cultivated and managed by humans; this plant is therefore entirely dependent on human intervention to survive and reproduce",
      "In Brazil, it has become naturalised, meaning that it was introduced there but has adapted well, formed self-sustaining populations and reproduces on its own, without human assistance", "it is",

      # Introduced
      "This plant has also been introduced to",
      "This plant has also been introduced to several countries around the world like",

      # Phytogeographic domain
      "Amazon", "Atlantic Forest", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "It inhabits the", "It colonizes various habitats",
      "It also can be found in every biome in Brazil", "how lucky",

      # Vegetation types
      "where", "it grows mainly in a particular vegetation layer",
      "it grows mainly in particular vegetation layers",
      "shrubland Carrasco", "seasonal deciduous forest", "terra-firme forest",
      "rainforest", "herbaceous or grassland savanna", "fire-prone savanna-like Cerrado vegetation",
      "V\u00e1rzea forest", "anthropogenic area", "seasonal evergreen forest",
      "seasonal semideciduous forest", "gallery forest", "mixed ombrophilous forest",
      "seasonally flooded Igap\u00f3 forest", "white-sand coastal scrubland Restinga",
      "Caatinga seasonally dry forest", "rupestrian grassland",
      "rocky outcrop vegetation", "amazonian savanna", "high altitude grassland",
      "V\u00e1rzea forest", "mangrove", "palm grove", "white\u2011sand Campinarana vegetation",
      "aquatic vegetation",

      # IUCN
      "extinct (EX)", "extinct in the wild (EW)", "critically endangered (CR)", "endangered (EN)",
      "vulnerable (VU)", "near threatened (NT)", "least concern (LC)", "data deficient (DD)", "not evaluated (NE)",
      "The International Union for Conservation of Nature (IUCN) has classified this species as",

      # Genus
      "It is the only species of this genus in Brazil",
      "It is one of the few {n} species of this genus in Brazil",
      "This species is part of the genus with the greatest diversity in Brazil",
      "This species belongs to one of the genera with the greatest diversity in Brazil"
    ),

    pt = c(

      # Taxon
      "pertence \u00e0", "fam\u00edlia", "Esta esp\u00e9cie n\u00e3o tem nenhum nome popular conhecido", "\u00c9 comumente chamada de",
      "\u00c9 comumente conhecida", "por pelo menos", "nomes populares", "como", "e",

      # Distribution
      "Uma verdadeira joia rara", "ela s\u00f3 \u00e9 encontrada no estado", "Esta esp\u00e9cie \u00e9 encontrada em v\u00e1rios pa\u00edses como",
      "Esta esp\u00e9cie \u00e9", "do", "n\u00e3o end\u00eamica", "end\u00eamica", "ou", "norte do Brasil", "nordeste do Brasil",
      "sul do Brasil", "sudeste do Brasil", "centro-oeste do Brasil",

      # Establishment means
      "Isso significa que n\u00e3o \u00e9 encontrado em seu estado natural em nenhum outro lugar do planeta",
      "Ela \u00e9 nativa do Brasil, ou seja, cresce naturalmente no pa\u00eds sem interven\u00e7\u00e3o humana",
      "No Brasil, ela foi cultivada e manejada intencionalmente pelo homem; portanto, essa planta depende inteiramente da interven\u00e7\u00e3o humana para sobreviver e se reproduzir",
      "No Brasil, ela foi naturalizada, ou seja, foi introduzida no pa\u00eds, mas se adaptou bem, formou popula\u00e7\u00f5es aut\u00f4nomas e se reproduz sozinha, sem a ajuda do homem", "Ela \u00e9",

      # Introduced
      "Esta planta tamb\u00e9m foi introduzida em",
      "Esta planta tamb\u00e9m foi introduzida em v\u00e1rios pa\u00edses ao redor do mundo, como",

      # Phytogeographic domain
      "Amaz\u00f4nia", "Mata Atl\u00e2ntica", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Ela habita", "Ela coloniza v\u00e1rios habitats",
      "Ela tamb\u00e9m pode ser encontrada em todos os biomas do Brasil", "que sorte",

      # Vegetation types
      "onde", "ela cresce preferencialmente em uma camada vegetal espec\u00edfica",
      "ela cresce preferencialmente em camadas vegetais espec\u00edficas",
      "Carrasco", "Floresta Estacional Decidual", "Floresta de Terra Firme",
      "Floresta Ombr\u00f3fila", "Campo Limpo", "Cerrado sensu lato",
      "Floresta de V\u00e1rzea", "\u00c1rea Antr\u00f3pica",
      "Floresta Estacional Perenif\u00f3lia", "Floresta Estacional Semidecidual",
      "Floresta Ciliar ou Galeria", "Floresta Ombr\u00f3fila Mista", "Floresta de Igap\u00f3",
      "Restinga", "Caatinga sensu stricto", "Campo rupestre",
      "Vegeta\u00e7\u00e3o Sobre Afloramentos Rochosos", "Savana Amaz\u00f4nica", "Campo de Altitude",
      "Campo de V\u00e1rzea", "Manguezal", "Palmeiral", "Campinarana", "Vegeta\u00e7\u00e3o Aqu\u00e1tica",

      # IUCN
      "extinto (EX)", "extinto na natureza (EW)", "criticamente em perigo (CR)", "em perigo (EN)",
      "vulner\u00e1vel (VU)", "quase amea\u00e7ado (NT)", "pouco preocupante (LC)", "dados insuficientes (DD)",
      "n\u00e3o avaliado (NE)",
      "A Uni\u00e3o Internacional para a Conserva\u00e7\u00e3o da Natureza (IUCN) classificou esta esp\u00e9cie como",

      # Genus
      "\u00c9 a \u00fanica esp\u00e9cie desse g\u00eanero no Brasil",
      "\u00c9 uma das poucas {n} esp\u00e9cies desse g\u00eanero no Brasil",
      "Essa esp\u00e9cie faz parte do g\u00eanero com maior diversidade no Brasil",
      "Essa esp\u00e9cie pertence a um dos g\u00eaneros com maior diversidade no Brasil"
    ),

    es = c(

      # Taxon
      "pertenece a", "la familia", "Esta especie no tiene ning\u00fan nombre com\u00fan conocido",
      "Se la conoce com\u00fanmente como", "Se conoce com\u00fanmente como", "por al menos",
      "nombres comunes", "como", "y",

      # Distribution
      "Una verdadera joya rara", "solo se encuentra en ese estado",
      "Esta especie se encuentra en varios pa\u00edses como",
      "Esta especie es", "del", "no end\u00e9mica", "end\u00e9mica", "o", "norte de Brasil",
      "nordeste de Brasil", "sur de Brasil", "sudeste de Brasil", "centro-oeste de Brasil",

      # Establishment means
      "Esto significa que no se encuentra en su estado natural en ning\u00fan otro lugar del planeta",
      "Es aut\u00f3ctona de Brasil, es decir, crece all\u00ed de forma natural sin intervenci\u00f3n humana",
      "En Brasil, ha sido cultivada y gestionada intencionadamente por el ser humano, por lo que esta planta depende por completo de la intervenci\u00f3n humana para sobrevivir y reproducirse",
      "En Brasil se ha naturalizado, es decir, que fue introducida all\u00ed, pero se ha adaptado bien, ha formado poblaciones aut\u00f3nomas y se reproduce por s\u00ed sola, sin la ayuda del hombre", "Es",

      # Introduced
      "Esta planta tambi\u00e9n ha sido introducida en",
      "Esta planta tambi\u00e9n ha sido introducida en varios pa\u00edses del mundo, como",

      # Phytogeographic domain
      "Amazon\u00eda", "Mata Atl\u00e1ntica", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Habita", "Coloniza varios h\u00e1bitats",
      "Tambi\u00e9n se puede encontrar en todos los biomas de Brasil", "qu\u00e9 suerte",

      # Vegetation types
      "donde", "\u00abcrece preferentemente en un estrato vegetal concreto",
      "crece preferentemente en estratos vegetales concretos",
      "Carrasco (matorral)", "Bosque Estacional Deciduo", "Bosque de Tierra Firme",
      "Selva Ombr\u00f3fila", "Campo Limpio", "Cerrado",
      "Bosque de V\u00e1rzea", "\u00c1rea Antr\u00f3pica", "Bosque Estacional Perennifolio",
      "Bosque Estacional Semideciduo", "Bosque de Galer\u00eda",
      "Bosque Ombr\u00f3filo Mixto", "Bosque de Igap\u00f3", "Restinga",
      "Caatinga", "Campo Rupestre", "Vegetaci\u00f3n sobre Afloramientos Rochosos",
      "Sabana Amaz\u00f3nica", "Campo de Altitud", "Campo de V\u00e1rzea", "Manglar",
      "Palmeral", "vegetaci\u00f3n de Campinarana sobre arena blanca", "Vegetaci\u00f3n Acu\u00e1tica",

      # IUCN
      "extinto (EX)", "extinto en estado silvestre (EW)", "en peligro cr\u00edtico (CR)", "en peligro (EN)",
      "vulnerable (VU)", "casi amenazado (NT)", "preocupaci\u00f3n menor (LC)", "datos insuficientes (DD)",
      "no evaluado (NE)",
      "La Uni\u00f3n Internacional para la Conservaci\u00f3n de la Naturaleza (IUCN) ha clasificado esta especie como",

      # Genus
      "Es la \u00fanica especie de este g\u00e9nero en Brasil",
      "Es una de las pocas {n} especies de este g\u00e9nero en Brasil",
      "Esta especie forma parte del g\u00e9nero con mayor diversidad en Brasil",
      "Esta especie pertenece a uno de los g\u00e9neros con mayor diversidad en Brasil"
    ),

    fr = c(
      # Taxon
      "appartient \u00e0", "la famille", "Cette esp\u00e8ce n\u2019a aucun nom commun connu", "Elle est commun\u00e9ment appel\u00e9e",
      "Elle est commun\u00e9ment appel\u00e9e", "par au moins", "noms communs", "comme", "et",

      # Distribution
      "Un vrai joyau rare", "on ne la retrouve que dans l'\u00e9tat", "Cette esp\u00e8ce se trouve dans plusieurs pays tels que",
      "Cette esp\u00e8ce est", "du", "non end\u00e9mique", "end\u00e9mique", "ou", "nord du Br\u00e9sil", "nord-est du Br\u00e9sil",
      "sud du Br\u00e9sil", "sud-est du Br\u00e9sil", "centre-ouest du Br\u00e9sil",

      # Establishment means
      "Cela signifie qu'on ne la trouve \u00e0 l'\u00e9tat naturel nulle part ailleurs sur Terre",
      "Elle est native du Br\u00e9sil, c'est \u00e0 dire qu'elle y pousse naturellement sans intervention humaine",
      "Au Br\u00e9sil, elle a \u00e9t\u00e9 intentionnellement cultiv\u00e9e et g\u00e9r\u00e9e par l'homme, cette plante d\u00e9pend donc enti\u00e8rement de l'intervention humaine pour survivre et se reproduire",
      "Au Br\u00e9sil, elle a \u00e9t\u00e9 naturalis\u00e9e, c'est \u00e0 dire qu'elle y a \u00e9t\u00e9 introduite, mais s'est bien adapt\u00e9e, a form\u00e9 des populations autonomes et se reproduit seule, sans l'aide de l'homme", "Elle est",

      # Introduced
      "Cette plante a \u00e9galement \u00e9t\u00e9 introduite au",
      "Cette plante a \u00e9galement \u00e9t\u00e9 introduite dans plusieurs pays du monde, comme",

      # Phytogeographic domain
      "Amazonie", "For\u00eat Atlantique", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Elle habite", "Elle colonise des \u00e9cosyst\u00e8mes divers comme",
      "On peut aussi la trouver dans tous les biomes du Br\u00e9sil", "quelle chance",

      # Vegetation types
      "o\u00f9", "elle pousse de pr\u00e9f\u00e9rence dans une strate v\u00e9g\u00e9tale particuli\u00e8re",
      "elle pousse de pr\u00e9f\u00e9rence dans des strates v\u00e9g\u00e9tales particuli\u00e8res",
      "Carrasco (fourr\u00e9)", "for\u00eat tropicale semi-d\u00e9cidue", "for\u00eat amazonienne non inondable",
      "for\u00eat ombrophile", "savane herbac\u00e9e ou prairiale", "Cerrado",
      "for\u00eat de V\u00e1rzea", "zone anthropique", "for\u00eat tropicale saisonni\u00e8re perennifoli\u00e9e",
      "for\u00eat saisonni\u00e8re semi-d\u00e9cidue", "for\u00eat galerie",
      "for\u00eat ombrophile mixte", "for\u00eat d\u2019Igap\u00f3", "Restinga",
      "Caatinga", "prairie rupestre", "v\u00e9g\u00e9tation sur affleurements rocheux",
      "savane amazonienne", "prairie d\u2019altitude", "prairie de V\u00e1rzea", "mangrove",
      "palmeraie", "Campinarana (v\u00e9g\u00e9tation sur sable blanc)", "v\u00e9g\u00e9tation aquatique",

      # IUCN
      "\u00e9teint (EX)", "\u00e9teint \u00e0 l'\u00e9tat sauvage (EW)", "en danger critique (CR)", "en danger (EN)",
      "vuln\u00e9rable (VU)", "quasi menac\u00e9 (NT)", "pr\u00e9occupation mineure (LC)", "donn\u00e9es insuffisantes (DD)",
      "non \u00e9valu\u00e9 (NE)",
      "L'Union internationale pour la conservation de la nature (IUCN) a class\u00e9 cette esp\u00e8ce comme",

      # Genus
      "C'est la seule esp\u00e8ce de ce genre au Br\u00e9sil",
      "C'est l'une des seules {n} esp\u00e8ces de ce genre au Br\u00e9sil",
      "Cette esp\u00e8ce fait partie du genre avec la plus grande diversit\u00e9 au Br\u00e9sil",
      "Cette esp\u00e8ce appartient \u00e0 l'un des genres avec la plus grande diversit\u00e9 au Br\u00e9sil"
    )
  )
  return(dict)
}

.get_lab <- function(lang = "en", dict){
  lang <- match.arg(lang, choices = c("en", "pt", "es", "fr"))

  lab <- dict[[lang]]
  names(lab) <- dict$key

  # Convert nested list entries such as species_tbl/family_tbl into character vectors
  lab <- lapply(lab, function(x) {
    if (is.list(x)) {
      unlist(x, use.names = FALSE)
    } else {
      x
    }
  })

  lab
}

.tr_dict <- function(key, lang = "en", dict){
  # Validate and normalize lang
  lang <- tolower(trimws(as.character(lang)[1]))
  if (!lang %in% c("en", "pt", "es", "fr")) {
    lang <- "en"
  }
  # Search for translation
  result <- dict[dict$key == key, lang, drop = TRUE]

  return(as.character(result))
}

# Translate English names through a geographic dictionary, keeping the
# original name when it is missing from the dictionary ####
.tr_or_raw <- function(values, lang = "en", dict) {
  vapply(values, function(v) {
    key <- dict$key[dict$en %in% v]
    if (length(key) == 0) return(v)
    tr <- .tr_dict(key[1], lang, dict)
    if (length(tr) == 0 || is.na(tr[1])) v else tr[1]
  }, character(1), USE.NAMES = FALSE)
}

.tr_dict_vec <- function(keys, lang = "en",
                         dict = get("dict", envir = parent.frame())){
  sapply(keys, function(k) .tr_dict(k, lang, dict), USE.NAMES = FALSE)
}

.convert_acronym_br_state <- function(x){

  valid_states <- c("Acre" = "AC", "Alagoas" = "AL", "Amap\u00e1" = "AP", "Amazonas" = "AM",
                    "Bahia" = "BA", "Cear\u00e1" = "CE", "Distrito Federal" = "DF",
                    "Esp\u00edrito Santo" = "ES", "Goi\u00e1s" = "GO", "Maranh\u00e3o" = "MA",
                    "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
                    "Par\u00e1" = "PA", "Para\u00edba" = "PB", "Paran\u00e1" = "PR", "Pernambuco" = "PE",
                    "Piau\u00ed" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
                    "Rio Grande do Sul" = "RS", "Rond\u00f4nia" = "RO", "Roraima" = "RR",
                    "Santa Catarina" = "SC", "S\u00e3o Paulo" = "SP", "Sergipe" = "SE",
                    "Tocantins" = "TO")

  valid_states_full <- names(valid_states)
  valid_states_acronyms <- unname(valid_states)

  states_no_diacritics <- stringi::stri_trans_general(x, "Latin-ASCII")
  valid_states_full_no_diacritics <- stringi::stri_trans_general(valid_states_full, "Latin-ASCII")
  valid_states_acronyms_no_diacritics <- stringi::stri_trans_general(valid_states_acronyms, "Latin-ASCII")

  corrected_states <- character(length(x))

  for (i in seq_along(x)) {
    match_full <- match(states_no_diacritics[i], valid_states_full_no_diacritics)
    match_acronym <- match(states_no_diacritics[i], valid_states_acronyms_no_diacritics)

    if (!is.na(match_full)) {
      corrected_states[i] <- valid_states_full[match_full]
    } else if (!is.na(match_acronym)) {
      corrected_states[i] <- names(valid_states)[match_acronym]
    } else {
      corrected_states[i] <- x[i]
    }
  }

  return(corrected_states)
}

# Auxiliary functions to build HTML phrase and audio guides for arboretum_audios and arboretum_data
.save_phrase_html <- function(df,
                              function_use = c("_data", "_audios"),
                              ui_strings,
                              lang_button_label,
                              printed_lang,
                              html_phrases,
                              output_path,
                              verbose,
                              add_lang = NULL,
                              data_filename = NULL) {

  if (function_use == "_data") {
    ui_strings <- lapply(ui_strings, function(x) {
      # remove all *_audios entries
      x <- x[!grepl("_audios$", names(x))]
      names(x)[names(x) == "title_data"] <- "title"
      names(x)[names(x) == "subtitle_data"] <- "subtitle"
      x
    })
  } else if (function_use == "_audios") {
    ui_strings <- lapply(ui_strings, function(x) {
      # remove all *_data entries
      x <- x[!grepl("_data$", names(x))]
      names(x)[names(x) == "title_audios"] <- "title"
      names(x)[names(x) == "subtitle_audios"] <- "subtitle"
      x
    })
  }

  initial_lang <- printed_lang[1L]
  ui_strings_json <- jsonlite::toJSON(ui_strings[printed_lang], auto_unbox = TRUE)
  species_data_json <- if (function_use == "_data") {
    jsonlite::toJSON(df, na = "null", auto_unbox = TRUE)
  } else {
    "[]"
  }
  data_fn_js <- if (!is.null(data_filename) && nzchar(data_filename)) {
    .escape_html(data_filename)
  } else {
    "arboretum_data.xlsx"
  }
  lang_buttons_html <- .build_language_buttons(printed_lang, initial_lang, lang_button_label)
  index_html <- .build_species_index(df)
  cards_html <- .build_species_cards(function_use,
                                     df, printed_lang, html_phrases, initial_lang,
                                     add_lang = add_lang)

  html <- paste0(
    '<!DOCTYPE html>
<html lang="', .escape_html(ui_strings[[initial_lang]]$html_lang), '">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>', .escape_html(ui_strings[[initial_lang]]$title), '</title>
<style>
:root {
  --bg: #f6f3ee;
  --panel: #fffdf9;
  --ink: #1f2937;
  --muted: #6b7280;
  --line: #e5ded3;
  --accent: #2f6f57;
  --accent-soft: #e7f2ec;
  --shadow: 0 10px 30px rgba(0, 0, 0, 0.08);
}
* { box-sizing: border-box; }
html { scroll-behavior: smooth; }
body {
  margin: 0;
  font-family: Arial, Helvetica, sans-serif;
  color: var(--ink);
  background: var(--bg);
  line-height: 1.6;
}
.container {
  max-width: 1100px;
  margin: 0 auto;
  padding: 32px 20px 60px;
}
.hero {
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 18px;
  padding: 24px;
  box-shadow: var(--shadow);
  margin-bottom: 24px;
}
.hero-top {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 16px;
  flex-wrap: wrap;
}
.hero h1 {
  margin: 0 0 8px;
  font-size: 2rem;
}
.hero p {
  margin: 0;
  color: var(--muted);
}
.lang-switch {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}
.lang-btn {
  padding: 8px 12px;
  border: 1px solid #d4e4d4;
  border-radius: 10px;
  background: #f0f7f0;
  cursor: pointer;
  font-size: 0.85rem;
  font-weight: 600;
  color: var(--accent);
}
.lang-btn.active {
  background: var(--accent);
  color: #fff;
  border-color: var(--accent);
}
.search-wrap {
  position: sticky;
  top: 0;
  z-index: 10;
  background: linear-gradient(to bottom, var(--bg) 80%, rgba(246,243,238,0));
  padding: 16px 0 18px;
}
.search-box {
  width: 100%;
  padding: 14px 16px;
  border-radius: 14px;
  border: 1px solid var(--line);
  font-size: 1rem;
  background: #fff;
}
.index-panel {
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 18px;
  padding: 20px;
  box-shadow: var(--shadow);
  margin-bottom: 24px;
}
.index-panel h2 {
  margin-top: 0;
}
.index-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 10px;
}
.index-link {
  display: block;
  text-decoration: none;
  color: var(--accent);
  background: var(--accent-soft);
  border-radius: 12px;
  padding: 10px 12px;
  border: 1px solid #d4e7dc;
}
.cards {
  display: grid;
  gap: 18px;
}
.species-card {
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 18px;
  padding: 22px;
  box-shadow: var(--shadow);
}
.species-header h2 {
  margin: 0 0 4px;
  font-size: 1.5rem;
}
.family {
  margin: 0 0 14px;
  color: var(--muted);
  font-weight: 700;
  letter-spacing: 0.03em;
}
.lang-block + .lang-block {
  margin-top: 14px;
  padding-top: 14px;
  border-top: 1px solid var(--line);
}
.lang-block h3 {
  margin: 0 0 6px;
  font-size: 1rem;
  color: var(--accent);
}
.lang-block p {
  margin: 0;
}
.back-top {
  display: inline-block;
  margin-top: 16px;
  color: var(--muted);
  text-decoration: none;
  font-size: 0.95rem;
}
.empty-state {
  display: none;
  margin-top: 12px;
  color: var(--muted);
}
.footer-note {
  margin-top: 22px;
  color: var(--muted);
  font-size: 0.95rem;
}
.hidden {
  display: none !important;
}
.subtitle-link {
  color: inherit;
  font-weight: 700;
  text-decoration: none;
}
.subtitle-link:hover {
  text-decoration: underline;
}',

ifelse(function_use == "_audios",
       paste0('.record-root-wrap {
  margin-top: 14px;
  display: flex;
  gap: 10px;
  align-items: center;
  flex-wrap: wrap;
}
.record-root-status {
  color: var(--muted);
  font-size: 0.95rem;
}
.record-controls {
  display: flex;
  gap: 8px;
  align-items: center;
  flex-wrap: wrap;
  margin-top: 10px;
}
.record-btn,
.stop-record-btn {
  padding: 7px 11px;
  border: 1px solid #d4e4d4;
  border-radius: 10px;
  background: #f0f7f0;
  cursor: pointer;
  font-size: 0.85rem;
  font-weight: 600;
  color: var(--accent);
}
.stop-record-btn {
  background: #faf3f0;
}
.record-status {
  color: var(--muted);
  font-size: 0.9rem;
  min-width: 80px;
}'), paste0("\n")),

ifelse(function_use == "_data", paste0(
'.search-row {
  display: flex;
  gap: 8px;
  align-items: center;
}
.search-row .search-box {
  flex: 1;
  min-width: 0;
  width: auto;
}
.edit-toolbar {
  display: flex;
  gap: 6px;
  align-items: center;
  flex-wrap: wrap;
  padding: 8px 12px;
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 12px;
  margin-bottom: 10px;
}
.save-btn {
  padding: 6px 11px;
  border-radius: 9px;
  border: 1px solid;
  cursor: pointer;
  font-size: 0.8rem;
  font-weight: 600;
  white-space: nowrap;
  line-height: 1.4;
}
.save-btn-primary  { background: var(--accent); color: #fff; border-color: var(--accent); }
.save-btn-secondary { background: #f0f7f0; color: var(--accent); border-color: #d4e4d4; }
.save-btn-csv      { background: #f7f3f0; color: #6b5a3a; border-color: #e0d4c0; }
.save-status {
  font-size: 0.8rem;
  color: var(--muted);
  margin-right: auto;
  min-width: 60px;
}
.edit-toggle-btn {
  display: block;
  margin-top: 14px;
  padding: 6px 12px;
  border: 1px dashed var(--line);
  border-radius: 8px;
  background: transparent;
  color: var(--muted);
  cursor: pointer;
  font-size: 0.84rem;
}
.edit-toggle-btn:hover { border-color: var(--accent); color: var(--accent); }
.edit-panel {
  margin-top: 12px;
  border: 1px solid var(--line);
  border-radius: 12px;
  padding: 16px;
  background: #fafaf8;
}
.edit-section { margin-bottom: 14px; }
.edit-section:last-child { margin-bottom: 0; }
.edit-section-label {
  display: block;
  font-size: 0.78rem;
  font-weight: 700;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.05em;
  margin-bottom: 6px;
}
.edit-uses-header {
  display: grid;
  grid-template-columns: 52px 1fr 1fr;
  gap: 8px;
  font-size: 0.73rem;
  font-weight: 700;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.04em;
  margin-bottom: 6px;
}
.edit-lang-row {
  display: grid;
  grid-template-columns: 52px 1fr 1fr;
  gap: 8px;
  align-items: start;
  margin-bottom: 8px;
}
.edit-phrase-row { grid-template-columns: 52px 1fr; }
.edit-lang-label { font-size: 0.8rem; font-weight: 700; color: var(--accent); padding-top: 6px; }
.edit-ta {
  width: 100%;
  border: 1px solid var(--line);
  border-radius: 8px;
  padding: 7px 9px;
  font-size: 0.83rem;
  font-family: inherit;
  resize: vertical;
  background: #fff;
  color: var(--ink);
  line-height: 1.5;
  transition: border-color 0.15s;
}
.edit-ta:focus { outline: none; border-color: var(--accent); box-shadow: 0 0 0 2px rgba(47,111,87,0.12); }
.edit-hint { display: block; font-size: 0.75rem; color: #9ca3af; margin-top: 4px; }
.phrase-base, .phrase-extra { display: inline; }
'), paste0("\n")),

'</style>
</head>
<body>
<div class="container" id="top">
  <section class="hero">
    <div class="hero-top">
      <div class="hero-copy">
        <h1 id="pageTitle" data-i18n="title">', .escape_html(ui_strings[[initial_lang]]$title), '</h1>
        <p id="pageSubtitle">', subtitle_html(ui_strings, initial_lang), '</p>
      </div>
      <div class="lang-switch">', lang_buttons_html, '</div>',
ifelse(function_use == "_audios",
       paste0('
    </div>
    <div class="record-root-wrap">
      <button id="pickAudioRoot" class="lang-btn" type="button">\U{1f4c1} Choose audio folder</button>
      <span id="audioRootStatus" class="record-root-status"></span>
    </div>'), paste0("\n")),
'</section>

  <div class="search-wrap">',
ifelse(function_use == "_data", paste0(
'    <div id="editToolbar" class="edit-toolbar">
      <span class="save-status" id="saveStatus"></span>
      <button type="button" class="save-btn save-btn-csv" id="downloadCsvBtn">&#8681;&nbsp;CSV</button>
      <button type="button" class="save-btn save-btn-secondary" id="downloadXlsxBtn">&#8681;&nbsp;XLSX</button>
      <button type="button" class="save-btn save-btn-primary" id="saveXlsxBtn">&#128190;&nbsp;Save</button>
    </div>'), ""),
'    <div class="search-row">
      <input id="searchInput" class="search-box" type="text" placeholder="', .escape_html(ui_strings[[initial_lang]]$search_placeholder), '">
    </div>
  </div>

  <section class="index-panel">
    <h2 id="indexTitle" data-i18n="index_title">', .escape_html(ui_strings[[initial_lang]]$index_title), '</h2>
    <div id="indexGrid" class="index-grid">
      ', index_html, '
    </div>
    <p id="emptyState" class="empty-state" data-i18n="no_results">', .escape_html(ui_strings[[initial_lang]]$no_results), '</p>
  </section>

  <section id="cards" class="cards">
    ', cards_html, '
  </section>

  <p id="footerNote" class="footer-note" data-i18n="footer_note">', .escape_html(ui_strings[[initial_lang]]$footer_note), '</p>
</div>

<script>
(function () {
  const uiStrings = ', ui_strings_json, ';
  const generatedDate = "', .escape_html(as.character(Sys.Date())), '";
  let currentLang = "', initial_lang, '";
  let audioRootHandle = null;
  let mediaRecorder = null;
  let mediaStream = null;
  let recordedChunks = [];
  let currentSection = null;

  const input = document.getElementById("searchInput");
  const cards = Array.from(document.querySelectorAll(".species-card"));
  const links = Array.from(document.querySelectorAll(".index-link"));
  const emptyState = document.getElementById("emptyState");
  const langButtons = Array.from(document.querySelectorAll(".lang-btn"));
  const audioRootStatus = document.getElementById("audioRootStatus");

  function escapeHtml(text) {
    return String(text)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function setStatus(section, value) {
    const node = section ? section.querySelector(".record-status") : null;
    if (node) node.textContent = value || "";
  }

  function applyLanguage(lang) {
    const ui = uiStrings[lang] || uiStrings.en;
    currentLang = lang;

    document.documentElement.lang = ui.html_lang || lang;
    document.title = ui.title;
    document.getElementById("pageTitle").textContent = ui.title;
    document.getElementById("pageSubtitle").innerHTML =
      escapeHtml(ui.subtitle) + "<br>" +
      escapeHtml(ui.generated_with) + " <a href=\\"https://github.com/DBOSlab/aRboretum\\" target=\\"_blank\\" rel=\\"noopener noreferrer\\" class=\\"subtitle-link\\">aRboretum</a> " +
      generatedDate + ".";
    document.getElementById("indexTitle").textContent = ui.index_title;
    document.getElementById("footerNote").textContent = ui.footer_note;
    document.getElementById("searchInput").placeholder = ui.search_placeholder;
    document.getElementById("emptyState").textContent = ui.no_results;

    document.querySelectorAll("[data-i18n=\\"family\\"]").forEach(el => {
      el.textContent = ui.family;
    });

    document.querySelectorAll("[data-i18n=\\"back_to_top\\"]").forEach(el => {
      el.textContent = ui.back_to_top;
    });

    document.querySelectorAll(".lang-content").forEach(el => {
      el.classList.toggle("hidden", el.dataset.lang !== lang);
    });

    langButtons.forEach(btn => {
      btn.classList.toggle("active", btn.dataset.lang === lang);
    });
  }

  function applyFilter() {
    const q = input.value.trim().toLowerCase();
    let visibleCount = 0;

    cards.forEach(card => {
      const speciesName = (card.dataset.name || "").toLowerCase();
      const familyText = (card.querySelector(".family")?.textContent || "").toLowerCase();
      const visibleLangText = Array.from(card.querySelectorAll(".lang-content"))
        .filter(el => !el.classList.contains("hidden"))
        .map(el => el.textContent || "")
        .join(" ")
        .toLowerCase();

      const searchableText = [speciesName, familyText, visibleLangText].join(" ");
      const match = searchableText.includes(q);

      card.classList.toggle("hidden", !match);
      if (match) visibleCount += 1;
    });

    links.forEach(link => {
      const match = (link.dataset.name || "").toLowerCase().includes(q);
      link.classList.toggle("hidden", !match);
    });

    emptyState.style.display = visibleCount === 0 ? "block" : "none";
  }',
ifelse(function_use == "_audios",
       paste0('
  async function pickAudioRoot() {
    if (!window.showDirectoryPicker) {
      alert("Direct folder saving is supported in Chromium-based browsers when this page is served from localhost or HTTPS.");
      return;
    }

    try {
      audioRootHandle = await window.showDirectoryPicker();
      if (audioRootStatus) {
        audioRootStatus.textContent = "Folder selected: " + audioRootHandle.name;
      }
    } catch (err) {
      console.error(err);
    }
  }

  async function getOrCreateSubfolder(rootHandle, folderName) {
    return await rootHandle.getDirectoryHandle(folderName, { create: true });
  }

  async function saveRecordingToFolder(blob, sectionEl) {
    if (!audioRootHandle) {
      throw new Error("Please choose the audio root folder first.");
    }

    const family = sectionEl.dataset.family;
    const folderSpecies = sectionEl.dataset.folderSpecies;
    const langSuffix = sectionEl.dataset.langSuffix;
    const subfolderName = `${family}_${folderSpecies}_${langSuffix}`;
    const filename = `${family}_${folderSpecies}_${langSuffix}.webm`;

    const subfolder = await getOrCreateSubfolder(audioRootHandle, subfolderName);
    const fileHandle = await subfolder.getFileHandle(filename, { create: true });
    const writable = await fileHandle.createWritable();
    await writable.write(blob);
    await writable.close();
  }

  async function startRecording(sectionEl) {
    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
      alert("Audio recording is not supported in this browser.");
      return;
    }

    if (!audioRootHandle) {
      alert("Please choose the audio root folder first.");
      return;
    }

    if (mediaRecorder && mediaRecorder.state !== "inactive") {
      alert("A recording is already in progress.");
      return;
    }

    currentSection = sectionEl;
    recordedChunks = [];
    setStatus(sectionEl, "Recording...");

    try {
      mediaStream = await navigator.mediaDevices.getUserMedia({ audio: true });
      mediaRecorder = new MediaRecorder(mediaStream);

      mediaRecorder.ondataavailable = function(event) {
        if (event.data && event.data.size > 0) {
          recordedChunks.push(event.data);
        }
      };

      mediaRecorder.onstop = async function() {
        try {
          const blob = new Blob(recordedChunks, { type: mediaRecorder.mimeType || "audio/webm" });
          await saveRecordingToFolder(blob, currentSection);
          setStatus(currentSection, "Saved");
        } catch (err) {
          console.error(err);
          setStatus(currentSection, "Save failed");
        } finally {
          if (mediaStream) {
            mediaStream.getTracks().forEach(track => track.stop());
          }
          mediaStream = null;
          mediaRecorder = null;
          recordedChunks = [];
          currentSection = null;
        }
      };

      mediaRecorder.start();
    } catch (err) {
      console.error(err);
      setStatus(sectionEl, "Mic denied");
      if (mediaStream) {
        mediaStream.getTracks().forEach(track => track.stop());
      }
      mediaStream = null;
      mediaRecorder = null;
      recordedChunks = [];
      currentSection = null;
    }
  }

  function stopRecording(sectionEl) {
    if (!mediaRecorder || mediaRecorder.state === "inactive") {
      setStatus(sectionEl, "Idle");
      return;
    }
    setStatus(sectionEl, "Saving...");
    mediaRecorder.stop();
  }

  input.addEventListener("input", applyFilter);

  document.getElementById("pickAudioRoot")?.addEventListener("click", pickAudioRoot);

  document.querySelectorAll(".record-btn").forEach(btn => {
    btn.addEventListener("click", function () {
      const section = this.closest(".lang-block");
      if (section) startRecording(section);
    });
  });

  document.querySelectorAll(".stop-record-btn").forEach(btn => {
    btn.addEventListener("click", function () {
      const section = this.closest(".lang-block");
      stopRecording(section);
    });
  });'), paste0("\n")),

'langButtons.forEach(btn => {
    btn.addEventListener("click", function () {
      if (this.id !== "pickAudioRoot") {
        applyLanguage(this.dataset.lang);
      }
    });
  });',

'applyLanguage(currentLang);
',
ifelse(function_use == "_data", paste0(

'  // ================================================================
  // Data Editor
  // ================================================================
  let speciesData = ', species_data_json, ';
  const dataFilename = "', data_fn_js, '";
  let xlsxFileHandle = null;

  function setUnsaved() {
    const stat = document.getElementById("saveStatus");
    if (stat) stat.textContent = "\u25cf Unsaved changes";
  }
  function setSaved(msg) {
    const stat = document.getElementById("saveStatus");
    if (stat) stat.textContent = msg || "\u2713 Saved";
  }

  // Toggle edit panels
  document.querySelectorAll(".edit-toggle-btn").forEach(btn => {
    btn.addEventListener("click", function () {
      const panel = document.getElementById(this.dataset.target);
      if (!panel) return;
      if (panel.hasAttribute("hidden")) {
        panel.removeAttribute("hidden");
        this.innerHTML = "\u00d7 Close editing";
      } else {
        panel.setAttribute("hidden", "");
        this.innerHTML = "&#9998; Edit data fields";
      }
    });
  });

  // Live-update phrase spans when editable fields change
  document.querySelectorAll(".edit-field").forEach(ta => {
    ta.addEventListener("input", function () {
      const row  = parseInt(this.dataset.row, 10);
      const col  = this.dataset.col;
      const lang = this.dataset.lang || "";
      const kind = this.dataset.kind || "";
      if (speciesData[row]) speciesData[row][col] = this.value;
      setUnsaved();
      if (kind === "plant" || kind === "notes") refreshExtraPhrase(row, lang);
      else if (kind === "addlang" || kind === "fullphrase") refreshBasePhrase(row, lang, this.value);
    });
  });

  function normPhrase(s) {
    s = (s || "").trim();
    return s && !/[.!?;:]$/.test(s) ? s + "." : s;
  }

  function refreshExtraPhrase(row, lang) {
    const plantTA = document.querySelector(`.edit-field[data-row="${row}"][data-lang="${lang}"][data-kind="plant"]`);
    const notesTA = document.querySelector(`.edit-field[data-row="${row}"][data-lang="${lang}"][data-kind="notes"]`);
    const parts = [];
    if (plantTA && plantTA.value.trim()) parts.push(normPhrase(plantTA.value.trim()));
    if (notesTA && notesTA.value.trim()) parts.push(normPhrase(notesTA.value.trim()));
    const extraSpan = document.querySelector(`.phrase-extra[data-row="${row}"][data-lang="${lang}"]`);
    if (!extraSpan) return;
    const baseSpan = document.querySelector(`.phrase-base[data-row="${row}"][data-lang="${lang}"]`);
    const hasBase  = baseSpan && baseSpan.textContent.trim().length > 0;
    const newExtra = parts.join(" ");
    extraSpan.textContent = (hasBase && newExtra) ? " " + newExtra : newExtra;
  }

  function refreshBasePhrase(row, lang, val) {
    const span = document.querySelector(`.phrase-base[data-row="${row}"][data-lang="${lang}"]`);
    if (span) span.textContent = val.replace(/<\\/?i>/g, "").trim();
    refreshExtraPhrase(row, lang);
  }

  function collectUpdates() {
    const data = speciesData.map(r => Object.assign({}, r));
    document.querySelectorAll(".edit-field").forEach(ta => {
      const row = parseInt(ta.dataset.row, 10);
      const col = ta.dataset.col;
      if (data[row]) data[row][col] = ta.value.trim() || null;
    });
    return data;
  }

  function csvEscape(v) {
    if (v == null) return "";
    const s = String(v);
    return (s.indexOf(",") >= 0 || s.indexOf(\'"\') >= 0 || s.indexOf("\\n") >= 0)
      ? \'"\' + s.replace(/"/g, \'""\') + \'"\' : s;
  }

  function triggerDownload(blob, fname) {
    const url = URL.createObjectURL(blob);
    const a   = Object.assign(document.createElement("a"), { href: url, download: fname });
    document.body.appendChild(a); a.click(); document.body.removeChild(a);
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  function downloadCSV() {
    const data    = collectUpdates();
    if (!data.length) return;
    const cols    = Object.keys(data[0]);
    const lines   = [cols.map(csvEscape).join(","),
                     ...data.map(r => cols.map(c => csvEscape(r[c])).join(","))];
    const csvName = dataFilename.replace(/\\.xlsx$/i, ".csv");
    triggerDownload(new Blob(["\ufeff" + lines.join("\\n")], { type: "text/csv;charset=utf-8;" }),
                    csvName);
    setSaved("\u2713 CSV downloaded");
  }

  function loadSheetJS(cb) {
    if (window.XLSX) { cb(); return; }
    const s = document.createElement("script");
    s.src = "https://cdn.sheetjs.com/xlsx-0.20.3/package/dist/xlsx.full.min.js";
    s.onload = cb;
    s.onerror = () => setSaved("\u26a0 XLSX unavailable \u2014 try CSV");
    document.head.appendChild(s);
  }

  function buildWorkbook() {
    const wb = XLSX.utils.book_new();
    XLSX.utils.book_append_sheet(wb, XLSX.utils.json_to_sheet(collectUpdates()), "aRboretum");
    return wb;
  }

  function downloadXLSX() {
    loadSheetJS(() => {
      XLSX.writeFile(buildWorkbook(), dataFilename);
      setSaved("\u2713 XLSX downloaded");
    });
  }

  // ----------------------------------------------------------------
  // IndexedDB: persist FileSystemFileHandle across page reloads
  // ----------------------------------------------------------------
  const handleKey = "aRboretum_fh_" + dataFilename;

  function _idbOpen() {
    return new Promise((res, rej) => {
      const r = indexedDB.open("aRboretum", 1);
      r.onupgradeneeded = () => r.result.createObjectStore("fh");
      r.onsuccess = () => res(r.result);
      r.onerror   = () => rej(r.error);
    });
  }
  function _idbPut(key, val) {
    return _idbOpen().then(db => new Promise((res, rej) => {
      const tx = db.transaction("fh", "readwrite");
      tx.objectStore("fh").put(val, key);
      tx.oncomplete = res; tx.onerror = () => rej(tx.error);
    }));
  }
  function _idbGet(key) {
    return _idbOpen().then(db => new Promise((res, rej) => {
      const tx  = db.transaction("fh", "readonly");
      const req = tx.objectStore("fh").get(key);
      req.onsuccess = () => res(req.result); req.onerror = () => rej(req.error);
    }));
  }

  // On page load: silently restore handle when permission is already granted
  _idbGet(handleKey).then(h => {
    if (!h || !h.queryPermission) return;
    return h.queryPermission({ mode: "readwrite" }).then(p => {
      if (p === "granted") xlsxFileHandle = h;
    });
  }).catch(() => {});

  // Resolve to a writable handle (re-requesting permission inside a user
  // gesture if needed), or null when no handle has been stored yet.
  function _resolveHandle() {
    if (!xlsxFileHandle) return Promise.resolve(null);
    return xlsxFileHandle.queryPermission({ mode: "readwrite" }).then(p => {
      if (p === "granted") return xlsxFileHandle;
      if (!xlsxFileHandle.requestPermission) return null;
      return xlsxFileHandle.requestPermission({ mode: "readwrite" }).then(r =>
        r === "granted" ? xlsxFileHandle : null
      );
    }).catch(() => null);
  }

  function _writeBlob(blob) {
    _resolveHandle().then(h => {
      if (h) {
        h.createWritable()
          .then(w => w.write(blob).then(() => w.close()))
          .then(() => setSaved("\u2713 Saved"))
          .catch(() => { triggerDownload(blob, dataFilename); setSaved("\u2713 Downloaded"); });
      } else if (window.showSaveFilePicker) {
        // When opened as file://, the dialog starts in the same folder as
        // this HTML file \u2014 i.e. the dir/ that arboretum_data() wrote to.
        window.showSaveFilePicker({
          suggestedName: dataFilename,
          types: [{ description: "Excel Workbook", accept: { "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet": [".xlsx"] } }]
        }).then(h2 => {
          xlsxFileHandle = h2;
          _idbPut(handleKey, h2).catch(() => {});
          return h2.createWritable().then(w => w.write(blob).then(() => w.close()));
        }).then(() => setSaved("\u2713 Saved"))
          .catch(() => {});
      } else {
        triggerDownload(blob, dataFilename);
        setSaved("\u2713 Downloaded");
      }
    });
  }

  function saveToFile() {
    loadSheetJS(() => {
      const wb  = buildWorkbook();
      const out = XLSX.write(wb, { type: "array", bookType: "xlsx" });
      _writeBlob(new Blob([out], { type: "application/octet-stream" }));
    });
  }

  document.getElementById("downloadCsvBtn")?.addEventListener("click",  downloadCSV);
  document.getElementById("downloadXlsxBtn")?.addEventListener("click", downloadXLSX);
  document.getElementById("saveXlsxBtn")?.addEventListener("click",     saveToFile);
'), ""),

'})();
</script>
</body>
</html>'
  )

  writeLines(html, con = output_path, useBytes = TRUE)

  if (verbose) {
    message("Phrases saved to: ", output_path)
  }
}

.build_species_cards <- function(function_use,
                                 df,
                                 printed_lang,
                                 html_phrases,
                                 initial_lang,
                                 add_lang = NULL) {
  cards <- vector("character", length(df$taxonName))

  std_langs <- intersect(printed_lang, c("pt", "en", "fr", "es"))
  add_langs  <- setdiff(printed_lang, c("pt", "en", "fr", "es"))

  for (i in seq_along(df$taxonName)) {
    species_name   <- .normalize_text(df$taxonName[i])
    family_name    <- .normalize_text(df$family[i])
    species_id     <- .slugify(species_name)
    folder_species <- .folder_species_name(species_name)
    family_upper   <- toupper(family_name)
    row_idx        <- i - 1L

    lang_blocks <- character(0)

    for (lang in printed_lang) {
      base_text <- html_phrases[[lang]][[species_name]]
      base_text <- gsub("[<]i[>]|[<][/]i[>]", "", base_text)
      base_text <- .normalize_text(base_text, ensure_period = FALSE)

      extra_text <- .get_extra_phrase_text(df, i, lang)

      full_text <- base_text
      if (!is.null(extra_text)) {
        full_text <- paste(full_text, extra_text)
      }
      full_text <- .normalize_text(full_text, ensure_period = FALSE)

      hidden_class <- if (lang == initial_lang) "" else " hidden"
      lang_suffix  <- .lang_suffix(lang)

      # For _data: split phrase into base + extra spans for live editing
      if (function_use == "_data") {
        base_disp  <- if (!is.na(base_text)  && nzchar(base_text))  base_text  else ""
        extra_disp <- if (!is.null(extra_text) && nzchar(extra_text)) extra_text else ""
        sep        <- if (nzchar(base_disp) && nzchar(extra_disp)) " " else ""
        phrase_p   <- paste0(
          '<p>',
          '<span class="phrase-base" data-row="', row_idx, '" data-lang="', .escape_html(lang), '">',
          .escape_html(base_disp), '</span>',
          sep,
          '<span class="phrase-extra" data-row="', row_idx, '" data-lang="', .escape_html(lang), '">',
          .escape_html(extra_disp), '</span>',
          '</p>'
        )
      } else {
        phrase_p <- paste0('<p>', .escape_html(if (!is.na(full_text)) full_text else ""), '</p>')
      }

      lang_blocks <- c(
        lang_blocks,
        paste0(
          '<section class="lang-block lang-content', hidden_class, '" ',
          'data-lang="', .escape_html(lang), '" ',
          'data-species="', .escape_html(species_name), '" ',
          'data-family="', .escape_html(family_upper), '" ',
          'data-folder-species="', .escape_html(folder_species), '" ',
          'data-lang-suffix="', .escape_html(lang_suffix), '">',
          '<h3>', .escape_html(.lang_label(lang)), '</h3>',
          phrase_p,
          ifelse(function_use == "_audios",
                 paste0(
                   '<div class="record-controls">',
                   '<button type="button" class="record-btn">\U{1f399} Record</button>',
                   '<button type="button" class="stop-record-btn">\u23f9 Stop</button>',
                   '<span class="record-status"></span>',
                   '</div>'), paste0("\n")),
          '</section>'
        )
      )
    }

    # Build edit panel (only for _data function)
    edit_panel_html <- ""
    if (function_use == "_data") {
      vern_val <- if (!.is_missing_text(df$FFB.vernacularName[i])) df$FFB.vernacularName[i] else ""

      lang_field_rows <- paste(vapply(std_langs, function(lang) {
        sfx  <- .lang_suffix(lang)
        pcol <- paste0("plant_uses_", sfx)
        ncol <- paste0("free_notes_",  sfx)
        pval <- if (pcol %in% names(df) && !.is_missing_text(df[[pcol]][i])) df[[pcol]][i] else ""
        nval <- if (ncol  %in% names(df) && !.is_missing_text(df[[ncol]][i]))  df[[ncol]][i]  else ""
        paste0(
          '<div class="edit-lang-row">',
          '<span class="edit-lang-label">', .escape_html(.lang_label(lang)), '</span>',
          '<div>',
          '<textarea class="edit-field edit-ta" rows="3"',
          ' data-row="', row_idx, '" data-col="', pcol, '"',
          ' data-lang="', .escape_html(lang), '" data-kind="plant">',
          .escape_html(pval), '</textarea></div>',
          '<div>',
          '<textarea class="edit-field edit-ta" rows="3"',
          ' data-row="', row_idx, '" data-col="', ncol, '"',
          ' data-lang="', .escape_html(lang), '" data-kind="notes">',
          .escape_html(nval), '</textarea></div>',
          '</div>'
        )
      }, character(1)), collapse = "\n")

      # Editable full phrase per built-in language; stored in full_phrases_*
      full_phrase_rows <- paste(vapply(std_langs, function(lang) {
        fcol <- paste0("full_phrases_", .lang_suffix(lang))
        fval <- if (fcol %in% names(df) && !.is_missing_text(df[[fcol]][i])) {
          df[[fcol]][i]
        } else if (!.is_missing_text(html_phrases[[lang]][[species_name]])) {
          html_phrases[[lang]][[species_name]]
        } else {
          ""
        }
        paste0(
          '<div class="edit-lang-row edit-phrase-row">',
          '<span class="edit-lang-label">', .escape_html(.lang_label(lang)), '</span>',
          '<textarea class="edit-field edit-ta" rows="4"',
          ' data-row="', row_idx, '" data-col="', fcol, '"',
          ' data-lang="', .escape_html(lang), '" data-kind="fullphrase">',
          .escape_html(fval), '</textarea>',
          '</div>'
        )
      }, character(1)), collapse = "\n")

      full_phrase_html <- if (length(std_langs) > 0) {
        paste0(
          '<div class="edit-section">',
          '<label class="edit-section-label">Full phrases (full_phrases_*)</label>',
          '<small class="edit-hint" style="margin-bottom:6px">',
          '&#8505; Automatically generated phrases. Edit them freely; stored phrases ',
          'are kept on the next runs. Clear a field to regenerate it. ',
          'Plant uses and free notes are appended after the phrase.',
          '</small>',
          full_phrase_rows,
          '</div>'
        )
      } else ""

      # Show add_lang textarea when add_lang was specified, even if no phrases
      # have been entered yet (all NA) — the user needs to type them here first
      add_lang_html <- if (!is.null(add_lang) && "full_phrases_ADD_LANGUAGE" %in% names(df)) {
        acol   <- "full_phrases_ADD_LANGUAGE"
        aval   <- if (!.is_missing_text(df[[acol]][i])) df[[acol]][i] else ""
        alabel <- .lang_label(add_lang)
        paste0(
          '<div class="edit-section">',
          '<label class="edit-section-label">Full phrase (', .escape_html(alabel), ')</label>',
          '<small class="edit-hint" style="margin-bottom:6px">',
          '&#8505; Paste the complete translation for ', .escape_html(alabel), '. ',
          'Changes will be used on the next arboretum_data() run.',
          '</small>',
          '<textarea class="edit-field edit-ta" rows="5"',
          ' data-row="', row_idx, '" data-col="', acol, '"',
          ' data-lang="', .escape_html(add_lang), '" data-kind="addlang">',
          .escape_html(aval), '</textarea>',
          '</div>'
        )
      } else ""

      edit_panel_html <- paste0(
        '<button type="button" class="edit-toggle-btn" data-target="edit-panel-', row_idx, '">',
        '&#9998; Edit data fields</button>',
        '<div class="edit-panel" id="edit-panel-', row_idx, '" hidden>',
        '<div class="edit-section">',
        '<label class="edit-section-label">Common names (FFB.vernacularName)</label>',
        '<textarea class="edit-field edit-ta" rows="2"',
        ' data-row="', row_idx, '" data-col="FFB.vernacularName" data-kind="vernacular">',
        .escape_html(vern_val), '</textarea>',
        '<small class="edit-hint">',
        '&#8505; Vernacular name changes take effect on the next arboretum_data() run.',
        '</small>',
        '</div>',
        '<div class="edit-section">',
        '<div class="edit-uses-header"><span></span><span>Plant uses</span><span>Free notes</span></div>',
        lang_field_rows,
        '</div>',
        full_phrase_html,
        add_lang_html,
        '</div>'
      )
    }

    cards[i] <- paste0(
      '<article class="species-card" id="', species_id, '" data-name="', .escape_html(tolower(species_name)), '">',
      '<div class="species-header">',
      '<h2>', .escape_html(species_name), '</h2>',
      '<p class="family"><span data-i18n="family">Family</span>: ', .escape_html(family_name), '</p>',
      '</div>',
      paste(lang_blocks, collapse = "\n"),
      edit_panel_html,
      '<a class="back-top" href="#top" data-i18n="back_to_top">Back to top</a>',
      '</article>'
    )
  }

  paste(cards, collapse = "\n")
}

.lang_label <- function(lang) {
  switch(lang,
         pt = "Portugu\u00eas",
         en = "English",
         fr = "Fran\u00e7ais",
         es = "Espa\u00f1ol",
         toupper(lang))
}

.lang_suffix <- function(lang) {
  switch(lang,
         pt = "PT",
         en = "EN",
         fr = "FR",
         es = "ES",
         toupper(lang))
}

.is_missing_text <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))
}

.normalize_text <- function(x, ensure_period = FALSE) {
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) {
    return(NA_character_)
  }
  if (ensure_period && !grepl("[.!?;:]$", x)) {
    x <- paste0(x, ".")
  }
  x
}

.escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.folder_species_name <- function(species_name) {
  species_name <- trimws(as.character(species_name))
  species_name <- gsub("\\s+", "_", species_name)
  species_name
}

.slugify <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  x
}

subtitle_html <- function(ui_strings, lang) {
  subtitle <- ui_strings[[lang]]$subtitle
  generated_with <- ui_strings[[lang]]$generated_with
  date_string <- as.character(Sys.Date())

  paste0(
    .escape_html(subtitle),
    "<br>",
    .escape_html(generated_with), " ",
    '<a href="https://github.com/DBOSlab/aRboretum" target="_blank" rel="noopener noreferrer" class="subtitle-link">aRboretum</a> ',
    .escape_html(date_string),
    "."
  )
}

.get_extra_phrase_text <- function(df, row_index, lang) {
  suffix <- .lang_suffix(lang)
  plant_col <- paste0("plant_uses_", suffix)
  notes_col <- paste0("free_notes_", suffix)

  extras <- character(0)

  if (plant_col %in% names(df)) {
    plant_val <- df[[plant_col]][row_index]
    if (!.is_missing_text(plant_val)) {
      plant_val <- .normalize_text(plant_val, ensure_period = TRUE)
      if (!is.na(plant_val)) {
        extras <- c(extras, plant_val)
      }
    }
  }

  if (notes_col %in% names(df)) {
    notes_val <- df[[notes_col]][row_index]
    if (!.is_missing_text(notes_val)) {
      notes_val <- .normalize_text(notes_val, ensure_period = TRUE)
      if (!is.na(notes_val)) {
        extras <- c(extras, notes_val)
      }
    }
  }

  if (length(extras) == 0) {
    return(NULL)
  }

  paste(extras, collapse = " ")
}

.build_language_buttons <- function(printed_lang,
                                    initial_lang,
                                    lang_button_label) {
  if (length(printed_lang) <= 1L) {
    return("")
  }

  btns <- vapply(printed_lang, function(lang) {
    active <- if (lang == initial_lang) ' class="lang-btn active"' else ' class="lang-btn"'

    label <- unname(lang_button_label[lang])
    if (length(label) == 0 || is.na(label) || !nzchar(trimws(label))) {
      label <- toupper(lang)
    }

    paste0(
      "<button", active, ' data-lang="', lang, '">',
      .escape_html(label),
      "</button>"
    )
  }, character(1))

  paste(btns, collapse = "\n")
}

.build_species_index <- function(df) {
  ids <- vapply(df$taxonName, .slugify, character(1))

  searchable_text <- vapply(seq_len(nrow(df)), function(i) {
    extras <- unlist(df[i, c(
      intersect(
        c(
          "plant_uses_EN", "plant_uses_PT", "plant_uses_FR", "plant_uses_ES",
          "free_notes_EN", "free_notes_PT", "free_notes_FR", "free_notes_ES"
        ),
        names(df)
      )
    )], use.names = FALSE)

    extras <- extras[!is.na(extras) & nzchar(trimws(extras))]
    paste(
      c(df$taxonName[i], df$family[i], extras),
      collapse = " "
    )
  }, character(1))

  links <- paste0(
    '<a class="index-link" data-name="', .escape_html(tolower(searchable_text)), '" href="#', ids, '">',
    .escape_html(df$taxonName),
    "</a>"
  )

  paste(links, collapse = "\n")
}

.ui_strings <- function() {
  ui_strings <- list(
    en = list(
      html_lang = "en",
      title_audios = "Personal Audio Recording Guide",
      title_data = "Phrases Generating Guide",
      subtitle_audios = "Use this file to record your own species audios before generating the HTML labels and minisite.",
      subtitle_data = "Use this file to check the automatically generated phrases before recording audios and generating the HTML labels and minisite.",
      search_placeholder = "Search species or family name...",
      index_title = "Index",
      no_results = "No species matched your search.",
      back_to_top = "Back to top",
      footer_note = "Use your browser search or the search field above to jump quickly across species.",
      family = "Family",
      generated_with = "Generated with"
    ),
    pt = list(
      html_lang = "pt",
      title_audios = "Guia para Grava\u00e7\u00e3o de \u00c1udios Pessoais",
      title_data = "Guia de Gera\u00e7\u00e3o de Frases",
      subtitle_audios = "Use este arquivo para gravar seus pr\u00f3prios \u00e1udios das esp\u00e9cies antes de gerar os r\u00f3tulos em HTML e o minisite.",
      subtitle_data = "Use este arquivo para verificar as frases geradas automaticamente antes de gravar os \u00e1udios e gerar as etiquetas HTML e o minisite.",
      search_placeholder = "Pesquisar nome da esp\u00e9cie ou fam\u00edlia...",
      index_title = "\u00cdndice",
      no_results = "Nenhuma esp\u00e9cie corresponde \u00e0 sua busca.",
      back_to_top = "Voltar ao topo",
      footer_note = "Use a busca do navegador ou o campo acima para navegar rapidamente entre as esp\u00e9cies.",
      family = "Fam\u00edlia",
      generated_with = "Gerado com"
    ),
    fr = list(
      html_lang = "fr",
      title_audios = "Guide d\u2019Enregistrement Audio Personnel",
      title_data = "Guide de g\u00e9n\u00e9ration de phrases",
      subtitle_audios = "Utilisez ce fichier pour enregistrer vos propres audios d\u2019esp\u00e8ces avant de g\u00e9n\u00e9rer les \u00e9tiquettes HTML et le minisite.",
      subtitle_data = "Utilisez ce fichier pour v\u00e9rifier les phrases g\u00e9n\u00e9r\u00e9es automatiquement avant d\u2019enregistrer les audios et de g\u00e9n\u00e9rer les \u00e9tiquettes HTML et le minisite.",
      search_placeholder = "Rechercher le nom de l\u2019esp\u00e8ce ou de la famille...",
      index_title = "Index",
      no_results = "Aucune esp\u00e8ce ne correspond \u00e0 votre recherche.",
      back_to_top = "Retour en haut",
      footer_note = "Utilisez la recherche du navigateur ou le champ ci-dessus pour naviguer rapidement entre les esp\u00e8ces.",
      family = "Famille",
      generated_with = "G\u00e9n\u00e9r\u00e9 avec"
    ),
    es = list(
      html_lang = "es",
      title_audios = "Gu\u00eda para Grabar Audios Personales",
      title_data = "Gu\u00eda de Generaci\u00f3n de Frases",
      subtitle_audios = "Use este archivo para grabar sus propios audios de especies antes de generar las etiquetas HTML y el minisitio.",
      subtitle_data = "Use este archivo para revisar las frases generadas autom\u00e1ticamente antes de grabar los audios y generar las etiquetas HTML y el minisite.",
      search_placeholder = "Buscar nombre de la especie o familia...",
      index_title = "\u00cdndice",
      no_results = "Ninguna especie coincide con su b\u00fasqueda.",
      back_to_top = "Volver arriba",
      footer_note = "Use la b\u00fasqueda del navegador o el campo superior para navegar r\u00e1pidamente entre las especies.",
      family = "Familia",
      generated_with = "Generado con"
    )
  )
}


# Secondary function to find related country for each botanical subdivision ####
.botdiv_to_countries <- function(x, i){

  if (is.na(x[[i]]) || !nzchar(x[[i]])) return(NA_character_)

  # Load botanical subdivisions from internal package data
  botregions <- get("botregions", envir = as.environment("package:aRboretum"))

  temp <- strsplit(x[[i]], " [|] ")[[1]]

  for (n in seq_along(temp)) {
    tf <- botregions$botanical_division %in% temp[n]
    if (length(botregions$country[tf]) == 0) {
      # Use strtrim to limit the length of each character/botanical region
      # because POWO has limited chars.
      bot_temp <- strtrim(botregions$botanical_division, 20)
      tf <- bot_temp %in% temp[n]
      if (length(botregions$country[tf]) > 0) {
        temp[n] <- botregions$country[tf]
      }
    } else {
      if (any(!botregions$country[tf] %in% temp[n])) {
        if (length(botregions$country[tf]) > 1) {
          temp[n] <- paste(botregions$country[tf], collapse = " | ")
        } else {
          temp[n] <- botregions$country[tf]
        }
      }
    }
  }
  temp <- sort(unique(temp))
  x[[i]] <- paste(temp, collapse = " | ")

  return(x[[i]])
}

# Auxiliary function to add genus richness curiosity notes
.add_genus_curiosity_notes <- function(result_merged,
                                       max_diversity,
                                       n_max_genera) {

  # Get the language dictionary
  dict <- .dict()

  for (i in 1:nrow(result_merged)) {
    n_sp <- result_merged$FFB.genusRichness[i]
    if (is.na(n_sp) || n_sp == 0) next

    for (lang_code in c("EN", "PT", "ES", "FR")) {
      lang_lower <- tolower(lang_code)
      col_name <- paste0("free_notes_", lang_code)

      # Rarity genus phrase
      if (n_sp == 1) {
        phrase_genus <- .tr_dict("only_species_of_genus", lang_lower, dict)
      } else if (n_sp >= 2 && n_sp <= 5) {
        template <- .tr_dict("one_of_n_species_of_genus", lang_lower, dict)
        phrase_genus <- gsub("\\{n\\}", n_sp, template)
      } else {
        phrase_genus <- ""
      }

      #Genus diversity phrase
      if (n_sp == max_diversity) {
        if (n_max_genera == 1) {
          p2 <- .tr_dict("genus_with_highest_diversity", lang_lower, dict)
        } else {
          p2 <- .tr_dict("one_of_genera_with_highest_diversity", lang_lower, dict)
        }
        if (nchar(phrase_genus) > 0) {
          phrase_genus <- paste(phrase_genus, p2, collapse = " ")
        } else {
          phrase_genus <- p2
        }
      }

      if (nchar(phrase_genus) == 0) next

      existing <- result_merged[i, col_name]
      if (is.na(existing) || existing == "") {
        result_merged[i, col_name] <- phrase_genus
      } else {
        result_merged[i, col_name] <- paste0(phrase_genus, "<br><br>", existing)
      }
    }
  }
  return(result_merged)
}


# IUCN Red List categories, keyed by category code ####
# Old Lower Risk subcategories (pre-2001 assessments) are mapped onto their
# current equivalents
.iucn_categories <- function() {
  c(EX = "Extinct",
    EW = "Extinct in the Wild",
    RE = "Regionally Extinct",
    CR = "Critically Endangered",
    EN = "Endangered",
    VU = "Vulnerable",
    NT = "Near Threatened",
    LC = "Least Concern",
    DD = "Data Deficient",
    NE = "Not Evaluated",
    `LR/cd` = "Near Threatened",
    `LR/nt` = "Near Threatened",
    `LR/lc` = "Least Concern")
}

# Extract the category code from a stored status such as "Endangered (EN)",
# a bare code such as "EN", or a label such as "endangered" ####
.iucn_code <- function(x) {
  cats <- .iucn_categories()
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
    s <- trimws(s)
    in_parens <- regmatches(s, regexpr("(?<=\\()[A-Za-z/]+(?=\\)\\s*$)", s, perl = TRUE))
    code <- if (length(in_parens) == 1) in_parens else s
    if (toupper(code) %in% toupper(names(cats))) {
      code <- names(cats)[toupper(names(cats)) %in% toupper(code)][1]
    } else if (tolower(s) %in% tolower(cats)) {
      code <- names(cats)[tolower(cats) %in% tolower(s)][1]
    } else {
      return(NA_character_)
    }
    if (grepl("^LR/", code)) {
      code <- names(cats)[match(cats[[code]], cats)]
    }
    code
  }, character(1), USE.NAMES = FALSE)
}

# Format a category code as it is stored in the IUCN.status column ####
.iucn_label <- function(code) {
  code <- .iucn_code(code)
  cats <- .iucn_categories()
  ifelse(is.na(code), NA_character_, paste0(cats[code], " (", code, ")"))
}

# Retrieve the global IUCN Red List status for a vector of taxon names ####
# Two sources are supported:
#   - "redlist": the official IUCN Red List API v4 through the rredlist package,
#     which requires a free API token (https://api.iucnredlist.org) stored in
#     the IUCN_REDLIST_KEY environment variable;
#   - "gbif": the IUCN Red List checklist mirrored by GBIF, which needs no token.
# With source = "auto", the Red List API is used whenever a token is available.
.get_iucn_status <- function(taxa,
                             source = c("auto", "redlist", "gbif"),
                             key = Sys.getenv("IUCN_REDLIST_KEY"),
                             verbose = TRUE) {
  source <- match.arg(source)
  if (source == "auto") {
    source <- if (nzchar(key) && .has_rredlist()) "redlist" else "gbif"
  }
  if (source == "redlist" && !.has_rredlist()) {
    stop("Package 'rredlist' is required to query the IUCN Red List API. Please install it.",
         call. = FALSE)
  }

  status <- rep(NA_character_, length(taxa))
  uniq <- unique(taxa[!is.na(taxa)])
  if (length(uniq) == 0) return(status)

  if (verbose) {
    message("Retrieving IUCN Red List status for ", length(uniq), " species from ",
            if (source == "redlist") "the IUCN Red List API" else "GBIF (IUCN Red List checklist)",
            "...")
  }

  codes <- vapply(uniq, function(taxon) {
    tryCatch(
      if (source == "redlist") .iucn_from_redlist(taxon, key) else .iucn_from_gbif(taxon),
      error = function(e) NA_character_
    )
  }, character(1), USE.NAMES = FALSE)

  labels <- .iucn_label(codes)
  status <- labels[match(taxa, uniq)]

  if (verbose) {
    message("IUCN status found for ", sum(!is.na(labels)), " of ", length(uniq), " species.")
  }
  status
}

# Thin wrappers so availability and web requests can be mocked in tests ####
.has_rredlist <- function() {
  requireNamespace("rredlist", quietly = TRUE)
}

# Thin wrapper around the GBIF API so it can be mocked in tests ####
.gbif_get <- function(url) {
  jsonlite::fromJSON(url, simplifyVector = TRUE)
}

.iucn_from_gbif <- function(taxon) {
  match <- .gbif_get(paste0(
    "https://api.gbif.org/v1/species/match?kingdom=Plantae&strict=true&name=",
    utils::URLencode(taxon, reserved = TRUE)
  ))
  if (is.null(match$usageKey) || !identical(match$matchType, "EXACT")) {
    return(NA_character_)
  }
  res <- .gbif_get(paste0("https://api.gbif.org/v1/species/", match$usageKey,
                          "/iucnRedListCategory"))
  code <- res$code
  if (is.null(code) || length(code) == 0) NA_character_ else .iucn_code(code[1])
}

.iucn_from_redlist <- function(taxon, key) {
  parts <- strsplit(trimws(taxon), "\\s+")[[1]]
  if (length(parts) < 2) return(NA_character_)

  res <- rredlist::rl_species(genus = parts[1], species = parts[2], key = key)
  assessments <- as.data.frame(res$assessments)
  if (nrow(assessments) == 0 || !"red_list_category_code" %in% names(assessments)) {
    return(NA_character_)
  }

  # Keep the latest global assessment (scope code "1")
  if ("latest" %in% names(assessments)) {
    assessments <- assessments[assessments$latest %in% TRUE, , drop = FALSE]
  }
  if ("scopes" %in% names(assessments) && nrow(assessments) > 1) {
    is_global <- vapply(assessments$scopes, function(sc) {
      "1" %in% unlist(sc$code)
    }, logical(1))
    if (any(is_global)) assessments <- assessments[is_global, , drop = FALSE]
  }
  if (nrow(assessments) == 0) return(NA_character_)

  .iucn_code(assessments$red_list_category_code[1])
}
