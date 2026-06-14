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
  utils::write.csv(df, file = paste0(dir, "/", filename), rowNames = FALSE)
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

  if (verbose) message("Loaded data with ", nrow(df), " species and ", ncol(df), " columns.")
  return(df)
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
    message("\n✅ Copied ", sum(copied), " files successfully")
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

  for (lang in base_langs) {
    html_phrases[[lang]] <- .phrase_generator(
      df = df,
      dict = dict,
      lang = lang,
      verbose = verbose
    )
    if (verbose) {
      message("Generated phrases for language: ", toupper(lang))
    }
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
    has_add_lang_phrases = has_add_lang_phrases
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
        and <- paste(" ", .tr_dict("and", lang, dict),  " ")
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

    if (endemism == "Endemic") {
      if (n_btcl_cntr == 1) {
        if (country == "Brazil") {
          temp_keys <- dict$key[dict$en == botanical_country]
        }
        if (n_state == 1) {
          phrase_dis <- paste0(.capitalize(.tr_dict("endemic", lang, dict)),
                               " ",
                               .tr_dict("to", lang, dict),
                               " ",
                               ifelse(country == "Brazil", .tr_dict(temp_keys, lang, dict), botanical_country),
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
            ifelse(country == "Brazil", .tr_dict(temp_keys, lang, dict), botanical_country), ".", " "
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
      temp_keys <- vector()
      temp <- strsplit(country, " \\| ")[[1]]
      for (l in seq_along(temp)) {
        temp_keys[l] <- country_dict$key[which(country_dict$en %in% temp[l])]
      }
      alt_country <- .tr_dict_vec(temp_keys, lang, country_dict)

      or <- paste(" ", .tr_dict("or", lang, dict),  " ")
      alt_country <- paste0(sample(alt_country, 2), collapse = or)
      phrase_dis <- paste0(
        .tr_dict("this_species_is_found_in", lang, dict), " ",
        alt_country,
        "." , " "
      )
    }

    # ========================================================================
    # PART 4: Establishment means (Native, Naturalized, Cultivated) ####
    # ========================================================================
    # e.g., ""

    if (is.na(origins)) {
      phrase_est <- ""

    } else if (origins == "Native" & endemism == "Endemic") {
      phrase_est <- paste0(
        .tr_dict("native_brazil_natural", lang, dict),
        ".", " "
      )
    } else if (origins == "Native" & endemism == "Non-endemic") {
      phrase_est <- paste0(
        .tr_dict("native_brazil_explanation", lang, dict),
        ".", " "
      )

    } else if (origins == "Naturalized") {
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
          .tr_dict(introduced, lang, bot_cntr_dict),
          ".", " "
        )
      } else {

        temp_keys <- vector()
        temp <- strsplit(introduced, " \\| ")[[1]]
        for (l in seq_along(temp)) {
          temp_keys[l] <- bot_cntr_dict$key[which(bot_cntr_dict$en %in% temp[l])]
        }
        alt_introduced <- .tr_dict_vec(temp_keys, lang, bot_cntr_dict)

        and <- paste(" ", .tr_dict("and", lang, dict),  " ")
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
    # e.g., ""

    if (is.na(phyto)) {
      phrase_biom <- ""

    } else {
      n_phyto <- lengths(strsplit(phyto, "\\|"))

      if (n_phyto == 1) {
        temp_keys <- dict$key[dict$en == phyto]

        phrase_biom <- paste0(
          .tr_dict("it_inhabits_the", lang, dict), " ",
          .tr_dict(temp_keys, lang, dict),
          ",", " "
        )
      } else if (n_phyto %in% 2:5) {

        and <- paste(" ", .tr_dict("and", lang, dict),  " ")

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
          alt_phyto,
          ",", " "
        )
      } else if (n_phyto == 6) {
        phrase_biom <- paste0(
          .tr_dict("found_in_every_biome_in_brazil", lang, dict), ",",
          " ", .tr_dict("how_lucky", lang, dict), "!"
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

      if (n_vt == 1) {
        temp_keys <- dict$key[dict$pt == vege_type]
        phrase_veg <- paste0(
          .tr_dict("more_specifically", lang, dict), ",", " ",
          .tr_dict("grow_particular_vegetype", lang, dict), ",", " ",
          .tr_dict(temp_keys, lang, dict), ".", " "
        )
      } else {
        or <- paste(" ", .tr_dict("or", lang, dict),  " ")

        temp_keys <- vector()
        temp <- strsplit(vege_type, " \\| ")[[1]]
        for (l in seq_along(temp)) {
          temp_keys[l] <- dict$key[which(dict$pt %in% temp[l])]
        }
        temp_vege <- .tr_dict_vec(temp_keys, lang, dict)
        alt_vege_type <- paste0(sample(temp_vege, 2), collapse = or)

        if (is.na(phyto)) {
          phrase_veg <- paste0(
            .tr_dict("more_specifically", lang, dict), ",", " ",
            .tr_dict("grow_vegetypes", lang, dict), " ",
            .tr_dict("such_as", lang, dict),
            " ",
            alt_vege_type, ".", " "
          )
        } else {
          phrase_veg <- paste0(
            .tr_dict("more_specifically", lang, dict), ",", " ",
            .tr_dict("grow_vegetypes", lang, dict), " ",
            .tr_dict("such_as", lang, dict),
            " ",
            alt_vege_type, ".", " "
          )
        }
      }
    }

    if (phrase_biom == "") {
      phrase_veg <- .capitalize(phrase_veg)
    }

    # ============================================================
    # PART 8: IUCN's status ####
    # ============================================================
    # e.g., ""

    if (is.na(status)) {
      phrase_IUCN <- ""

    } else {
      temp_keys <- dict$key[dict$en == status]
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
      "native_brazil_natural",
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
      "more_specifically", "grow_particular_vegetype",
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
      "It is therefore also native; it grows there naturally without human intervention",
      "In Brazil, it is native; that is to say, it grows there naturally without human intervention",
      "In Brazil, it is cultivated; this means that it has been intentionally grown and managed by humans, so this plant is entirely dependent on human intervention to survive and reproduce",
      "In Brazil, it is naturalised, meaning that it was introduced there but has adapted well, formed self-sustaining populations and reproduces on its own, without human assistance", "it is",

      # Introduced
      "This plant has also been introduced to",
      "This plant has also been introduced to several countries around the world like",

      # Phytogeographic domain
      "Amazon", "Atlantic Forest", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "It inhabits the", "It colonizes various habitats",
      "It also can be found in every biome in Brazil", "how lucky",

      # Vegetation types
      "more specifically", "this species likes to grow in a particular vegetation type",
      "this species likes to grow in particular vegetation types",
      "shrubland Carrasco", "seasonal deciduous forest", "terra-firme forest",
      "rainforest", "herbaceous or grassland savanna", "fire-prone savanna-like Cerrado vegetation",
      "Várzea forest", "anthropogenic area", "seasonal evergreen forest",
      "seasonal semideciduous forest", "gallery forest", "mixed ombrophilous forest",
      "seasonally flooded Igapó forest", "white-sand coastal scrubland Restinga",
      "Caatinga seasonally dry forest", "rupestrian grassland",
      "rocky outcrop vegetation", "amazonian savanna", "high altitude grassland",
      "Várzea forest", "mangrove", "palm grove", "white‑sand Campinarana vegetation",
      "aquatic vegetation",

      # IUCN
      "extinct (EX)", "extinct in the wild (EW)", "critically endangered (CR)", "endangered (EN)",
      "vulnerable (VU)", "near threatened (NT)", "least concern (LC)", "data deficient (DD)", "not evaluated (NE)",
      "The International Union for Conservation of Nature (IUCN) has classified this species as",

      # Genus
      "Only species of this genus in Brazil",
      "One of the only {n} species of this genus in Brazil",
      "The genus with the highest diversity in Brazil",
      "One of the genera with the highest diversity in Brazil"
    ),

    pt = c(

      # Taxon
      "pertence à", "família", "Esta espécie não tem nenhum nome popular conhecido", "É comumente chamada de",
      "É comumente conhecida", "por pelo menos", "nomes populares", "como", "e",

      # Distribution
      "Uma verdadeira joia rara", "ela só é encontrada no estado", "Esta espécie é encontrada em vários países como",
      "Esta espécie é", "do", "não endêmica", "endêmica", "ou", "norte do Brasil", "nordeste do Brasil",
      "sul do Brasil", "sudeste do Brasil", "centro-oeste do Brasil",

      # Establishment means
      "Portanto, ela também é nativa, crescendo naturalmente sem intervenção humana",
      "No Brasil, ela é nativa, ou seja, cresce naturalmente sem intervenção humana",
      "No Brasil, ela é cultivada, o que significa que foi plantada e cuidada intencionalmente pelo homem; assim, essa planta depende inteiramente da intervenção humana para sobreviver e se reproduzir",
      "No Brasil, ela é naturalizada, ou seja, foi introduzida, mas se adaptou bem, formou populações autônomas e se reproduz sozinha, sem a ajuda do homem", "Ela é",

      # Introduced
      "Esta planta também foi introduzida em",
      "Esta planta também foi introduzida em vários países ao redor do mundo, como",

      # Phytogeographic domain
      "Amazônia", "Mata Atlântica", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Ela habita", "Ela coloniza vários habitats",
      "Ela também pode ser encontrada em todos os biomas do Brasil", "que sorte",

      # Vegetation types
      "mais especificamente", "esta espécie gosta de crescer em um tipo de vegetação particular",
      "esta espécie gosta de crescer em tipos particulares de vegetações",
      "Carrasco", "Floresta Estacional Decidual", "Floresta de Terra Firme",
      "Floresta Ombrófila", "Campo Limpo", "Cerrado sensu lato",
      "Floresta de Várzea", "Área Antrópica",
      "Floresta Estacional Perenifólia", "Floresta Estacional Semidecidual",
      "Floresta Ciliar ou Galeria", "Floresta Ombrófila Mista", "Floresta de Igapó",
      "Restinga", "Caatinga sensu stricto", "Campo rupestre",
      "Vegetação Sobre Afloramentos Rochosos", "Savana Amazônica", "Campo de Altitude",
      "Campo de Várzea", "Manguezal", "Palmeiral", "Campinarana", "Vegetação Aquática",

      # IUCN
      "extinto (EX)", "extinto na natureza (EW)", "criticamente em perigo (CR)", "em perigo (EN)",
      "vulnerável (VU)", "quase ameaçado (NT)", "pouco preocupante (LC)", "dados insuficientes (DD)",
      "não avaliado (NE)",
      "A União Internacional para a Conservação da Natureza (IUCN) classificou esta espécie como",

      # Genus
      "'Única espécie deste gênero no Brasil",
      "Uma das únicas {n} espécies deste gênero no Brasil",
      "O gênero com a maior diversidade no Brasil",
      "Um dos gêneros com a maior diversidade no Brasil"
    ),

    es = c(

      # Taxon
      "pertenece a", "la familia", "Esta especie no tiene ningún nombre común conocido",
      "Se la conoce comúnmente como", "Se conoce comúnmente como", "por al menos",
      "nombres comunes", "como", "y",

      # Distribution
      "Una verdadera joya rara", "solo se encuentra en ese estado",
      "Esta especie se encuentra en varios países como",
      "Esta especie es", "del", "no endémica", "endémica", "o", "norte de Brasil",
      "nordeste de Brasil", "sur de Brasil", "sudeste de Brasil", "centro-oeste de Brasil",

      # Establishment means
      "Por lo tanto, también es autóctona, es decir, donde crece de forma natural sin intervención humana",
      "En Brasil es autóctona, es decir, crece de forma natural sin intervención humana",
      "En Brasil se cultiva, lo que significa que ha sido cultivada y gestionada intencionadamente por el ser humano; por lo tanto, esta planta depende totalmente de la intervención humana para sobrevivir y reproducirse",
      "En Brasil, está naturalizada, es decir, que fue introducida allí, pero se ha adaptado bien, ha formado poblaciones autónomas y se reproduce por sí sola, sin la ayuda del hombre", "Es",

      # Introduced
      "Esta planta también ha sido introducida en",
      "Esta planta también ha sido introducida en varios países del mundo, como",

      # Phytogeographic domain
      "Amazonía", "Mata Atlántica", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Habita", "Coloniza varios hábitats",
      "También se puede encontrar en todos los biomas de Brasil", "qué suerte",

      # Vegetation types
      "más específicamente", "esta especie le gusta crecer en un tipo de vegetación particular",
      "esta especie le gusta crecer en tipos particulares de vegetacións",
      "Carrasco (matorral)", "Bosque Estacional Deciduo", "Bosque de Tierra Firme",
      "Selva Ombrófila", "Campo Limpio", "Cerrado",
      "Bosque de Várzea", "Área Antrópica", "Bosque Estacional Perennifolio",
      "Bosque Estacional Semideciduo", "Bosque de Galería",
      "Bosque Ombrófilo Mixto", "Bosque de Igapó", "Restinga",
      "Caatinga", "Campo Rupestre", "Vegetación sobre Afloramientos Rochosos",
      "Sabana Amazónica", "Campo de Altitud", "Campo de Várzea", "Manglar",
      "Palmeral", "vegetación de Campinarana sobre arena blanca", "Vegetación Acuática",

      # IUCN
      "extinto (EX)", "extinto en estado silvestre (EW)", "en peligro crítico (CR)", "en peligro (EN)",
      "vulnerable (VU)", "casi amenazado (NT)", "preocupación menor (LC)", "datos insuficientes (DD)",
      "no evaluado (NE)",
      "La Unión Internacional para la Conservación de la Naturaleza (IUCN) ha clasificado esta especie como",

      # Genus
      "Única especie de este género en Brasil",
      "Una de las únicas {n} especies de este género en Brasil",
      "El género con mayor diversidad en Brasil",
      "Uno de los géneros con mayor diversidad en Brasil"
    ),

    fr = c(
      # Taxon
      "appartient à", "la famille", "Cette espèce n’a aucun nom commun connu", "Elle est communément appelée",
      "Elle est communément appelée", "par au moins", "noms communs", "comme", "et",

      # Distribution
      "Un vrai joyau rare", "on ne la retrouve que dans l'état", "Cette espèce se trouve dans plusieurs pays tels que",
      "Cette espèce est", "du", "non endémique", "endémique", "ou", "nord du Brésil", "nord-est du Brésil",
      "sud du Brésil", "sud-est du Brésil", "centre-ouest du Brésil",

      # Establishment means
      "Elle y est donc aussi native, elle y pousse naturellement sans intervention humaine",
      "Au Brésil elle y est native, c'est à dire qu'elle y pousse naturellement sans intervention humaine",
      "Au Brésil, elle y est cultivée, cela signifie qu'elle a été intentionnellement cultivée et gérée par l'homme, cette plante dépend donc entièrement de l'intervention humaine pour survivre et se reproduire",
      "Au Brésil, elle y est naturalisée, c'est à dire qu'elle y a été introduite, mais s'est bien adaptée, a formé des populations autonomes et se reproduit seule, sans l'aide de l'homme", "Elle est",

      # Introduced
      "Cette plante a également été introduite au",
      "Cette plante a également été introduite dans plusieurs pays du monde, comme",

      # Phytogeographic domain
      "Amazonie", "Forêt Atlantique", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Elle habite", "Elle colonise divers habitats",
      "On peut aussi la trouver dans tous les biomes du Brésil", "quelle chance",

      # Vegetation types
      "plus spécifiquement", "cette espèce aime pousser dans un type de végétation particulier",
      "cette espèce aime pousser dans des types de végétations particuliers",
      "Carrasco (fourré)", "forêt tropicale semi-décidue", "forêt amazonienne non inondable",
      "forêt ombrophile", "savane herbacée ou prairiale", "Cerrado",
      "forêt de Várzea", "zone anthropique", "forêt tropicale saisonnière perennifoliée",
      "forêt saisonnière semi-décidue", "forêt galerie",
      "forêt ombrophile mixte", "forêt d’Igapó", "Restinga",
      "Caatinga", "prairie rupestre", "végétation sur affleurements rocheux",
      "savane amazonienne", "prairie d’altitude", "prairie de Várzea", "mangrove",
      "palmeraie", "Campinarana (végétation sur sable blanc)", "végétation aquatique",

      # IUCN
      "éteint (EX)", "éteint à l'état sauvage (EW)", "en danger critique (CR)", "en danger (EN)",
      "vulnérable (VU)", "quasi menacé (NT)", "préoccupation mineure (LC)", "données insuffisantes (DD)",
      "non évalué (NE)",
      "L'Union internationale pour la conservation de la nature (IUCN) a classé cette espèce comme",

      # Genus
      "Seule espèce de ce genre au Brésil",
      "L'une des seules {n} espèces de ce genre au Brésil",
      "Le genre avec la plus grande diversité au Brésil",
      "L'un des genres avec la plus grande diversité au Brésil"
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
