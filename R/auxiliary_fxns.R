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

    if (is.na(endemism)) {
      phrase_dis <- ""

    } else {
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
    }

    # ========================================================================
    # PART 4: Establishment means (Native, Naturalized, Cultivated) ####
    # ========================================================================
    # e.g., ""

    if (is.na(origins)) {
      phrase_est <- ""

    } else if (endemism == "Endemic") {
      phrase_est <- paste0(
        .tr_dict("only_place_earth", lang, dict),
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
        or <- paste(" ", .tr_dict("or", lang, dict),  " ")

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

    if (phrase_biom == "" | n_phyto == 6) {
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
      "It is the only species of this genus in Brazil",
      "It is one of the few {n} species of this genus in Brazil",
      "This species is part of the genus with the greatest diversity in Brazil",
      "This species belongs to one of the genera with the greatest diversity in Brazil"
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
      "Isso significa que não é encontrado em seu estado natural em nenhum outro lugar do planeta",
      "Ela é nativa do Brasil, ou seja, cresce naturalmente no país sem intervenção humana",
      "No Brasil, ela foi cultivada e manejada intencionalmente pelo homem; portanto, essa planta depende inteiramente da intervenção humana para sobreviver e se reproduzir",
      "No Brasil, ela foi naturalizada, ou seja, foi introduzida no país, mas se adaptou bem, formou populações autônomas e se reproduz sozinha, sem a ajuda do homem", "Ela é",

      # Introduced
      "Esta planta também foi introduzida em",
      "Esta planta também foi introduzida em vários países ao redor do mundo, como",

      # Phytogeographic domain
      "Amazônia", "Mata Atlântica", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Ela habita", "Ela coloniza vários habitats",
      "Ela também pode ser encontrada em todos os biomas do Brasil", "que sorte",

      # Vegetation types
      "onde", "ela cresce preferencialmente em uma camada vegetal específica",
      "ela cresce preferencialmente em camadas vegetais específicas",
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
      "É a única espécie desse gênero no Brasil",
      "É uma das poucas {n} espécies desse gênero no Brasil",
      "Essa espécie faz parte do gênero com maior diversidade no Brasil",
      "Essa espécie pertence a um dos gêneros com maior diversidade no Brasil"
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
      "Esto significa que no se encuentra en su estado natural en ningún otro lugar del planeta",
      "Es autóctona de Brasil, es decir, crece allí de forma natural sin intervención humana",
      "En Brasil, ha sido cultivada y gestionada intencionadamente por el ser humano, por lo que esta planta depende por completo de la intervención humana para sobrevivir y reproducirse",
      "En Brasil se ha naturalizado, es decir, que fue introducida allí, pero se ha adaptado bien, ha formado poblaciones autónomas y se reproduce por sí sola, sin la ayuda del hombre", "Es",

      # Introduced
      "Esta planta también ha sido introducida en",
      "Esta planta también ha sido introducida en varios países del mundo, como",

      # Phytogeographic domain
      "Amazonía", "Mata Atlántica", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Habita", "Coloniza varios hábitats",
      "También se puede encontrar en todos los biomas de Brasil", "qué suerte",

      # Vegetation types
      "donde", "«crece preferentemente en un estrato vegetal concreto",
      "crece preferentemente en estratos vegetales concretos",
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
      "Es la única especie de este género en Brasil",
      "Es una de las pocas {n} especies de este género en Brasil",
      "Esta especie forma parte del género con mayor diversidad en Brasil",
      "Esta especie pertenece a uno de los géneros con mayor diversidad en Brasil"
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
      "Cela signifie qu'on ne la trouve à l'état naturel nulle part ailleurs sur Terre",
      "Elle est native du Brésil, c'est à dire qu'elle y pousse naturellement sans intervention humaine",
      "Au Brésil, elle a été intentionnellement cultivée et gérée par l'homme, cette plante dépend donc entièrement de l'intervention humaine pour survivre et se reproduire",
      "Au Brésil, elle a été naturalisée, c'est à dire qu'elle y a été introduite, mais s'est bien adaptée, a formé des populations autonomes et se reproduit seule, sans l'aide de l'homme", "Elle est",

      # Introduced
      "Cette plante a également été introduite au",
      "Cette plante a également été introduite dans plusieurs pays du monde, comme",

      # Phytogeographic domain
      "Amazonie", "Forêt Atlantique", "Pampa", "Pantanal", "Caatinga", "Cerrado",
      "Elle habite", "Elle colonise des écosystèmes divers comme",
      "On peut aussi la trouver dans tous les biomes du Brésil", "quelle chance",

      # Vegetation types
      "où", "elle pousse de préférence dans une strate végétale particulière",
      "elle pousse de préférence dans des strates végétales particulières",
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
      "C'est la seule espèce de ce genre au Brésil",
      "C'est l'une des seules {n} espèces de ce genre au Brésil",
      "Cette espèce fait partie du genre avec la plus grande diversité au Brésil",
      "Cette espèce appartient à l'un des genres avec la plus grande diversité au Brésil"
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
      <button id="pickAudioRoot" class="lang-btn" type="button">📁 Choose audio folder</button>
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
    if (stat) stat.textContent = "● Unsaved changes";
  }
  function setSaved(msg) {
    const stat = document.getElementById("saveStatus");
    if (stat) stat.textContent = msg || "✓ Saved";
  }

  // Toggle edit panels
  document.querySelectorAll(".edit-toggle-btn").forEach(btn => {
    btn.addEventListener("click", function () {
      const panel = document.getElementById(this.dataset.target);
      if (!panel) return;
      if (panel.hasAttribute("hidden")) {
        panel.removeAttribute("hidden");
        this.innerHTML = "× Close editing";
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
      else if (kind === "addlang") refreshAddLangPhrase(row, lang, this.value);
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

  function refreshAddLangPhrase(row, lang, val) {
    const span = document.querySelector(`.phrase-base[data-row="${row}"][data-lang="${lang}"]`);
    if (span) span.textContent = val.trim();
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
    triggerDownload(new Blob(["﻿" + lines.join("\\n")], { type: "text/csv;charset=utf-8;" }),
                    csvName);
    setSaved("✓ CSV downloaded");
  }

  function loadSheetJS(cb) {
    if (window.XLSX) { cb(); return; }
    const s = document.createElement("script");
    s.src = "https://cdn.sheetjs.com/xlsx-0.20.3/package/dist/xlsx.full.min.js";
    s.onload = cb;
    s.onerror = () => setSaved("⚠ XLSX unavailable — try CSV");
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
      setSaved("✓ XLSX downloaded");
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
          .then(() => setSaved("✓ Saved"))
          .catch(() => { triggerDownload(blob, dataFilename); setSaved("✓ Downloaded"); });
      } else if (window.showSaveFilePicker) {
        // When opened as file://, the dialog starts in the same folder as
        // this HTML file — i.e. the dir/ that arboretum_data() wrote to.
        window.showSaveFilePicker({
          suggestedName: dataFilename,
          types: [{ description: "Excel Workbook", accept: { "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet": [".xlsx"] } }]
        }).then(h2 => {
          xlsxFileHandle = h2;
          _idbPut(handleKey, h2).catch(() => {});
          return h2.createWritable().then(w => w.write(blob).then(() => w.close()));
        }).then(() => setSaved("✓ Saved"))
          .catch(() => {});
      } else {
        triggerDownload(blob, dataFilename);
        setSaved("✓ Downloaded");
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
                   '<button type="button" class="record-btn">🎙 Record</button>',
                   '<button type="button" class="stop-record-btn">⏹ Stop</button>',
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
         pt = "Português",
         en = "English",
         fr = "Français",
         es = "Español",
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
      title_audios = "Guia para Gravação de Áudios Pessoais",
      title_data = "Guia de Geração de Frases",
      subtitle_audios = "Use este arquivo para gravar seus próprios áudios das espécies antes de gerar os rótulos em HTML e o minisite.",
      subtitle_data = "Use este arquivo para verificar as frases geradas automaticamente antes de gravar os áudios e gerar as etiquetas HTML e o minisite.",
      search_placeholder = "Pesquisar nome da espécie ou família...",
      index_title = "Índice",
      no_results = "Nenhuma espécie corresponde à sua busca.",
      back_to_top = "Voltar ao topo",
      footer_note = "Use a busca do navegador ou o campo acima para navegar rapidamente entre as espécies.",
      family = "Família",
      generated_with = "Gerado com"
    ),
    fr = list(
      html_lang = "fr",
      title_audios = "Guide d’Enregistrement Audio Personnel",
      title_data = "Guide de génération de phrases",
      subtitle_audios = "Utilisez ce fichier pour enregistrer vos propres audios d’espèces avant de générer les étiquettes HTML et le minisite.",
      subtitle_data = "Utilisez ce fichier pour vérifier les phrases générées automatiquement avant d’enregistrer les audios et de générer les étiquettes HTML et le minisite.",
      search_placeholder = "Rechercher le nom de l’espèce ou de la famille...",
      index_title = "Index",
      no_results = "Aucune espèce ne correspond à votre recherche.",
      back_to_top = "Retour en haut",
      footer_note = "Utilisez la recherche du navigateur ou le champ ci-dessus pour naviguer rapidement entre les espèces.",
      family = "Famille",
      generated_with = "Généré avec"
    ),
    es = list(
      html_lang = "es",
      title_audios = "Guía para Grabar Audios Personales",
      title_data = "Guía de Generación de Frases",
      subtitle_audios = "Use este archivo para grabar sus propios audios de especies antes de generar las etiquetas HTML y el minisitio.",
      subtitle_data = "Use este archivo para revisar las frases generadas automáticamente antes de grabar los audios y generar las etiquetas HTML y el minisite.",
      search_placeholder = "Buscar nombre de la especie o familia...",
      index_title = "Índice",
      no_results = "Ninguna especie coincide con su búsqueda.",
      back_to_top = "Volver arriba",
      footer_note = "Use la búsqueda del navegador o el campo superior para navegar rápidamente entre las especies.",
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

