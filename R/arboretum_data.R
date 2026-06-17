#' Extract and compile flora data from multiple taxonomic databases
#'
#' @author
#' Martin Boucknooghe & Domingos Cardoso
#'
#' @description
#' This function queries both [Flora e Funga do Brasil (FFB)](https://floradobrasil.jbrj.gov.br/consulta/)
#' and [Plants of the World Online (POWO)](https://powo.science.kew.org/) to retrieve
#' taxonomic, distributional, vernacular, conservation, and occurrence-related information
#' for a given list of plant species. It standardizes input names, handles synonyms,
#' merges data from both sources, and returns a single dataframe that can optionally be
#' saved as a CSV or Excel file. The output also includes genus-level richness and rank
#' information derived from FFB.
#'
#' @param spp_list Required. A character vector of species names, for example
#'   `c("Euterpe edulis", "Coffea arabica")`. Names should be binomials without
#'   authorship. Leading and trailing whitespace are removed, and names are standardized
#'   internally before querying. An error is thrown if any element does not contain a
#'   space, indicating a probable non-species name.
#' @param printed_lang Character vector. Built-in language(s) to generate folders and
#'   phrases for. Accepted values are `"pt"`, `"en"`, `"fr"`, and `"es"`.
#' @param add_lang Character string or `NULL`. Optional code for one additional
#'   language to include in the folder structure for personal recordings, for
#'   example `"PANARA"` or `"TUKANO"`. This argument is intended for cases where
#'   users want to add custom community or local-language audio without translating
#'   the full package interface. When supplied, one extra recording folder per
#'   species is created using that language code.
#' @param verbose Logical. If `TRUE`, progress messages are printed to the console.
#'   Default is `TRUE`.
#' @param save Logical. If `TRUE`, the resulting dataframe is saved to disk.
#'   Default is `TRUE`.
#' @param format Character string indicating the output file format. One of `"csv"` or
#'   `"xlsx"`. Partial matching is allowed through \code{match.arg()}.
#'   Default is `"csv"`.
#' @param filename Character string. Base name for the output file, without extension.
#'   Default is `"arboretum_mined_data"`.
#' @param dir Character string. Directory path where the output file will be saved.
#'   Trailing slashes are automatically removed. The directory is created if it does
#'   not exist. Default is `"arboretum_mined_data"`.
#'
#' @return
#' A dataframe combining data retrieved from FFB and POWO. The returned columns include:
#' \itemize{
#'   \item \code{family}: Family name of the accepted taxon.
#'   \item \code{genus}: Genus extracted from the accepted scientific name.
#'   \item \code{taxonName}: Accepted scientific name after synonym resolution.
#'   \item \code{scientificNameAuthorship}: Authorship string.
#'   \item \code{FFB.vernacularName}: Vernacular names from FFB, with multiple values
#'     concatenated by `" | "`.
#'   \item \code{country}: Country names from FFB or derived from POWO botanical countries.
#'   \item \code{endemism}: Endemism status in Brazil from FFB, or inferred from POWO
#'     country-level distribution when FFB data are unavailable.
#'   \item \code{botanical_country}: Native distribution based on POWO botanical countries.
#'   \item \code{introduced_to}: Countries or botanical regions where the species is
#'     introduced according to POWO.
#'   \item \code{FFB.establishmentMeans}: Establishment means from FFB, such as
#'     `"Native"`, `"Cultivated"`, or `"Naturalized"`.
#'   \item \code{FFB.stateProvince}: Brazilian states from FFB, concatenated by `" | "`.
#'   \item \code{FFB.phytogeographicDomain}: Brazilian phytogeographic domains from FFB,
#'     translated into English when applicable and concatenated by `" | "`.
#'   \item \code{FFB.vegetationType}: Vegetation types from FFB, concatenated by `" | "`.
#'   \item \code{IUCN.status}: IUCN conservation status retrieved from POWO, when available.
#'   \item \code{FFB.genusRichness}: Number of accepted species of the genus recorded in FFB.
#'   \item \code{FFB.genusRank}: Rank of the genus by species richness in FFB, with 1
#'     representing the richest genus.
#'   \item \code{plant_uses_EN}, \code{plant_uses_PT}, \code{plant_uses_ES},
#'     \code{plant_uses_FR}: Plant-use fields in English, Portuguese, Spanish, and French,
#'     reserved for downstream annotation or future use.
#'   \item \code{free_notes_EN}, \code{free_notes_PT}, \code{free_notes_ES},
#'     \code{free_notes_FR}: Free-text note fields in English, Portuguese, Spanish, and
#'     French, reserved for downstream annotation or future use.
#'   \item \code{POWO.url}: URL to the species page in POWO.
#'   \item \code{FFB.url}: URL to the species page in FFB.
#' }
#'
#' If a species is found in only one database, fields from the missing database are
#' returned as `NA`. Species not found in either database are omitted from the final
#' dataframe. For overlapping fields, FFB data generally take precedence, except for
#' selected POWO-derived fields such as botanical country, introduced range, IUCN status,
#' and POWO URL.
#'
#' @details
#' The function follows four main steps:
#'
#' \enumerate{
#'   \item \strong{Flora e Funga do Brasil data extraction}
#'   \itemize{
#'     \item Downloads the latest FFB Darwin Core Archive using
#'       \code{floraR::flora_download()}.
#'     \item Parses the archive with \code{floraR::flora_parse()}.
#'     \item Extracts and processes the taxon, distribution, vernacular name, and species
#'       profile tables.
#'     \item Matches each queried species name against the FFB taxon table.
#'     \item Resolves synonyms to their accepted names when possible.
#'     \item Retrieves family, accepted name, authorship, vernacular names, distribution,
#'       endemism, establishment means, Brazilian states, phytogeographic domains,
#'       vegetation types, and FFB reference URLs.
#'   }
#'
#'   \item \strong{POWO data extraction}
#'   \itemize{
#'     \item Searches POWO using \code{taxize::pow_search()}.
#'     \item Resolves synonyms to accepted names when possible.
#'     \item Retrieves detailed information using \code{taxize::pow_lookup()}, including
#'       taxonomy, distribution, introduced range, IUCN status, and POWO URL.
#'     \item Converts POWO botanical countries to standard country names using an internal
#'       helper function.
#'   }
#'
#'   \item \strong{Data merging}
#'   \itemize{
#'     \item Combines FFB and POWO results into a single dataframe.
#'     \item Prioritizes FFB data for overlapping fields when available.
#'     \item Complements FFB records with POWO botanical countries, introduced range,
#'       conservation status, and POWO URLs.
#'     \item Infers endemism from POWO country-level distribution when FFB endemism data
#'       are unavailable.
#'   }
#'
#'   \item \strong{Genus-level summaries}
#'   \itemize{
#'     \item Extracts the genus from each accepted taxon name.
#'     \item Calculates the number of accepted species per genus in FFB.
#'     \item Adds genus richness and genus rank to the final dataframe.
#'     \item Adds multilingual genus curiosity notes using internal helper functions,
#'       when available.
#'   }
#' }
#'
#' Before processing, \code{spp_list} is cleaned using internal helper functions.
#' Leading and trailing whitespace are removed, names are standardized, and each element
#' is checked to ensure that it contains a space. The function stops with an error if
#' any element appears not to be a binomial species name.
#'
#' If \code{save = TRUE}, the function creates the output directory if needed and saves
#' the resulting dataframe either as a CSV file using \code{utils::write.csv()} or as
#' an Excel file using \code{openxlsx::write.xlsx()}. When \code{verbose = TRUE}, a
#' message reports the saved file path.
#'
#' The temporary FFB download folder named `"flora_download"` is removed when the
#' function exits.
#'
#' @note
#' \itemize{
#'   \item The \pkg{floraR} package is required to download and parse the FFB Darwin Core
#'     Archive.
#'   \item The \pkg{taxize} package is required to query POWO.
#'   \item The \pkg{openxlsx} package is required only when \code{format = "xlsx"}.
#'   \item The function queries both FFB and POWO; there is currently no argument to select
#'     only one database.
#'   \item An internet connection is required.
#'   \item The initial FFB Darwin Core Archive download can be large and may take some time
#'     depending on the connection.
#'   \item POWO and FFB data are dynamic external resources, so results may change across
#'     database versions or query dates.
#' }
#'
#' @seealso
#' \code{\link[floraR]{flora_download}},
#' \code{\link[floraR]{flora_parse}},
#' \code{\link[taxize]{pow_search}},
#' \code{\link[taxize]{pow_lookup}}
#'
#' @examples
#' \dontrun{
#' # Single species, without saving the result
#' result <- arboretum_data(
#'   spp_list = "Luetzelburgia bahiensis",
#'   save = FALSE
#' )
#'
#' # Multiple species, saving the result as an Excel file
#' spp <- c("Cybianthus collinus",
#'          "Paubrasilia echinata",
#'          "Luetzelburgia bahiensis")
#'
#' result <- arboretum_data(
#'   spp_list = spp,
#'   save = TRUE,
#'   format = "xlsx",
#'   filename = "my_plant_data",
#'   dir = "results"
#' )
#'
#' # Suppress progress messages
#' result <- arboretum_data(
#'   spp_list = c("Euterpe edulis", "Coffea arabica"),
#'   verbose = FALSE
#' )
#' }
#'
#' @importFrom taxize pow_search pow_lookup
#' @importFrom openxlsx write.xlsx
#' @importFrom utils write.csv
#' @importFrom tibble add_column tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom stats na.omit setNames
#' @importFrom stringi stri_trans_general
#'
#' @export

arboretum_data <- function(spp_list = NULL,
                           printed_lang = c("pt", "en", "fr", "es"),
                           add_lang = NULL,
                           verbose = TRUE,
                           save = TRUE,
                           format = c("csv", "xlsx"),
                           filename = "arboretum_data",
                           dir = "arboretum_data"){

  # Input validation  ####
  spp_list <- .arg_check_spp_list(spp_list)
  printed_lang <- .arg_check_printed_lang(printed_lang)
  dir <- .arg_check_dir(dir)
  format <- match.arg(format)

  files <- list.files(dir)

  if (!any(grepl("[.]xlsx$|[.]csv$", files))) {

    # Setting up the dataframe structure with the required information ####
    result_FFB <- data.frame(
      original_query = spp_list,
      family = NA_character_,
      taxonName = NA_character_,
      scientificNameAuthorship = NA_character_,
      vernacularName = NA_character_,
      country = NA_character_,
      endemism = NA_character_,
      establishmentMeans = NA_character_,
      stateProvince = NA_character_,
      phytogeographicDomain = NA_character_,
      vegetationType = NA_character_,
      references = NA_character_
    )

    result_POWO <- data.frame(
      original_query = spp_list,
      family = NA_character_,
      taxonName = NA_character_,
      scientificNameAuthorship = NA_character_,
      country = NA_character_,
      botanical_country = NA_character_,
      introduced_to = NA_character_,
      IUCN.status = NA_character_,
      references = NA_character_
    )

    # Download and parse Flora e Funga do Brasil DwC-A dataset ####

    if (!requireNamespace("floraR", quietly = TRUE)) {
      stop("Package 'floraR' is required for `arboretum_data()`. Please install it.")
    }

    floraR::flora_download(version = "latest", dir = "flora_download")
    # Remove the downloaded FFB folder flora_download when this function finishes,
    # run this cleanup code before returning.
    on.exit(unlink("flora_download", recursive = TRUE, force = TRUE), add = TRUE)

    dwca <- floraR::flora_parse(path = "flora_download", version = "latest")

    if (verbose) message("Flora e Funga do Brasil DwC-A dataset successfully donwloaded and parsed!")

    # Data extraction ####
    # The first one contains all the required data; as the names may change, we therefore take the first element
    taxon_data <- dwca[[1]][["data"]][["taxon.txt"]]
    taxon_data <- taxon_data[taxon_data$taxonRank %in% "ESPECIE", ]
    distribution_data <- dwca[[1]][["data"]][["distribution.txt"]]
    distribution_data$locationID <- gsub("BR-", "", distribution_data$locationID)
    vernacular_data <- dwca[[1]][["data"]][["vernacularname.txt"]]
    speciesprofile_data <- dwca[[1]][["data"]][["speciesprofile.txt"]]
    speciesprofile_data$vegetationType <- gsub("\\s[(].*", "", speciesprofile_data$vegetationType)
    tf <- speciesprofile_data$vegetationType %in% "Caatinga"
    speciesprofile_data$vegetationType[tf] <- "Caatinga sensu stricto"
    tf <- speciesprofile_data$vegetationType %in% "Cerrado"
    speciesprofile_data$vegetationType[tf] <- "Cerrado sensu lato"

    # Adjusting FFB database into English
    distribution_data[[3]][distribution_data[[3]] %in% "BR"] <- "Brazil"
    distribution_data[[4]][distribution_data[[4]] %in% "NATIVA"] <- "Native"
    distribution_data[[4]][distribution_data[[4]] %in% "CULTIVADA"] <- "Cultivated"
    distribution_data[[4]][distribution_data[[4]] %in% "NATURALIZADA"] <- "Naturalized"
    distribution_data[[6]][distribution_data[[6]] %in% "Endemica"] <- "Endemic"
    distribution_data[[6]][distribution_data[[6]] %in% "Não endemica"] <- "Non-endemic"
    distribution_data[[7]][distribution_data[[7]] %in% "Amazônia"] <- "Amazon"
    distribution_data[[7]][distribution_data[[7]] %in% "Mata Atlântica"] <- "Atlantic Forest"

    #Genus statistics and synonym management for accurate data
    taxon_data_temp <- taxon_data[taxon_data$taxonomicStatus %in% "NOME_ACEITO", ]

    genus_richness_br <- as.data.frame(table(taxon_data_temp$genus))
    genus_ranked <- genus_richness_br[order(genus_richness_br$Freq, decreasing = TRUE), ]
    genus_ranked$rank <- 1:nrow(genus_ranked)
    max_diversity <- max(genus_ranked$Freq, na.rm = TRUE)
    n_max_genera <- sum(genus_ranked$Freq == max_diversity, na.rm = TRUE)
    genus_counts <- stats::setNames(genus_richness_br$Freq, genus_richness_br$Var1)
    rank_by_genus <- stats::setNames(genus_ranked$rank, genus_ranked$Var1)

    # Collection of data for each species ####
    for (i in seq_along(spp_list)) {
      sp <- spp_list[i]

      if (verbose) message(i, "/", length(spp_list), ": retrieving information from '", sp, "'")

      # POWO data
      result_POWO <- .extract_powo_data(result_POWO, sp, i)

      # FFB data
      # Retrieve the taxonIDs corresponding to the exact name
      taxon_id <- taxon_data$id[taxon_data$taxonName == sp]
      if (length(taxon_id) > 0) {
        # Handling synonyms ####
        taxon_id <- .get_accepted_name_id(taxon_id,
                                          taxon_data,
                                          verbose = verbose)

        temp_taxon <- taxon_data[taxon_data$id == taxon_id, ]
        temp_dist <- distribution_data[distribution_data$id == taxon_id, ]
        temp_vern <- vernacular_data[vernacular_data$id == taxon_id, ]
        temp_vege <- speciesprofile_data[speciesprofile_data$id == taxon_id, ]

        result_FFB$family[i] <- unique(temp_taxon$family)
        result_FFB$taxonName[i] <- unique(temp_taxon$taxonName)
        result_FFB$scientificNameAuthorship[i] <- unique(temp_taxon$scientificNameAuthorship)
        result_FFB$vernacularName[i] <- paste0(sort(unique(temp_vern$vernacularName)),
                                               collapse = " | ")
        result_FFB$endemism[i] <- unique(temp_dist$endemism)
        result_FFB$establishmentMeans[i] <- unique(temp_dist$establishmentMeans)
        result_FFB$stateProvince[i] <- paste0(sort(unique(temp_dist$locationID)),
                                              collapse = " | ")
        result_FFB$country[i] <- unique(temp_dist$countryCode)
        result_FFB$phytogeographicDomain[i] <- paste0(sort(unique(temp_dist$phytogeographicDomain)),
                                                      collapse = " | ")
        result_FFB$vegetationType[i] <- paste0(sort(unique(temp_vege$vegetationType)),
                                               collapse = " | ")

        # The references column shows where the URL for each species in FFB
        result_FFB$references[i] <- temp_taxon$references
      }
    }

    # Convert all empty spaces to NA across all columns
    result_FFB <- data.frame(lapply(result_FFB, function(x) {
      if (is.character(x)) {
        x[x == "" | trimws(x) == ""] <- NA
      }
      x
    }), stringsAsFactors = FALSE)

    result_POWO <- data.frame(lapply(result_POWO, function(x) {
      if (is.character(x)) {
        x[x == "" | trimws(x) == ""] <- NA
      }
      x
    }), stringsAsFactors = FALSE)

    # Update the originally queried species list
    spp_list_updated <- vector()
    for (i in seq_along(spp_list)) {

      tf <- result_POWO$original_query %in% spp_list[i]
      if (any(tf)) {
        spp_list_updated[i] <- result_POWO$taxonName[tf]
      }

      tf <- result_FFB$original_query %in% spp_list[i]
      if (any(tf)) {
        if (!is.na(result_FFB$taxonName[tf])) {
          spp_list_updated[i] <- result_FFB$taxonName[tf]
        }
      }

    }
    spp_list_updated <- unique(spp_list_updated)

    tf <- is.na(result_FFB$taxonName)
    if (any(tf)) {
      result_FFB <- result_FFB[!tf, ]
      row.names(result_FFB) <- 1:nrow(result_FFB)
      if (verbose) {
        message()
      }
    }

    tf <- is.na(result_POWO$taxonName)
    if (any(tf)) {
      result_POWO <- result_POWO[!tf, ]
      row.names(result_POWO) <- 1:nrow(result_POWO)
      if (verbose) {
        message()
      }
    }

    tf <- duplicated(result_POWO$taxonName)
    if (any(tf)) {
      result_POWO <- result_POWO[!tf, ]
      row.names(result_POWO) <- 1:nrow(result_POWO)
      if (verbose) {
        message()
      }
    }

    if (nrow(result_FFB) == 0 && nrow(result_POWO) > 0) {
      result_merged <- data.frame(
        family = result_POWO$family,
        genus = NA_character_,
        taxonName = result_POWO$taxonName,
        scientificNameAuthorship = result_POWO$scientificNameAuthorship,
        FFB.vernacularName = NA_character_,
        country = result_POWO$country,
        endemism = ifelse(grepl("\\s[|]\\s", result_POWO$country), "Non-endemic", "Endemic"),
        botanical_country = result_POWO$botanical_country,
        introduced_to = result_POWO$introduced_to,
        FFB.establishmentMeans = NA_character_,
        FFB.stateProvince = NA_character_,
        FFB.vegetationType = NA_character_,
        FFB.genusRichness = NA_character_,
        FFB.genusRank = NA_character_,
        IUCN.status = result_POWO$IUCN.status,
        plant_uses_EN = NA_character_,
        plant_uses_PT = NA_character_,
        plant_uses_ES = NA_character_,
        plant_uses_FR = NA_character_,
        free_notes_EN = NA_character_,
        free_notes_PT = NA_character_,
        free_notes_ES = NA_character_,
        free_notes_FR = NA_character_,
        full_phrases_ADD_LANGUAGE = NA_character_,
        POWO.url = result_POWO$references
      )
    } else {
      result_merged <- data.frame(
        family = NA_character_,
        genus = NA_character_,
        taxonName = spp_list_updated,
        scientificNameAuthorship = NA_character_,
        FFB.vernacularName = NA_character_,
        country = NA_character_,
        endemism = NA_character_,
        botanical_country = NA_character_,
        introduced_to = NA_character_,
        FFB.establishmentMeans = NA_character_,
        FFB.stateProvince = NA_character_,
        FFB.phytogeographicDomain = NA_character_,
        FFB.vegetationType = NA_character_,
        FFB.genusRichness = NA_character_,
        FFB.genusRank = NA_character_,
        IUCN.status = NA_character_,
        plant_uses_EN = NA_character_,
        plant_uses_PT = NA_character_,
        plant_uses_ES = NA_character_,
        plant_uses_FR = NA_character_,
        free_notes_EN = NA_character_,
        free_notes_PT = NA_character_,
        free_notes_ES = NA_character_,
        free_notes_FR = NA_character_,
        full_phrases_ADD_LANGUAGE = NA_character_,
        POWO.url = NA_character_,
        FFB.url = NA_character_
      )

      # Filling in with FFB extracted data
      for (i in seq_along(result_FFB$taxonName)) {
        tf <- result_merged$taxonName %in% result_FFB$taxonName[i]
        result_merged$family[tf] <- result_FFB$family[i]
        result_merged$scientificNameAuthorship[tf] <- result_FFB$scientificNameAuthorship[i]
        result_merged$FFB.vernacularName[tf] <- result_FFB$vernacularName[i]
        result_merged$country[tf] <- result_FFB$country[i]
        result_merged$endemism[tf] <- result_FFB$endemism[i]
        result_merged$FFB.establishmentMeans[tf] <- result_FFB$establishmentMeans[i]
        result_merged$FFB.stateProvince[tf] <- result_FFB$stateProvince[i]
        result_merged$FFB.phytogeographicDomain[tf] <- result_FFB$phytogeographicDomain[i]
        result_merged$FFB.vegetationType[tf] <- result_FFB$vegetationType[i]
        result_merged$FFB.url[tf] <- result_FFB$references[i]
      }

      # Filling in with POWO extracted data
      for (i in seq_along(result_POWO$taxonName)) {
        tf <- result_merged$taxonName %in% result_POWO$taxonName[i]
        if (any(tf)) {
          result_merged$family[tf] <- result_POWO$family[i]
          result_merged$scientificNameAuthorship[tf] <- result_POWO$scientificNameAuthorship[i]
          result_merged$botanical_country[tf] <- result_POWO$botanical_country[i]
          result_merged$introduced_to[tf] <- result_POWO$introduced_to[i]
          result_merged$IUCN.status[tf] <- result_POWO$IUCN.status[i]
          result_merged$POWO.url[tf] <- result_POWO$references[i]
          if (result_merged$endemism[tf] %in% "Non-endemic" | is.na(result_merged$country[tf])) {
            result_merged$country[tf] <- result_POWO$country[i]
            result_merged$endemism[tf] <- ifelse(grepl("\\s[|]\\s", result_POWO$country[i]),
                                                 "Non-endemic",
                                                 "Endemic")
          }
        } else {
          tf <- result_FFB$original_query %in% result_POWO$taxonName[i]
          tf <- result_merged$taxonName %in% result_FFB$taxonName[tf]
          if (any(tf)) {
            result_merged$family[tf] <- result_POWO$family[i]
            result_merged$scientificNameAuthorship[tf] <- result_POWO$scientificNameAuthorship[i]
            result_merged$botanical_country[tf] <- result_POWO$botanical_country[i]
            result_merged$introduced_to[tf] <- result_POWO$introduced_to[i]
            result_merged$IUCN.status[tf] <- result_POWO$IUCN.status[i]
            result_merged$POWO.url[tf] <- result_POWO$references[i]
            if (result_merged$endemism[tf] %in% "Non-endemic") {
              result_merged$country[tf] <- result_POWO$country[i]
              result_merged$endemism[tf] <- ifelse(grepl("\\s[|]\\s", result_POWO$country[i]),
                                                   "Non-endemic",
                                                   "Endemic")
            }
          }
        }
      }
    }

    # ============================================================
    # Create dataframe and empty folders for personal audios ####
    # ============================================================

    result_merged <- result_merged %>%
      dplyr::arrange(family, taxonName)

    result_merged$genus <- gsub("\\s.*", "", result_merged$taxonName)
    result_merged$FFB.genusRichness <- genus_counts[result_merged$genus]
    result_merged$FFB.genusRank <- rank_by_genus[result_merged$genus]
    result_merged$FFB.genusRichness[is.na(result_merged$FFB.genusRichness)] <- 0

    result_merged <- .add_genus_curiosity_notes(
      result_merged = result_merged,
      max_diversity = max_diversity,
      n_max_genera = n_max_genera
    )

    if (save) {
      if (format == "csv") {
        .save_csv(df = result_merged,
                  verbose = verbose,
                  filename = filename,
                  dir = dir)
      } else if (format == "xlsx") {
        .save_xlsx(df = result_merged,
                   verbose = verbose,
                   filename = filename,
                   dir = dir)
      }
    }

  } else {

    data_path <- file.path(dir, files[grepl("[.]xlsx$|[.]csv$", files)])
    result_merged <- .read_species_data(data_path, verbose)
  }

  # Generate phrases for each requested language
  ui_strings <- .ui_strings()

  lang_button_label <- c(
    en = "English",
    pt = "Português",
    fr = "Français",
    es = "Español"
  )

  missing_langs <- setdiff(printed_lang, names(lang_button_label))
  if (length(missing_langs) > 0) {
    lang_button_label[missing_langs] <- toupper(missing_langs)
  }

  phrases_out <- .build_arboretum_phrases(
    data_path = NULL,
    df = result_merged,
    printed_lang = printed_lang,
    add_lang = add_lang,
    verbose = verbose
  )

  printed_lang <- phrases_out$printed_lang
  html_phrases <- phrases_out$html_phrases

  output_path <- file.path(dir, "__phrase_generating_guide.html")
  .save_phrase_html(
    df = result_merged,
    function_use = "_data",
    ui_strings = ui_strings,
    lang_button_label = lang_button_label,
    printed_lang = printed_lang,
    html_phrases = html_phrases,
    output_path = output_path,
    verbose = verbose
  )

  return(result_merged)
}


# Side function to mine plant data from POWO ####
.extract_powo_data <- function(result_POWO, sp, i){

  tax <- taxize::pow_search(sci_com = sp)
  pos <- which(tax[["data"]][["name"]] %in% sp)
  if (length(pos) == 0) return(result_POWO)

  if (any(!tax[["data"]][["accepted"]][pos])) {
    accepted_name <- stats::na.omit(tax[["data"]][["synonymOf"]][["name"]])
    if (length(accepted_name) == 0) return(result_POWO)
    result_POWO$taxonName[i] <- accepted_name

    # Search back in POWO with accepted name when the original search returns synonym
    tax <- taxize::pow_search(sci_com = accepted_name)
    pos <- which(tax[["data"]][["name"]] %in% accepted_name)
    url <- tax$data$url[pos][tax[["data"]][["accepted"]][pos]]
    id <- gsub(".*[/]taxon[/]", "", url)
    pow <- taxize::pow_lookup(id = id,
                              include = c("distribution", "descriptions"))
  } else {
    id <- gsub(".*[/]taxon[/]", "", tax$data$url)[pos]
    pow <- taxize::pow_lookup(id = id,
                              include = c("distribution", "descriptions"))
  }

  result_POWO$family[i] <- pow[["meta"]][["family"]]
  result_POWO$taxonName[i] <- pow[["meta"]][["name"]]
  result_POWO$scientificNameAuthorship[i] <- pow[["meta"]][["authors"]]

  native_vec <- pow[["meta"]][["distribution"]][["natives"]][["name"]]
  result_POWO$botanical_country[i] <- paste(native_vec, collapse = " | ")
  result_POWO$country[i] <- .botdiv_to_countries(result_POWO$botanical_country, i)

  introduced_vec <- pow[["meta"]][["distribution"]][["introduced"]][["name"]]
  if (length(pow[["meta"]][["distribution"]][["introduced"]][["name"]]) >= 1) {
    result_POWO$introduced_to[i] <- paste(introduced_vec, collapse = " | ")
  } else {
    result_POWO$introduced_to[i] <- NA
  }

  iucn_status <- pow[["meta"]][["descriptions"]][["IUCN"]][["descriptions"]][["conservation"]][["description"]]
  if (!is.null(iucn_status)) {
    iucn_status <- paste0(strsplit(iucn_status, " - ")[[1]][2], " (",
                          strsplit(iucn_status, " - ")[[1]][1], ")")
    result_POWO$IUCN.status[i] <- iucn_status
  }

  # The references column shows the URL for each species in POWO
  result_POWO$references[i] <-  paste0("https://powo.science.kew.org/taxon/", id)

  return(result_POWO)
}
