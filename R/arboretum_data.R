#' Extract and compile flora data from multiple taxonomic databases
#'
#' @author
#' Martin Boucknooghe & Domingos Cardoso
#'
#' @description
#' This function queries both [Flora e Funga do Brasil (FFB)](https://floradobrasil.jbrj.gov.br/consulta/)
#' and the [World Checklist of Vascular Plants (WCVP)](https://powo.science.kew.org/), the
#' backbone of Plants of the World Online (POWO), to retrieve taxonomic, distributional,
#' vernacular, and occurrence-related information for a given list of plant species. The
#' global conservation status of each species is retrieved from the
#' [IUCN Red List](https://www.iucnredlist.org/). It standardizes input names, handles synonyms,
#' merges data from both sources, and returns a single dataframe that can optionally be
#' saved as a CSV or Excel file. The output also includes genus-level richness and rank
#' information derived from FFB. When \code{save = TRUE}, the function also writes a
#' standalone HTML data reviewing guide (\code{__data_reviewing_guide.html}) to
#' \code{dir}, in which the retrieved data can be reviewed, corrected, and exported
#' back to the data file directly from the browser. Once the data have been
#' reviewed, the species phrases used by the labels are generated with
#' \code{arboretum_phrases()}.
#'
#' @param spp_list Required. A character vector of species names, for example
#'   `c("Euterpe edulis", "Coffea arabica")`. Names should be binomials without
#'   authorship. Leading and trailing whitespace are removed, and names are standardized
#'   internally before querying. An error is thrown if any element does not contain a
#'   space, indicating a probable non-species name.
#' @param printed_lang Character vector. Language(s) of the HTML data reviewing guide:
#'   the guide interface can be switched between them, and the plant-use and
#'   free-note fields of these languages (\code{plant_uses_*}, \code{free_notes_*})
#'   are shown for editing. Accepted values are `"pt"`, `"en"`, `"fr"`, and `"es"`.
#'   The data file always includes the plant-use and free-note columns of all four
#'   languages. Default is `c("pt", "en", "fr", "es")`.
#' @param verbose Logical. If `TRUE`, progress messages are printed to the console.
#'   Default is `TRUE`.
#' @param save Logical. If `TRUE`, the resulting dataframe is saved to disk and the
#'   HTML data reviewing guide is written to \code{dir}. Default is `TRUE`.
#' @param format Character string indicating the output file format. One of `"csv"`
#'   or `"xlsx"`. Partial matching is allowed through \code{match.arg()}.
#'   Default is `"csv"`.
#' @param filename Character string. Base name for the output file, without extension.
#'   Default is `"arboretum_data"`.
#' @param dir Character string. Directory path where the output file will be saved.
#'   Trailing slashes are automatically removed. The directory is created if it does
#'   not exist. Default is `"arboretum_data"`.
#' @param ffb_dir Character string. Path to the folder where Flora e Funga do Brasil
#'   (FFB) Darwin Core Archives are downloaded and kept between runs, as in
#'   \code{floraR::flora_download()}. Two kinds of paths are accepted:
#'   \itemize{
#'     \item A folder holding one or more downloaded versions (for example
#'       `"flora_download"`). The most recent version already downloaded there is
#'       always used, without contacting the FFB server, so the function also works
#'       offline. The latest version is only downloaded, with
#'       \code{floraR::flora_download()}, when the folder holds no downloaded
#'       version yet. To update a previously downloaded dataset, run
#'       \code{floraR::flora_download(dir = ffb_dir)}.
#'     \item A single version folder (for example
#'       `"flora_download/dwca_ffb_v393_430_latest"`), which is used as is, without
#'       any download.
#'   }
#'   Default is `"flora_download"`.
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
#'   \item \code{FFB.genusRichness}: Number of accepted species of the genus recorded in FFB.
#'   \item \code{FFB.genusRank}: Rank of the genus by species richness in FFB, with 1
#'     representing the richest genus.
#'   \item \code{plant_uses_EN}, \code{plant_uses_PT}, \code{plant_uses_ES},
#'     \code{plant_uses_FR}: Plant-use fields in English, Portuguese, Spanish, and French,
#'     intended for user annotation by direct editing of the saved file or via the
#'     HTML phrase guide of \code{arboretum_phrases()}.
#'   \item \code{free_notes_EN}, \code{free_notes_PT}, \code{free_notes_ES},
#'     \code{free_notes_FR}: Free-text note fields in English, Portuguese, Spanish, and
#'     French, intended for user annotation by direct editing of the saved file or via
#'     the HTML phrase guide of \code{arboretum_phrases()}.
#'   \item \code{IUCN.status}: Global IUCN Red List category, for example
#'     `"Endangered (EN)"`, or `NA` when the species has not been assessed.
#'   \item \code{POWO.url}: URL to the species page in POWO.
#'   \item \code{FFB.url}: URL to the species page in FFB.
#' }
#'
#' If a species is found in only one database, fields from the missing database are
#' returned as `NA`. Species not found in either database are omitted from the final
#' dataframe. For overlapping fields, FFB data generally take precedence, except for
#' selected WCVP-derived fields such as botanical country, introduced range, and POWO URL.
#'
#' The phrase columns (\code{full_phrases_*}) are not part of this output: they are
#' added by \code{arboretum_phrases()} once the data have been reviewed.
#'
#' When \code{save = TRUE}, the function also writes a standalone HTML data reviewing
#' guide (\code{__data_reviewing_guide.html}) to \code{dir}. It embeds the data and
#' provides, for each species, editable fields for \code{FFB.vernacularName},
#' \code{country}, \code{endemism}, \code{botanical_country}, \code{introduced_to},
#' \code{FFB.establishmentMeans}, \code{FFB.stateProvince},
#' \code{FFB.phytogeographicDomain}, \code{FFB.vegetationType}, \code{IUCN.status},
#' and the \code{plant_uses_*} and \code{free_notes_*} fields of \code{printed_lang}.
#' The \code{endemism}, \code{FFB.establishmentMeans}, and \code{IUCN.status}
#' fields are drop-down menus restricted to the values used to build the phrases.
#' The edited data can be saved or exported as CSV or XLSX without rerunning the
#' function.
#'
#' @details
#' The function follows five main steps:
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
#'   \item \strong{WCVP data extraction}
#'   \itemize{
#'     \item Loads the WCVP names and distribution tables once from
#'       \pkg{rWCVPdata}, so no per-species web requests are needed.
#'     \item Resolves synonyms to accepted names when possible.
#'     \item Retrieves family, authorship, native distribution (botanical countries),
#'       introduced range, and the POWO URL.
#'     \item Converts WCVP botanical countries to standard country names using an
#'       internal helper function.
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
#'
#'   \item \strong{IUCN Red List status}
#'   \itemize{
#'     \item If the \pkg{rredlist} package is installed and an IUCN Red List API token
#'       is stored in the \code{IUCN_REDLIST_KEY} environment variable (a free token
#'       can be requested at \url{https://api.iucnredlist.org}), the latest global
#'       assessment is retrieved from the official IUCN Red List API.
#'     \item Otherwise, the status is retrieved without a token from the IUCN Red List
#'       checklist mirrored by GBIF (\url{https://www.gbif.org}).
#'   }
#'
#' }
#'
#' Before processing, \code{spp_list} is cleaned using internal helper functions.
#' Leading and trailing whitespace are removed, names are standardized, and each element
#' is checked to ensure that it contains a space. The function stops with an error if
#' any element appears not to be a binomial species name.
#'
#' If \code{dir} already contains a CSV or XLSX data file, the species data are read
#' from that file instead of being retrieved again, and the data reviewing guide is
#' rebuilt from it. Delete or move that file to retrieve the data again.
#'
#' If \code{save = TRUE}, the function creates the output directory if needed and saves
#' the resulting dataframe either as a CSV file using \code{utils::write.csv()} or as
#' an Excel file using \code{openxlsx::write.xlsx()}. When \code{verbose = TRUE}, a
#' message reports the saved file path.
#'
#' The downloaded FFB dataset is kept in \code{ffb_dir}, so later runs reuse it
#' instead of downloading it again.
#'
#' @note
#' \itemize{
#'   \item The \pkg{floraR} package is required to download and parse the FFB Darwin Core
#'     Archive.
#'   \item The \pkg{rWCVPdata} package is required to access WCVP data.
#'   \item The \pkg{rredlist} package is optional and only used when an IUCN Red
#'     List API token is available.
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
#' \code{\link{arboretum_phrases}},
#' \code{\link[floraR]{flora_download}},
#' \code{\link[floraR]{flora_parse}},
#' \code{\link[rredlist]{rl_species}}
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
                           verbose = TRUE,
                           save = TRUE,
                           format = c("csv", "xlsx"),
                           filename = "arboretum_data",
                           dir = "arboretum_data",
                           ffb_dir = "flora_download"){

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
      references = NA_character_
    )

    # Download and parse Flora e Funga do Brasil DwC-A dataset ####

    if (!requireNamespace("floraR", quietly = TRUE)) {
      stop("Package 'floraR' is required for `arboretum_data()`. Please install it.")
    }

    dwca <- .load_ffb(ffb_dir, verbose = verbose)

    if (verbose) message("Flora e Funga do Brasil DwC-A dataset successfully loaded and parsed!")

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
    distribution_data[[6]][distribution_data[[6]] %in% "N\u00e3o endemica"] <- "Non-endemic"
    distribution_data[[7]][distribution_data[[7]] %in% "Amaz\u00f4nia"] <- "Amazon"
    distribution_data[[7]][distribution_data[[7]] %in% "Mata Atl\u00e2ntica"] <- "Atlantic Forest"

    #Genus statistics and synonym management for accurate data
    taxon_data_temp <- taxon_data[taxon_data$taxonomicStatus %in% "NOME_ACEITO", ]

    genus_richness_br <- as.data.frame(table(taxon_data_temp$genus))
    genus_ranked <- genus_richness_br[order(genus_richness_br$Freq, decreasing = TRUE), ]
    genus_ranked$rank <- 1:nrow(genus_ranked)
    max_diversity <- max(genus_ranked$Freq, na.rm = TRUE)
    n_max_genera <- sum(genus_ranked$Freq == max_diversity, na.rm = TRUE)
    genus_counts <- stats::setNames(genus_richness_br$Freq, genus_richness_br$Var1)
    rank_by_genus <- stats::setNames(genus_ranked$rank, genus_ranked$Var1)

    # Load WCVP names and distributions once for all queried species
    wcvp <- .load_wcvp()

    # Collection of data for each species ####
    for (i in seq_along(spp_list)) {
      sp <- spp_list[i]

      if (verbose) message(i, "/", length(spp_list), ": retrieving information from '", sp, "'")

      # WCVP data
      result_POWO <- .extract_wcvp_data(result_POWO, sp, i, wcvp)

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
      row.names(result_FFB) <- seq_len(nrow(result_FFB))
      if (verbose) {
        message()
      }
    }

    tf <- is.na(result_POWO$taxonName)
    if (any(tf)) {
      result_POWO <- result_POWO[!tf, ]
      row.names(result_POWO) <- seq_len(nrow(result_POWO))
      if (verbose) {
        message()
      }
    }

    tf <- duplicated(result_POWO$taxonName)
    if (any(tf)) {
      result_POWO <- result_POWO[!tf, ]
      row.names(result_POWO) <- seq_len(nrow(result_POWO))
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
        POWO.url = result_POWO$references,
        FFB.url = NA_character_
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

    # IUCN Red List status ####
    result_merged$IUCN.status <- .get_iucn_status(result_merged$taxonName,
                                                  verbose = verbose)

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
    data_filename <- paste0(filename, ".", format)

  } else {

    data_path <- file.path(dir, files[grepl("[.]xlsx$|[.]csv$", files)][1])
    if (verbose) {
      message("The data file '", data_path, "' already exists and is read instead of ",
              "retrieving the data again. Delete or move it to retrieve the data again.")
    }
    result_merged <- .read_species_data(data_path, verbose)
    data_filename <- basename(data_path)
  }

  # HTML data reviewing guide ####
  if (save) {
    .save_phrase_html(
      df = result_merged,
      function_use = "_review",
      ui_strings = .ui_strings(),
      lang_button_label = .lang_button_labels(printed_lang),
      printed_lang = printed_lang,
      html_phrases = list(),
      output_path = file.path(dir, "__data_reviewing_guide.html"),
      verbose = verbose,
      data_filename = data_filename
    )
  }

  return(result_merged)
}

# Side function to get and parse the FFB DwC-A dataset ####
# A previously downloaded FFB version is always used first, without contacting
# the FFB server; the latest version is only downloaded (with
# floraR::flora_download()) when ffb_dir holds no downloaded version yet. A path
# to a single version folder is used as is.
.load_ffb <- function(ffb_dir = "flora_download", verbose = TRUE) {
  if (!is.character(ffb_dir) || length(ffb_dir) != 1L || !nzchar(trimws(ffb_dir))) {
    stop("'ffb_dir' must be a single character string.", call. = FALSE)
  }
  ffb_dir <- gsub("/+$", "", trimws(ffb_dir))

  # A single version folder, e.g. "flora_download/dwca_ffb_v393_430_latest"
  if (.is_ffb_version_folder(ffb_dir)) {
    if (verbose) message("Using the FFB dataset in '", ffb_dir, "'")
    return(floraR::flora_parse(path = dirname(ffb_dir),
                               version = .ffb_folder_version(basename(ffb_dir)),
                               verbose = verbose))
  }

  local_folders <- .ffb_local_versions(ffb_dir)

  if (length(local_folders) == 0) {
    # Nothing downloaded yet: get the latest version
    tryCatch(
      floraR::flora_download(version = "latest", verbose = verbose, dir = ffb_dir),
      error = function(e) {
        stop("Could not download the Flora e Funga do Brasil dataset and no previously ",
             "downloaded version was found in '", ffb_dir, "'.\n",
             "Original error: ", conditionMessage(e), "\n",
             "Download it with floraR::flora_download() or set 'ffb_dir' to a folder ",
             "containing a downloaded FFB dataset.", call. = FALSE)
      }
    )
    local_folders <- .ffb_local_versions(ffb_dir)
    if (length(local_folders) == 0) {
      stop("No FFB dataset was found in '", ffb_dir, "'.", call. = FALSE)
    }
  } else if (verbose) {
    message("Using the FFB dataset previously downloaded in '", ffb_dir, "'. ",
            "To update it, run floraR::flora_download(dir = \"", ffb_dir, "\").")
  }

  # Use the most recent version available locally
  newest <- local_folders[order(package_version(.ffb_folder_version(local_folders)),
                                decreasing = TRUE)][1]

  floraR::flora_parse(path = ffb_dir, version = .ffb_folder_version(newest),
                      verbose = verbose)
}

# FFB version folders are named like "dwca_ffb_v393_430" or
# "dwca_ffb_v393_430_latest"
.ffb_folder_pattern <- function() "^dwca_ffb_v[0-9]+(_[0-9]+)*(_latest)?$"

.is_ffb_version_folder <- function(path) {
  dir.exists(path) &&
    grepl(.ffb_folder_pattern(), basename(path)) &&
    file.exists(file.path(path, "taxon.txt"))
}

.ffb_local_versions <- function(ffb_dir) {
  if (!dir.exists(ffb_dir)) return(character(0))
  folders <- list.files(ffb_dir)
  folders[grepl(.ffb_folder_pattern(), folders) &
            file.exists(file.path(ffb_dir, folders, "taxon.txt"))]
}

# Version string as used by floraR, e.g. "393.430"
.ffb_folder_version <- function(folder) {
  gsub("_", ".", gsub("^dwca_ffb_v|_latest$", "", folder))
}

# Side function to load the WCVP tables once per run ####
# Only species-rank names are kept for matching queried binomials, while the
# full names table is kept to resolve synonyms whose accepted name may sit at
# another rank.
.load_wcvp <- function() {
  wcvp_names <- rWCVPdata::wcvp_names
  wcvp_dist <- rWCVPdata::wcvp_distributions
  wcvp_dist <- wcvp_dist[!is.na(wcvp_dist$area), ]

  list(
    names = wcvp_names,
    species = wcvp_names[wcvp_names$taxon_rank %in% "Species", ],
    distributions = wcvp_dist
  )
}

# Side function to mine plant data from WCVP ####
.extract_wcvp_data <- function(result_POWO, sp, i, wcvp){

  matches <- wcvp$species[wcvp$species$taxon_name %in% sp, ]
  if (nrow(matches) == 0) return(result_POWO)

  # The same binomial may appear more than once in WCVP (e.g. an accepted name
  # plus illegitimate homonyms), so prefer the accepted record, then a synonym
  # pointing to an accepted name
  if (any(matches$taxon_status %in% "Accepted")) {
    accepted <- matches[matches$taxon_status %in% "Accepted", ][1, ]
  } else {
    syn <- matches[matches$taxon_status %in% "Synonym" &
                     !is.na(matches$accepted_plant_name_id), ]
    if (nrow(syn) == 0) return(result_POWO)
    accepted <- wcvp$names[wcvp$names$plant_name_id %in% syn$accepted_plant_name_id[1], ]
    if (nrow(accepted) == 0) return(result_POWO)
    accepted <- accepted[1, ]
  }

  dist <- wcvp$distributions[wcvp$distributions$plant_name_id %in% accepted$plant_name_id, ]
  native <- unique(dist$area[dist$introduced %in% 0])
  introduced <- unique(dist$area[dist$introduced >= 1])

  result_POWO$family[i] <- accepted$family
  result_POWO$taxonName[i] <- accepted$taxon_name
  result_POWO$scientificNameAuthorship[i] <- accepted$taxon_authors
  result_POWO$botanical_country[i] <- if (length(native) > 0) {
    paste(native, collapse = " | ")
  } else {
    NA_character_
  }
  result_POWO$country[i] <- .botdiv_to_countries(result_POWO$botanical_country, i)
  result_POWO$introduced_to[i] <- if (length(introduced) > 0) {
    paste(introduced, collapse = " | ")
  } else {
    NA_character_
  }

  # The references column shows the URL for each species in POWO
  result_POWO$references[i] <- paste0("https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:",
                                      accepted$powo_id)

  return(result_POWO)
}
