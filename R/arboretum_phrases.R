#' Generate editable multilingual species phrases
#'
#' @author
#' Martin Boucknooghe & Domingos Cardoso
#'
#' @description
#' This function builds the natural-language species descriptions used by the
#' species labels and audio guides. It is meant to be run once the species data
#' retrieved with \code{arboretum_data()} have been reviewed. The phrases are
#' generated from the data file and stored in it, in the columns
#' \code{full_phrases_EN}, \code{full_phrases_PT}, \code{full_phrases_ES},
#' \code{full_phrases_FR}, and \code{full_phrases_ADD_LANGUAGE}, which are created
#' at this step only. The function also writes a standalone HTML phrase guide
#' (\code{__phrase_generating_guide.html}) next to the data file, which allows
#' users to review and edit the phrases and annotation fields, and to export the
#' updated data directly from the browser.
#'
#' @param data_path Required. Path to the CSV or XLSX species data file created by
#'   \code{arboretum_data()}. The file is updated in place with the phrase columns.
#' @param printed_lang Character vector. Built-in language(s) for which phrases are
#'   generated. Accepted values are `"pt"`, `"en"`, `"fr"`, and `"es"`.
#'   Default is `c("pt", "en", "fr", "es")`.
#' @param add_lang Character string or `NULL`. Optional code for one additional
#'   language, for example `"PANARA"` or `"TUKANO"`. When supplied, the HTML phrase
#'   guide includes a dedicated editable field for entering a full phrase translation
#'   for the specified custom language. The content entered there is stored in the
#'   \code{full_phrases_ADD_LANGUAGE} column and is used by \code{arboretum_labels()}
#'   to include that language in the species labels. Default is \code{NULL}.
#' @param overwrite Logical. If `FALSE` (default), phrases already stored in the
#'   \code{full_phrases_*} columns are kept, so manual edits are never lost, and
#'   only empty cells are filled. If `TRUE`, all phrases of \code{printed_lang} are
#'   generated again from the data, for example after correcting the species data.
#'   \code{full_phrases_ADD_LANGUAGE} is never overwritten.
#' @param save Logical. If `TRUE` (default), the data file is updated with the
#'   phrase columns and the HTML phrase guide is written next to it.
#' @param verbose Logical. If `TRUE`, progress messages are printed to the console.
#'   Default is `TRUE`.
#'
#' @return
#' Invisibly, the species dataframe with the phrase columns:
#' \itemize{
#'   \item \code{full_phrases_EN}, \code{full_phrases_PT}, \code{full_phrases_ES},
#'     \code{full_phrases_FR}: Natural-language species descriptions generated for
#'     each language in \code{printed_lang}. They can be manually edited in the data
#'     file or in the HTML phrase guide. Plant uses and free notes are appended after
#'     the phrase in the labels, so there is no need to repeat them here.
#'   \item \code{full_phrases_ADD_LANGUAGE}: Complete phrase in the custom language
#'     specified by \code{add_lang}, entered by the user via the HTML phrase guide or
#'     direct editing.
#' }
#'
#' @details
#' Phrases describe the taxonomy, vernacular names, distribution and endemism,
#' establishment means, introduced range, phytogeographic domains, vegetation types,
#' and IUCN Red List status of each species, using the multilingual internal
#' dictionary of the package.
#'
#' Running \code{arboretum_phrases()} again only fills empty phrase cells, unless
#' \code{overwrite = TRUE}. Clearing a phrase in the data file or in the HTML guide
#' therefore makes it be generated again on the next run.
#'
#' \code{arboretum_audios()} and \code{arboretum_labels()} only read the stored
#' phrases and never generate them, so \code{arboretum_phrases()} must be run before
#' them.
#'
#' @seealso
#' \code{\link{arboretum_data}}, \code{\link{arboretum_audios}},
#' \code{\link{arboretum_labels}}
#'
#' @examples
#' \dontrun{
#' # 1. Retrieve and review the species data
#' arboretum_data(
#'   spp_list = c("Euterpe edulis", "Paubrasilia echinata"),
#'   dir = "arboretum_data"
#' )
#'
#' # 2. Generate the phrases once the data are fine
#' arboretum_phrases(
#'   data_path = "arboretum_data/arboretum_data.csv",
#'   printed_lang = c("pt", "en"),
#'   add_lang = "TUKANO"
#' )
#'
#' # Generate all phrases again after correcting the data
#' arboretum_phrases(
#'   data_path = "arboretum_data/arboretum_data.csv",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export

arboretum_phrases <- function(data_path = NULL,
                              printed_lang = c("pt", "en", "fr", "es"),
                              add_lang = NULL,
                              overwrite = FALSE,
                              save = TRUE,
                              verbose = TRUE) {

  # Input validation ####
  printed_lang <- .arg_check_printed_lang(printed_lang)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }

  df <- .read_species_data(data_path, verbose)
  had_phrase_cols <- all(.phrase_cols() %in% names(df))

  # Generate the phrases and store them in the full_phrases_* columns ####
  generated <- .generate_phrases(
    df = df,
    printed_lang = printed_lang,
    overwrite = overwrite,
    verbose = verbose
  )
  df <- generated$df

  # Save the data file in place, only when something changed ####
  if (save && (!had_phrase_cols || generated$n_generated > 0)) {
    filename <- tools::file_path_sans_ext(basename(data_path))
    dir <- dirname(data_path)
    if (tolower(tools::file_ext(data_path)) == "csv") {
      .save_csv(df = df, verbose = verbose, filename = filename, dir = dir)
    } else {
      .save_xlsx(df = df, verbose = verbose, filename = filename, dir = dir)
    }
  }

  # HTML phrase guide ####
  if (save) {
    phrases_out <- .stored_phrases(
      df = df,
      printed_lang = printed_lang,
      add_lang = add_lang,
      verbose = verbose
    )

    .save_phrase_html(
      df = df,
      function_use = "_data",
      ui_strings = .ui_strings(),
      lang_button_label = .lang_button_labels(phrases_out$printed_lang),
      printed_lang = phrases_out$printed_lang,
      html_phrases = phrases_out$html_phrases,
      output_path = file.path(dirname(data_path), "__phrase_generating_guide.html"),
      verbose = verbose,
      add_lang = add_lang,
      data_filename = basename(data_path)
    )
  }

  invisible(df)
}
