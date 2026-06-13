#' Generate printable QR code labels for plant collection specimens
#'
#' @author
#' Domingos Cardoso
#'
#' @description
#' This function generates print-ready QR code labels for living plant collection
#' specimens using a species data file, typically produced by
#' \code{arboretum_data()}. Labels are arranged on A4 pages and saved as PDF
#' or PNG files. If the number of labels exceeds the page capacity, the function
#' automatically creates multiple output files, one per page.
#'
#' Two visual layouts are available:
#' \itemize{
#' \item \code{"minimalist"}: a compact label with a border, scientific name,
#' optional first vernacular name, and QR code.
#' \item \code{"complete"}: a larger botanical label with scientific name,
#' authorship, family name, optional first vernacular name, optional
#' language-specific endemism phrase, QR code, optional specimen identifier,
#' and optional institutional logo.
#' }
#'
#' QR codes can link to species pages in a published minisite, to a single shared
#' URL, to POWO or FFB source pages from the input data, or, when no URL is
#' available, encode the taxon name as plain text.
#'
#' @param data_path Required. Character string giving the path to the mined species
#' data file. The file must be in CSV or Excel format and is typically generated
#' by \code{arboretum_data()}. It should contain, at minimum,
#' \code{taxonName} and \code{family}. Additional columns such as
#' \code{scientificNameAuthorship}, \code{FFB.vernacularName}, \code{endemism},
#' \code{country}, \code{POWO.url}, and \code{FFB.url} are used when available.
#' @param species Character vector or \code{NULL}. Optional subset of species names
#' to process. Values must match entries in the \code{taxonName} column. If
#' \code{NULL}, all species in the input file are included. Default is
#' \code{NULL}.
#' @param printed_lang Character vector. Language code used for the endemism phrase
#' in \code{"complete"} labels. Accepted values are \code{"pt"}, \code{"en"},
#' \code{"fr"}, and \code{"es"}. When more than one language is supplied and
#' \code{layout = "complete"}, the first valid value selected by
#' \code{match.arg()} is used. This argument is ignored for
#' \code{"minimalist"} labels. Default is \code{c("pt", "en", "fr", "es")}.
#' @param length Numeric or \code{NULL}. Label height in centimetres. If
#' \code{NULL}, the default is \code{2.5} cm for \code{"minimalist"} labels and
#' \code{8.0} cm for \code{"complete"} labels.
#' @param width Numeric or \code{NULL}. Label width in centimetres. If
#' \code{NULL}, the default is \code{2.5} cm for \code{"minimalist"} labels and
#' \code{6.0} cm for \code{"complete"} labels.
#' @param path_to_logo Character string or \code{NULL}. Optional path to a PNG or
#' JPEG logo file. The logo is shown only in \code{"complete"} labels, near the
#' bottom-right of each label. The image aspect ratio is preserved. If the logo
#' file is missing or unsupported, the function warns and continues without it.
#' Default is \code{NULL}.
#' @param layout Character string. Label layout. One of \code{"minimalist"} or
#' \code{"complete"}. Default is the first option, \code{"minimalist"}.
#' @param id_code Character vector or \code{NULL}. Optional specimen or collection
#' identifier printed on \code{"complete"} labels. If a single value matching a
#' column name in the input data is supplied, values are taken from that column.
#' Otherwise, supplied values are recycled or used directly to match the selected
#' species. Ignored visually for \code{"minimalist"} labels. Default is
#' \code{NULL}.
#' @param base_url Character string or \code{NULL}. Root URL of a published minisite
#' containing species label pages. When supplied, each QR code is linked to a
#' species-specific HTML page using the same filename convention as
#' \code{arboretum_labels()}:
#' \code{base_url/FAMILY_Genus_species_label.html}. This option has priority over
#' \code{url}, \code{POWO.url}, and \code{FFB.url}. Default is \code{NULL}.
#' @param url Character string, character vector, or \code{NULL}. URL to encode in
#' the QR code. If a single URL is supplied, it is used for all labels. If a vector
#' is supplied, it should match the number of selected species. Ignored when
#' \code{base_url} is supplied. Default is \code{NULL}.
#' @param color Character string or \code{NULL}. Hex colour used for the dark QR
#' modules. If \code{NULL}, the default dark forest green \code{"#1a2e1a"} is
#' used.
#' @param font_family Character string. Font family used for the scientific name.
#' Any font available to the active R graphics device can be used. Common values
#' include \code{"serif"}, \code{"sans"}, and \code{"mono"}. Default is
#' \code{"serif"}.
#' @param format Character string. Output file format. One of \code{"pdf"} or
#' \code{"png"}. Default is \code{"pdf"}.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed to the
#' console. Default is \code{TRUE}.
#' @param dir Character string. Output directory where the QR code pages will be
#' saved. The directory is created if it does not exist. Default is
#' \code{"arboretum_qrcodes"}.
#'
#' @return
#' Invisibly returns a character vector with the paths to the saved output files,
#' one path per generated page. The function is called mainly for its side effects:
#' \itemize{
#' \item reads the input species data;
#' \item optionally filters the data to selected species;
#' \item generates QR codes;
#' \item arranges labels on one or more A4 pages;
#' \item saves the pages as PDF or PNG files.
#' }
#'
#' @details
#' The function follows five main steps:
#'
#' \enumerate{
#' \item \strong{Input preparation}
#' \itemize{
#' \item Reads the species data using an internal helper function.
#' \item Optionally filters the data to the species supplied in \code{species}.
#' \item Validates the output format, layout, output directory, and label
#' dimensions.
#' }
#'
#' \item \strong{URL selection}
#' \itemize{
#' \item If \code{base_url} is supplied, the function builds one species-specific
#' minisite URL per label.
#' \item If \code{base_url} is not supplied but \code{url} is supplied,
#' \code{url} is encoded.
#' \item If neither is supplied, the function uses \code{POWO.url} when available.
#' \item If \code{POWO.url} is unavailable, the function uses \code{FFB.url}
#' when available.
#' \item If no URL is available, the taxon name is encoded as plain text.
#' }
#'
#' \item \strong{Page layout}
#' \itemize{
#' \item Labels are arranged on A4 pages measuring 21.0 × 29.7 cm.
#' \item Pages use 0.8 cm margins and 0.4 cm gaps between labels.
#' \item The number of rows and columns is calculated automatically from the
#' selected label dimensions.
#' \item Additional pages are created automatically when needed.
#' }
#'
#' \item \strong{Minimalist labels}
#' \itemize{
#' \item Display the scientific name in italic.
#' \item Display the first vernacular name when available.
#' \item Display a QR code with a white quiet zone for scanning reliability.
#' }
#'
#' \item \strong{Complete labels}
#' \itemize{
#' \item Display scientific name, authorship, family, and first vernacular name
#' when available.
#' \item Display a language-specific endemism phrase when \code{endemism} is
#' \code{"Endemic"} and a country value is available.
#' \item Display an optional specimen identifier from \code{id_code}.
#' \item Display an optional logo when \code{path_to_logo} is supplied.
#' \item Display a QR code with a white quiet zone for scanning reliability.
#' }
#' }
#'
#' Output files are named \code{arboretum_qrcodes.pdf} or
#' \code{arboretum_qrcodes.png} when all labels fit on one page. When multiple
#' pages are needed, files are named \code{arboretum_qrcodes_page01.pdf},
#' \code{arboretum_qrcodes_page02.pdf}, and so on, or with the equivalent
#' \code{.png} extension.
#'
#' @note
#' \itemize{
#' \item The function does not publish minisites or verify that URLs are online.
#' \item For \code{base_url}, trailing slashes are removed before species-specific
#' label filenames are appended.
#' \item The first vernacular name is used when \code{FFB.vernacularName} contains
#' multiple names separated by \code{"|"}.
#' \item PNG logos are read with \pkg{png}. JPEG logos require the \pkg{jpeg}
#' package to be installed.
#' \item Unsupported or unreadable logos are ignored with a warning.
#' \item Very small custom label dimensions may make text difficult to read or QR
#' codes difficult to scan.
#' }
#'
#' @seealso
#' \code{\link{arboretum_data}},
#' \code{\link{arboretum_labels}},
#' \code{\link{arboretum_minisite}}
#'
#' @examples
#' \dontrun{
#' # Minimalist QR codes using POWO or FFB URLs from the mined data
#' arboretum_qrcodes(
#' data_path = "arboretum_data/arboretum_data.xlsx"
#' )
#'
#' # QR codes linked to species pages in a published minisite
#' arboretum_qrcodes(
#' data_path = "arboretum_data/arboretum_data.xlsx",
#' base_url = "https://dboslab.github.io/jbrj-arboretum"
#' )
#'
#' # Complete labels with specimen IDs and a logo
#' arboretum_qrcodes(
#' data_path = "arboretum_data/arboretum_data.xlsx",
#' species = c("Euterpe edulis", "Paubrasilia echinata"),
#' layout = "complete",
#' printed_lang = "pt",
#' base_url = "https://dboslab.github.io/jbrj-arboretum",
#' id_code = c("JBRJ-001", "JBRJ-002"),
#' path_to_logo = "path/to/institution_logo.png",
#' format = "pdf"
#' )
#'
#' # Complete labels using specimen IDs stored in a data column
#' arboretum_qrcodes(
#' data_path = "arboretum_data/arboretum_data.xlsx",
#' layout = "complete",
#' id_code = "accession_id",
#' printed_lang = "en"
#' )
#'
#' # Larger minimalist PNG labels with a custom QR colour
#' arboretum_qrcodes(
#' data_path = "arboretum_data/arboretum_data.xlsx",
#' width = 3.5,
#' length = 3.5,
#' color = "#3b1f0a",
#' format = "png"
#' )
#' }
#'
#' @importFrom qrcode qr_code
#' @importFrom png readPNG
#' @importFrom grid grid.newpage grid.rect grid.text grid.raster grid.lines pushViewport popViewport viewport unit gpar
#' @importFrom grDevices pdf png dev.off as.raster
#' @importFrom tools file_ext
#'
#' @export

arboretum_qrcodes <- function(data_path = NULL,
                              species = NULL,
                              printed_lang = c("pt", "en", "fr", "es"),
                              length = NULL,
                              width = NULL,
                              path_to_logo = NULL,
                              layout = c("minimalist", "complete"),
                              id_code = NULL,
                              base_url = NULL,
                              url = NULL,
                              color = NULL,
                              font_family = "serif",
                              format = c("pdf", "png"),
                              verbose = TRUE,
                              dir = "arboretum_qrcodes") {

  layout <- match.arg(layout)
  format <- match.arg(format)
  if (layout == "complete") {
  printed_lang <- match.arg(printed_lang, choices = c("en", "pt", "es", "fr"))
  }
  dir <- .arg_check_dir(dir)

  df <- .read_species_data(data_path, verbose)

  # Optional species filter
  if (!is.null(species)) {
    species <- trimws(species)
    tf <- df$taxonName %in% species
    if (!any(tf))
      stop("None of the specified species were found in the data.\n",
           "Available: ", paste(df$taxonName, collapse = ", "), call. = FALSE)
    df <- df[tf, , drop = FALSE]
  }

  # Label dimensions (cm)
  default_w <- if (layout == "minimalist") 2.5 else 6.0
  default_h <- if (layout == "minimalist") 2.5 else 8.0
  label_w <- if (is.null(width))  default_w else as.numeric(width)
  label_h <- if (is.null(length)) default_h else as.numeric(length)
  if (label_w <= 0 || label_h <= 0)
    stop("'width' and 'length' must be positive numbers.", call. = FALSE)

  # URLs to encode in QR codes
  # Priority: base_url > url > POWO.url > FFB.url > taxon name
  n_sp <- nrow(df)
  if (!is.null(base_url)) {
    # Build per-species deep-links matching arboretum_labels() filename convention:
    # {base_url}/FAMILY_Genus_species_label.html
    base_url <- gsub("/+$", "", trimws(base_url))   # strip trailing slash(es)
    qr_urls <- paste0(base_url, "/",
                       toupper(df$family), "_",
                       gsub("\\s+", "_", df$taxonName),
                       "_label.html")
    if (verbose)
      message("Using minisite base URL — linking each QR code to its species label page.")
  } else if (!is.null(url)) {
    qr_urls <- if (length(url) == 1L) rep(url, n_sp) else url
  } else if ("POWO.url" %in% names(df) && any(!is.na(df$POWO.url))) {
    qr_urls <- ifelse(!is.na(df$POWO.url) & nzchar(df$POWO.url),
                      df$POWO.url, df$taxonName)
  } else if ("FFB.url" %in% names(df) && any(!is.na(df$FFB.url))) {
    qr_urls <- ifelse(!is.na(df$FFB.url) & nzchar(df$FFB.url),
                      df$FFB.url, df$taxonName)
  } else {
    qr_urls <- df$taxonName
    if (verbose) message("No URL provided — encoding taxon names in QR codes.")
  }

  # Specimen ID codes
  if (!is.null(id_code) && length(id_code) == 1L && id_code %in% names(df)) {
    id_codes <- as.character(df[[id_code]])
  } else if (!is.null(id_code)) {
    id_codes <- if (length(id_code) == 1L) rep(as.character(id_code), n_sp)
                else as.character(id_code)
  } else {
    id_codes <- rep(NA_character_, n_sp)
  }

  # QR module colour
  qr_color <- if (is.null(color)) "#1a2e1a" else color

  # Output directory
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (verbose) message("Created directory: ", dir)
  }

  # A4 page constants (cm)
  a4_w <- 21.0
  a4_h <- 29.7
  margin <- 0.8
  gap <- 0.4

  avail_w <- a4_w - 2 * margin
  avail_h <- a4_h - 2 * margin
  n_cols <- max(1L, floor(avail_w / (label_w + gap)))
  n_rows <- max(1L, floor(avail_h / (label_h + gap)))
  per_page <- n_cols * n_rows
  n_pages <- ceiling(n_sp / per_page)

  if (verbose) {
    message(sprintf(
      "Layout: %-12s | Size: %.1f × %.1f cm | Grid: %d×%d (%d/page) | Pages: %d",
      layout, label_w, label_h, n_cols, n_rows, per_page, n_pages
    ))
  }

  saved <- character(n_pages)

  for (pg in seq_len(n_pages)) {

    from <- (pg - 1L) * per_page + 1L
    to <- min(pg * per_page, n_sp)
    base <- if (n_pages == 1L) "arboretum_qrcodes"
            else sprintf("arboretum_qrcodes_page%02d", pg)

    if (format == "pdf") {
      out <- file.path(dir, paste0(base, ".pdf"))
      grDevices::pdf(out,
                     width = a4_w / 2.54,
                     height = a4_h / 2.54,
                     compress = TRUE)
    } else {
      out <- file.path(dir, paste0(base, ".png"))
      grDevices::png(out,
                     width = a4_w,
                     height = a4_h,
                     units = "cm",
                     res = 300)
    }

    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f9f8f6", col = NA))

    item <- 0L
    for (r in seq_len(n_rows)) {
      for (cl in seq_len(n_cols)) {
        item <- item + 1L
        idx <- from + item - 1L
        if (idx > to) break

        # Label position: fill top-to-bottom, left-to-right
        # y_cm is the bottom edge measured from the page bottom
        x_cm <- margin + (cl - 1L) * (label_w + gap)
        y_cm <- a4_h - margin - r * label_h - (r - 1L) * gap

        if (layout == "minimalist") {
          .draw_minimalist_qr(
            species_name = df$taxonName[idx],
            vernacular_name = df$FFB.vernacularName[idx],
            qr_url = qr_urls[idx],
            x_cm = x_cm, y_cm = y_cm,
            w_cm = label_w, h_cm = label_h,
            qr_color = qr_color,
            font_family = font_family,
            a4_w = a4_w, a4_h = a4_h
          )
        } else {
          .draw_complete_qr(
            species_name = df$taxonName[idx],
            authorship = df$scientificNameAuthorship[idx],
            family_name = df$family[idx],
            vernacular_name = df$FFB.vernacularName[idx],
            endemism = df$endemism[idx],
            country = df$country[idx],
            qr_url = qr_urls[idx],
            id_code = id_codes[idx],
            path_to_logo = path_to_logo,
            x_cm = x_cm, y_cm = y_cm,
            w_cm = label_w, h_cm = label_h,
            qr_color = qr_color,
            font_family = font_family,
            printed_lang = printed_lang,
            a4_w = a4_w, a4_h = a4_h
          )
        }
      }
    }

    grDevices::dev.off()
    saved[pg] <- out
    if (verbose) message("Saved: ", normalizePath(out, winslash = "/"))
  }

  invisible(saved)
}


# ── Drawing: minimalist layout ────────────────────────────────────────────────

.draw_minimalist_qr <- function(species_name, vernacular_name, qr_url,
                                 x_cm, y_cm, w_cm, h_cm,
                                 qr_color, font_family, a4_w, a4_h) {

  vp <- grid::viewport(
    x = grid::unit(x_cm / a4_w, "npc"),
    y = grid::unit(y_cm / a4_h, "npc"),
    width = grid::unit(w_cm / a4_w, "npc"),
    height = grid::unit(h_cm / a4_h, "npc"),
    just = c("left", "bottom"),
    clip = "on"
  )
  grid::pushViewport(vp)

  # White background + thin border
  grid::grid.rect(gp = grid::gpar(fill = "white", col = "#2a2a2a", lwd = 0.65))

  # ── Content ───────────────────────────────────────────────────────────────
  # Vernacular name: first entry only (the label is multilingual-agnostic here)
  vern_raw <- if (is.na(vernacular_name) || !nzchar(vernacular_name)) ""
                else as.character(vernacular_name)
  vern_clean <- if (nzchar(vern_raw))
                  trimws(strsplit(vern_raw, "|", fixed = TRUE)[[1L]][1L])
                else ""
  has_vern <- nzchar(vern_clean)

  # ── Font sizes (scaled to label width; reference 2.5 cm) ─────────────────
  name_pt <- max(3, round(3.5 * w_cm / 2.5, 0))
  vern_pt <- max(3, round(3.0 * w_cm / 2.5, 0))   # smaller than name

  # ── QR size (computed here — needed to derive separator position) ──────────
  qr_side_cm <- min(w_cm * 0.84, h_cm * 0.84)
  qr_side_npc <- qr_side_cm / h_cm

  # ── Text block positions ───────────────────────────────────────────────────
  name_y <- 0.938

  if (has_vern) {
    gap_nv <- ((name_pt / 2 + vern_pt / 2) * 0.03527 + 0.018) / h_cm
    vern_y <- name_y - gap_nv
    text_bot <- vern_y - (vern_pt / 2) * 0.03527 / h_cm
  } else {
    text_bot <- name_y - (name_pt / 2) * 0.03527 / h_cm
  }

  # ── Separator: visual midpoint between text block and QR data ─────────────
  # With quiet_zone = 2, the raster's top quiet zone ≈ 6 % of its height.
  # Setting sep_y = text_bot − 0.06 × qr_side_npc places the separator exactly
  # halfway: the gap above equals the quiet-zone gap below.
  sep_y <- text_bot - 0.06 * qr_side_npc

  # ── Species name ──────────────────────────────────────────────────────────
  grid::grid.text(
    label = species_name,
    x = 0.5,
    y = name_y,
    gp = grid::gpar(fontface = "italic",
                       fontsize = name_pt,
                       col = "#606060",
                       fontfamily = font_family),
    just = "center"
  )

  # ── Common name (smaller, grey, below species name) ───────────────────────
  if (has_vern) {
    grid::grid.text(
      label = vern_clean,
      x = 0.5,
      y = vern_y,
      gp = grid::gpar(fontface = "plain",
                         fontsize = vern_pt,
                         col = "#909090",
                         fontfamily = "sans"),
      just = "center"
    )
  }

  # ── Hairline separator — directly below text block ────────────────────────
  grid::grid.lines(
    x = grid::unit(c(0.07, 0.93), "npc"),
    y = grid::unit(c(sep_y, sep_y), "npc"),
    gp = grid::gpar(col = "#d8d8d8", lwd = 0.25)
  )

  # ── QR code ───────────────────────────────────────────────────────────────
  # Raster top starts at sep_y; quiet_zone = 2 provides the lower half of the
  # visual gap (matching the space above the separator).
  qr_center_y <- sep_y - qr_side_cm / (2 * h_cm)
  safe_url <- if (is.na(qr_url) || !nzchar(qr_url)) species_name else qr_url
  qr_mat <- qrcode::qr_code(safe_url, ecl = "M")
  qr_raster <- .qr_to_raster(qr_mat, qr_color, quiet_zone = 2L)

  grid::grid.raster(
    image = qr_raster,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(qr_center_y, "npc"),
    width = grid::unit(qr_side_cm, "cm"),
    height = grid::unit(qr_side_cm, "cm"),
    interpolate = FALSE
  )

  grid::popViewport()
}


# ── Drawing: complete layout ──────────────────────────────────────────────────

.draw_complete_qr <- function(species_name, authorship, family_name,
                               vernacular_name, endemism, country,
                               qr_url, id_code, path_to_logo,
                               x_cm, y_cm, w_cm, h_cm,
                               qr_color, font_family, printed_lang, a4_w, a4_h) {

  vp <- grid::viewport(
    x = grid::unit(x_cm / a4_w, "npc"),
    y = grid::unit(y_cm / a4_h, "npc"),
    width = grid::unit(w_cm / a4_w, "npc"),
    height = grid::unit(h_cm / a4_h, "npc"),
    just = c("left", "bottom"),
    clip = "on"
  )
  grid::pushViewport(vp)

  # White background + dark green border
  grid::grid.rect(gp = grid::gpar(fill = "#ffffff", col = "#1e2e1e", lwd = 0.85))

  # ── Load logo early — its actual height is needed to size the footer zone ───
  logo_img <- .load_logo_raster(path_to_logo)
  logo_w_cm <- 0; logo_h_cm <- 0
  if (!is.null(logo_img)) {
    logo_aspect <- dim(logo_img)[2L] / dim(logo_img)[1L]
    logo_max_h_cm <- h_cm * 0.165
    logo_max_w_cm <- w_cm * 0.82
    if (logo_max_w_cm / logo_aspect <= logo_max_h_cm) {
      logo_w_cm <- logo_max_w_cm; logo_h_cm <- logo_max_w_cm / logo_aspect
    } else {
      logo_h_cm <- logo_max_h_cm; logo_w_cm <- logo_max_h_cm * logo_aspect
    }
  }

  # ── id_code content and font computed early — both needed for footer_top ──
  id_clean <- if (!is.null(id_code) && !is.na(id_code) && nzchar(id_code))
                as.character(id_code)
              else ""
  id_pt <- max(4, round(6.5 * w_cm / 6.0, 0))
  id_h_npc <- id_pt * 0.03527 / h_cm   # id text half-height in viewport NPC

  # ── Layout zones ──────────────────────────────────────────────────────────
  header_top <- 0.960
  sep_y <- 0.650
  pad <- 0.065
  has_logo <- !is.null(logo_img)
  has_id <- nzchar(id_clean)

  # footer_top is sized so the QR bottom lands exactly at logo_top − gap, the
  # logo bottom lands at id_code centre, and id_code sits just above the border.
  # Accounting for the 0.96 QR-height factor:
  #   footer_top = (qr_needed_bottom − sep_y × 0.04) / 0.96
  # where qr_needed_bottom = border_pad + id_half + logo_h/h + qr_logo_gap
  if (has_logo) {
    qr_needed_bottom <- 0.025 + (if (has_id) id_h_npc / 2 else 0) +
                        logo_h_cm / h_cm + 0.012
    footer_top <- max(0.020, (qr_needed_bottom - sep_y * 0.04) / 0.96)
  } else if (has_id) {
    footer_top <- 0.055
  } else {
    footer_top <- 0.020
  }

  # Horizontal padding
  pad <- 0.065

  # ── Content computed first (needed for dynamic y-positions) ───────────────

  # Vernacular name (first entry only)
  vern_raw <- if (is.na(vernacular_name) || !nzchar(vernacular_name)) ""
                else as.character(vernacular_name)
  vern_clean <- if (nzchar(vern_raw))
                  trimws(strsplit(vern_raw, "|", fixed = TRUE)[[1L]][1L])
                else ""

  # Endemism phrase (language-aware)
  endem_val <- if (!is.null(endemism) && !is.na(endemism)) as.character(endemism) else ""
  cntr_val <- if (!is.null(country)  && !is.na(country))  as.character(country)  else ""
  cntr_first <- trimws(strsplit(cntr_val, "|", fixed = TRUE)[[1L]][1L])

  endemic_text <- ""
  if (endem_val == "Endemic" && nzchar(cntr_first)) {
    endemic_text <- switch(printed_lang,
      en = paste0("Endemic of ", cntr_first),
      pt = if (cntr_first == "Brazil") "Endêmica do Brasil"
           else paste0("Endêmica de ", cntr_first),
      es = paste0("Endémica de ",
                  if (cntr_first == "Brazil") "Brasil" else cntr_first),
      fr = if (cntr_first == "Brazil") "Endémique du Brésil"
           else paste0("Endémique de ", cntr_first),
      paste0("Endemic of ", cntr_first)
    )
  }

  # ── Font sizes (scaled to label width; reference 6 cm) ───────────────────
  name_pt <- max(9, round(14.0 * w_cm / 6.0, 0))
  auth_pt <- max(4, round( 7.0 * w_cm / 6.0, 0))   # half the name size
  fam_pt <- max(6, round(10.5 * w_cm / 6.0, 0))
  vern_pt <- max(4, round( 7.0 * w_cm / 6.0, 0))
  endem_pt <- max(4, round( 6.5 * w_cm / 6.0, 0))
  # id_pt already computed above (used to derive footer_top)

  # ── Vertical text spacing (baseline-to-baseline, in viewport NPC) ─────────
  # gap = (half of upper font + half of lower font) × 0.035 cm/pt + buffer
  gap_name_auth <- ((name_pt / 2 + auth_pt  / 2) * 0.03527 + 0.04) / h_cm
  gap_auth_fam <- ((auth_pt  / 2 + fam_pt   / 2) * 0.03527 + 0.20) / h_cm
  gap_fam_vern <- ((fam_pt   / 2 + vern_pt  / 2) * 0.03527 + 0.04) / h_cm
  # Endemism phrase: generous gap from the block above it (vernacular or family)
  gap_vern_endem <- ((vern_pt  / 2 + endem_pt / 2) * 0.03527 + 0.12) / h_cm
  gap_fam_endem <- ((fam_pt   / 2 + endem_pt / 2) * 0.03527 + 0.14) / h_cm

  # ── Y positions (top-down) ────────────────────────────────────────────────
  name_y <- header_top - 0.038
  auth_y <- name_y - gap_name_auth
  fam_y <- auth_y - gap_auth_fam
  vern_y <- fam_y  - gap_fam_vern
  # Endemism anchors to vernacular when present, otherwise directly to family
  endem_y <- if (nzchar(vern_clean)) vern_y - gap_vern_endem
             else fam_y - gap_fam_endem

  # Text block centred (logo at bottom — no longer affects header layout)
  txt_cx <- 0.5

  # ── Scientific name (italic, dark green) ──────────────────────────────────
  grid::grid.text(
    label = species_name,
    x = grid::unit(txt_cx, "npc"),
    y = name_y,
    gp = grid::gpar(fontface = "italic",
                       fontsize = name_pt,
                       col = "#1a3a1a",
                       fontfamily = font_family),
    just = "center"
  )

  # ── Authorship — half the size, snug directly below name ─────────────────
  auth_clean <- if (is.na(authorship) || !nzchar(authorship)) ""
                else as.character(authorship)
  if (nzchar(auth_clean)) {
    grid::grid.text(
      label = auth_clean,
      x = grid::unit(txt_cx, "npc"),
      y = auth_y,
      gp = grid::gpar(fontface = "plain",
                         fontsize = auth_pt,
                         col = "#8a8a8a",
                         fontfamily = "sans"),
      just = "center"
    )
  }

  # ── Family name (bold, medium green) — clearly separated from authorship ──
  fam_clean <- if (is.na(family_name) || !nzchar(family_name)) ""
               else toupper(as.character(family_name))
  if (nzchar(fam_clean)) {
    grid::grid.text(
      label = fam_clean,
      x = grid::unit(txt_cx, "npc"),
      y = fam_y,
      gp = grid::gpar(fontface = "bold",
                         fontsize = fam_pt,
                         col = "#2e5a2e",
                         fontfamily = "sans"),
      just = "center"
    )
  }

  # ── Vernacular / common name (italic, muted sage, first entry only) ────────
  if (nzchar(vern_clean)) {
    grid::grid.text(
      label = vern_clean,
      x = grid::unit(txt_cx, "npc"),
      y = vern_y,
      gp = grid::gpar(fontface = "italic",
                         fontsize = vern_pt,
                         col = "#7a9a7a",
                         fontfamily = "sans"),
      just = "center"
    )
  }

  # ── Endemism (language-aware, below vernacular or family if none) ──────────
  if (nzchar(endemic_text)) {
    grid::grid.text(
      label = endemic_text,
      x = grid::unit(txt_cx, "npc"),
      y = endem_y,
      gp = grid::gpar(fontface = "italic",
                         fontsize = endem_pt,
                         col = "#8a7040",
                         fontfamily = "sans"),
      just = "center"
    )
  }

  # ── QR code (rendered BEFORE separator so separator draws on top) ─────────
  # The QR raster top is placed exactly at sep_y. The raster's own quiet zone
  # (2 white modules) sits below the separator line, providing the minimum
  # required clear space. Because the separator is drawn afterwards it renders
  # on top of the white quiet zone, appearing to sit right at the QR edge.
  # When there is no logo/id_code the footer shrinks to 0.020 and the QR
  # expands to fill nearly the entire lower half of the label.
  qr_zone_h_cm <- h_cm * (sep_y - footer_top)
  qr_side_cm <- min(w_cm * 0.84, qr_zone_h_cm * 0.96)
  qr_top_y <- sep_y
  qr_zone_cy <- qr_top_y - qr_side_cm / (2 * h_cm)
  qr_bottom_y <- qr_zone_cy - qr_side_cm / (2 * h_cm)
  safe_url <- if (is.na(qr_url) || !nzchar(qr_url)) species_name else qr_url
  qr_mat <- qrcode::qr_code(safe_url, ecl = "M")
  qr_raster <- .qr_to_raster(qr_mat, qr_color, quiet_zone = 2L)

  grid::grid.raster(
    image = qr_raster,
    x = grid::unit(0.5, "npc"),
    y = grid::unit(qr_zone_cy, "npc"),
    width = grid::unit(qr_side_cm, "cm"),
    height = grid::unit(qr_side_cm, "cm"),
    interpolate = FALSE
  )

  # ── Decorative separator (drawn AFTER QR — renders on top of quiet zone) ──
  grid::grid.lines(
    x = grid::unit(c(pad, 1 - pad), "npc"),
    y = grid::unit(c(sep_y, sep_y), "npc"),
    gp = grid::gpar(col = "#9dbd9d", lwd = 0.55)
  )

  # ── Logo (bottom-right, anchored just below QR bottom) ────────────────────
  logo_bot_npc <- 0.028   # fallback vertical position when no logo
  if (has_logo) {
    logo_top_npc <- qr_bottom_y - 0.012
    logo_cy_npc <- logo_top_npc - logo_h_cm / (2 * h_cm)
    logo_cy_npc <- max(logo_cy_npc, logo_h_cm / (2 * h_cm) + 0.018)
    logo_bot_npc <- logo_cy_npc - logo_h_cm / (2 * h_cm)
    logo_cx_npc <- 1 - pad - (logo_w_cm / w_cm) / 2

    grid::grid.raster(
      logo_img,
      x = grid::unit(logo_cx_npc, "npc"),
      y = grid::unit(logo_cy_npc, "npc"),
      width = grid::unit(logo_w_cm, "cm"),
      height = grid::unit(logo_h_cm, "cm"),
      interpolate = TRUE
    )
  }

  # ── Specimen ID — aligned with logo bottom edge (or near label bottom) ────
  if (has_id) {
    grid::grid.text(
      label = id_clean,
      x = grid::unit(pad, "npc"),
      y = logo_bot_npc,   # same line as logo bottom when logo present
      gp = grid::gpar(fontface = "plain",
                         fontsize = id_pt,
                         col = "#c0c0c0",
                         fontfamily = "mono"),
      just = c("left", "center")
    )
  }

  grid::popViewport()
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# Convert a logical QR matrix to a colour raster, adding a white quiet zone.
.qr_to_raster <- function(qr_mat, dark_color = "#1a2e1a", quiet_zone = 4L) {
  n <- nrow(qr_mat)
  total <- n + 2L * quiet_zone
  full <- matrix("#FFFFFF", nrow = total, ncol = total)
  ri <- seq_len(n) + quiet_zone
  ci <- seq_len(n) + quiet_zone
  full[ri, ci] <- matrix(ifelse(qr_mat, dark_color, "#FFFFFF"),
                          nrow = n, ncol = n)
  grDevices::as.raster(full)
}

# Load a PNG or JPEG logo as a raster array for grid::grid.raster().
.load_logo_raster <- function(path_to_logo) {
  if (is.null(path_to_logo)) return(NULL)
  if (!file.exists(path_to_logo)) {
    warning("Logo file not found: ", path_to_logo, call. = FALSE)
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path_to_logo))
  tryCatch({
    if (ext == "png") {
      png::readPNG(path_to_logo)
    } else if (ext %in% c("jpg", "jpeg")) {
      if (!requireNamespace("jpeg", quietly = TRUE))
        stop("Package 'jpeg' is required for JPEG logos. ",
             "Run install.packages('jpeg') or convert to PNG.")
      jpeg::readJPEG(path_to_logo)
    } else {
      warning("Unsupported logo format '", ext, "'. Use PNG or JPEG.", call. = FALSE)
      NULL
    }
  }, error = function(e) {
    warning("Failed to load logo: ", e$message, call. = FALSE)
    NULL
  })
}
