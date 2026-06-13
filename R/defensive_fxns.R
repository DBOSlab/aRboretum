# Auxiliary defensive functions

# Checking the input spp list and making some cleaning ####
.arg_check_spp_list <- function(x) {
  x <- trimws(x)
  tf <- grepl("^[[:lower:]]" , x)
  if(any(tf)) {
    x[tf] <- .first_upper(x[tf])
  }

  tf <- !grepl("\\s", x)
  if (any(tf)) {
    stop("Please make sure you used only species names as input!\n",
         paste0("You may need to correct the folowing names:\n\n", paste0(x[tf], collapse = ", "))
    )
  }

  return(x)
}
.first_upper <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}


# Check if the dir input is "character" type and if it has a "/" in the end  ####
.arg_check_dir <- function(dir) {
  if (!is.character(dir) || length(dir) != 1L) stop("'dir' must be a single character string.", call. = FALSE)
  dir <- trimws(dir)
  if (!nzchar(dir)) stop("'dir' cannot be an empty string.", call. = FALSE)
  dir <- gsub("/$", "", dir)
  return(dir)
}

.arg_check_printed_lang <- function(printed_lang) {
  allowed_langs <- c("pt", "en", "fr", "es")

  if (is.null(printed_lang) || length(printed_lang) == 0) {
    stop("'printed_lang' must contain at least one language code.", call. = FALSE)
  }

  printed_lang <- unique(tolower(trimws(as.character(printed_lang))))
  invalid <- !printed_lang %in% allowed_langs
  if (any(invalid)) {
    stop("Invalid language code(s): ",
         paste(printed_lang[invalid], collapse = ", "),
         ". Allowed values: pt, en, fr, es", call. = FALSE)
  }
  return(printed_lang)
}

