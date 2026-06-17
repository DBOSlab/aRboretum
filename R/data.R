#' @name botregions
#'
#' @docType data
#'
#' @title Countries and associated classification of botanical divisions
#'
#' @description
#' Countries and associated classification of botanical divisions according to
#' the World Geographical Scheme for Recording Plant Distributions (WGSRPD).
#' This dataset provides the tabular look-up table with multilingual names for
#' all botanical regions recognized by the standard.
#'
#' @format A \code{data.frame} with the following columns:
#' \describe{
#'   \item{country}{Character. Botanical Country names (Level 3)}
#'   \item{botanical_division}{Character. Basic Recording Unit names (Level 4)}
#'   \item{continent}{Character. Continental level names (Level 1)}
#'   \item{country_EN, country_ES, country_FR, country_PT}{Character. Botanical
#'         Country names in English, Spanish, French, and Portuguese}
#'   \item{botanical_division_EN, botanical_division_ES, botanical_division_FR,
#'         botanical_division_PT}{Character. Basic Recording Unit names in
#'         English, Spanish, French, and Portuguese}
#'   \item{continent_EN, continent_ES, continent_FR, continent_PT}{Character.
#'         Continental names in English, Spanish, French, and Portuguese}
#' }
#'
#' @source
#' \url{https://www.tdwg.org/standards/wgsrpd/}
#'
#' Brummitt, R.K. (2001). World Geographic Scheme for Recording Plant Distributions,
#' Edition 2. Hunt Institute for Botanical Documentation, Carnegie Mellon University,
#' Pittsburgh.
#'
#' @usage data(botregions)
#'
#' @examples
#' \dontrun{
#' library(aRboretum)
#' data(botregions)
#' head(botregions)
#'
#' # Show botanical divisions in South America (Portuguese names)
#' subset(botregions, continent_EN == "South America",
#'        select = c(botanical_division_PT, country_PT))
#'
#' # Count botanical countries by continent
#' table(botregions$continent_EN)
#' }
#'
NULL

#' @name WGSRPD
#'
#' @docType data
#'
#' @title World Geographical Scheme for Recording Plant Distributions - Spatial Map
#'
#' @description
#' Spatial map data (polygon boundaries) for the World Geographical Scheme for
#' Recording Plant Distributions (WGSRPD). This dataset provides the geographic
#' boundaries for the standard set of botanical recording units used in plant
#' distribution studies.
#'
#' This dataset represents **Level 3** (Botanical Countries) of the WGSRPD
#' standard, which are "botanical countries" that may ignore purely political
#' considerations. Antarctica has been excluded, and country names for DR Congo
#' and Sudan-South Sudan have been corrected for consistency.
#'
#' @format A simple features (\code{sf}) object with polygon geometry and
#' the following attributes:
#' \describe{
#'   \item{LEVEL3_COD}{Character. Level 3 code (botanical country code)}
#'   \item{LEVEL3_NAM}{Character. Level 3 name (botanical country name)}
#'   \item{LEVEL1_COD}{Character. Level 1 code (continental code)}
#'   \item{LEVEL1_NAM}{Character. Level 1 name (continental name)}
#'   \item{geometry}{sfc polygon. The boundary polygons for each botanical country}
#' }
#'
#' @source
#' \url{https://www.tdwg.org/standards/wgsrpd/}
#'
#' The original shapefiles are maintained by the TDWG Geographical Schemes
#' Interest Group and were sourced from:
#' \url{https://github.com/tdwg/wgsrpd}
#'
#' The standard is published as:
#' Brummitt, R.K. (2001). World Geographic Scheme for Recording Plant Distributions,
#' Edition 2. Hunt Institute for Botanical Documentation, Carnegie Mellon University,
#' Pittsburgh.
#'
#' @seealso \code{\link{botregions}} for the multilingual tabular look-up table
#' of botanical divisions.
#'
#' @usage data(WGSRPD)
#'
#' @examples
#' \dontrun{
#' library(aRboretum)
#' library(sf)
#' data(WGSRPD)
#'
#' # Check coordinate reference system
#' st_crs(WGSRPD)
#'
#' # Plot all botanical countries
#' plot(WGSRPD["LEVEL3_NAM"], main = "WGSRPD Botanical Countries")
#'
#' # Subset to South America (Level 1 code "84")
#' south_america <- subset(WGSRPD, LEVEL1_COD == "84")
#' plot(south_america["LEVEL3_NAM"],
#'      main = "Botanical Countries of South America")
#'
#' # Count botanical countries by continent
#' table(WGSRPD$LEVEL1_NAM)
#'
#' # Merge with multilingual names from botregions
#' data(botregions)
#' merged <- merge(WGSRPD, botregions,
#'                 by.x = "LEVEL3_NAM",
#'                 by.y = "country_EN")
#' }
#'
NULL
