# Extract and compile flora data from multiple taxonomic databases

This function queries both [Flora e Funga do Brasil
(FFB)](https://floradobrasil.jbrj.gov.br/consulta/) and the [World
Checklist of Vascular Plants (WCVP)](https://powo.science.kew.org/), the
backbone of Plants of the World Online (POWO), to retrieve taxonomic,
distributional, vernacular, and occurrence-related information for a
given list of plant species. The global conservation status of each
species is retrieved from the [IUCN Red
List](https://www.iucnredlist.org/). It standardizes input names,
handles synonyms, merges data from both sources, and returns a single
dataframe that can optionally be saved as a CSV or Excel file. The
output also includes genus-level richness and rank information derived
from FFB. When `save = TRUE`, the function also writes a standalone HTML
data reviewing guide (`__data_reviewing_guide.html`) to `dir`, in which
the retrieved data can be reviewed, corrected, and exported back to the
data file directly from the browser. Once the data have been reviewed,
the species phrases used by the labels are generated with
[`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md).

## Usage

``` r
arboretum_data(
  spp_list = NULL,
  printed_lang = c("pt", "en", "fr", "es"),
  verbose = TRUE,
  save = TRUE,
  format = c("csv", "xlsx"),
  filename = "arboretum_data",
  dir = "arboretum_data",
  ffb_dir = "flora_download"
)
```

## Arguments

- spp_list:

  Required. A character vector of species names, for example
  `c("Euterpe edulis", "Coffea arabica")`. Names should be binomials
  without authorship. Leading and trailing whitespace are removed, and
  names are standardized internally before querying. An error is thrown
  if any element does not contain a space, indicating a probable
  non-species name.

- printed_lang:

  Character vector. Language(s) of the HTML data reviewing guide: the
  guide interface can be switched between them, and the plant-use and
  free-note fields of these languages (`plant_uses_*`, `free_notes_*`)
  are shown for editing. Accepted values are `"pt"`, `"en"`, `"fr"`, and
  `"es"`. The data file always includes the plant-use and free-note
  columns of all four languages. Default is `c("pt", "en", "fr", "es")`.

- verbose:

  Logical. If `TRUE`, progress messages are printed to the console.
  Default is `TRUE`.

- save:

  Logical. If `TRUE`, the resulting dataframe is saved to disk and the
  HTML data reviewing guide is written to `dir`. Default is `TRUE`.

- format:

  Character string indicating the output file format. One of `"csv"` or
  `"xlsx"`. Partial matching is allowed through
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html). Default is
  `"csv"`.

- filename:

  Character string. Base name for the output file, without extension.
  Default is `"arboretum_data"`.

- dir:

  Character string. Directory path where the output file will be saved.
  Trailing slashes are automatically removed. The directory is created
  if it does not exist. Default is `"arboretum_data"`.

- ffb_dir:

  Character string. Path to the folder where Flora e Funga do Brasil
  (FFB) Darwin Core Archives are downloaded and kept between runs, as in
  [`floraR::flora_download()`](https://rdrr.io/pkg/floraR/man/flora_download.html).
  Two kinds of paths are accepted:

  - A folder holding one or more downloaded versions (for example
    `"flora_download"`). The most recent version already downloaded
    there is always used, without contacting the FFB server, so the
    function also works offline. The latest version is only downloaded,
    with
    [`floraR::flora_download()`](https://rdrr.io/pkg/floraR/man/flora_download.html),
    when the folder holds no downloaded version yet. To update a
    previously downloaded dataset, run
    `floraR::flora_download(dir = ffb_dir)`.

  - A single version folder (for example
    `"flora_download/dwca_ffb_v393_430_latest"`), which is used as is,
    without any download.

  Default is `"flora_download"`.

## Value

A dataframe combining data retrieved from FFB and POWO. The returned
columns include:

- `family`: Family name of the accepted taxon.

- `genus`: Genus extracted from the accepted scientific name.

- `taxonName`: Accepted scientific name after synonym resolution.

- `scientificNameAuthorship`: Authorship string.

- `FFB.vernacularName`: Vernacular names from FFB, with multiple values
  concatenated by `" | "`.

- `country`: Country names from FFB or derived from POWO botanical
  countries.

- `endemism`: Endemism status in Brazil from FFB, or inferred from POWO
  country-level distribution when FFB data are unavailable.

- `botanical_country`: Native distribution based on POWO botanical
  countries.

- `introduced_to`: Countries or botanical regions where the species is
  introduced according to POWO.

- `FFB.establishmentMeans`: Establishment means from FFB, such as
  `"Native"`, `"Cultivated"`, or `"Naturalized"`.

- `FFB.stateProvince`: Brazilian states from FFB, concatenated by
  `" | "`.

- `FFB.phytogeographicDomain`: Brazilian phytogeographic domains from
  FFB, translated into English when applicable and concatenated by
  `" | "`.

- `FFB.vegetationType`: Vegetation types from FFB, concatenated by
  `" | "`.

- `FFB.genusRichness`: Number of accepted species of the genus recorded
  in FFB.

- `FFB.genusRank`: Rank of the genus by species richness in FFB, with 1
  representing the richest genus.

- `plant_uses_EN`, `plant_uses_PT`, `plant_uses_ES`, `plant_uses_FR`:
  Plant-use fields in English, Portuguese, Spanish, and French, intended
  for user annotation by direct editing of the saved file or via the
  HTML phrase guide of
  [`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md).

- `free_notes_EN`, `free_notes_PT`, `free_notes_ES`, `free_notes_FR`:
  Free-text note fields in English, Portuguese, Spanish, and French,
  intended for user annotation by direct editing of the saved file or
  via the HTML phrase guide of
  [`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md).

- `IUCN.status`: Global IUCN Red List category, for example
  `"Endangered (EN)"`, or `NA` when the species has not been assessed.

- `POWO.url`: URL to the species page in POWO.

- `FFB.url`: URL to the species page in FFB.

If a species is found in only one database, fields from the missing
database are returned as `NA`. Species not found in either database are
omitted from the final dataframe. For overlapping fields, FFB data
generally take precedence, except for selected WCVP-derived fields such
as botanical country, introduced range, and POWO URL.

The phrase columns (`full_phrases_*`) are not part of this output: they
are added by
[`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md)
once the data have been reviewed.

When `save = TRUE`, the function also writes a standalone HTML data
reviewing guide (`__data_reviewing_guide.html`) to `dir`. It embeds the
data and provides, for each species, editable fields for
`FFB.vernacularName`, `country`, `endemism`, `botanical_country`,
`introduced_to`, `FFB.establishmentMeans`, `FFB.stateProvince`,
`FFB.phytogeographicDomain`, `FFB.vegetationType`, `IUCN.status`, and
the `plant_uses_*` and `free_notes_*` fields of `printed_lang`. The
`endemism`, `FFB.establishmentMeans`, and `IUCN.status` fields are
drop-down menus restricted to the values used to build the phrases. The
edited data can be saved or exported as CSV or XLSX without rerunning
the function.

## Details

The function follows five main steps:

1.  **Flora e Funga do Brasil data extraction**

    - Downloads the latest FFB Darwin Core Archive using
      [`floraR::flora_download()`](https://rdrr.io/pkg/floraR/man/flora_download.html).

    - Parses the archive with
      [`floraR::flora_parse()`](https://rdrr.io/pkg/floraR/man/flora_parse.html).

    - Extracts and processes the taxon, distribution, vernacular name,
      and species profile tables.

    - Matches each queried species name against the FFB taxon table.

    - Resolves synonyms to their accepted names when possible.

    - Retrieves family, accepted name, authorship, vernacular names,
      distribution, endemism, establishment means, Brazilian states,
      phytogeographic domains, vegetation types, and FFB reference URLs.

2.  **WCVP data extraction**

    - Loads the WCVP names and distribution tables once from rWCVPdata,
      so no per-species web requests are needed.

    - Resolves synonyms to accepted names when possible.

    - Retrieves family, authorship, native distribution (botanical
      countries), introduced range, and the POWO URL.

    - Converts WCVP botanical countries to standard country names using
      an internal helper function.

3.  **Data merging**

    - Combines FFB and POWO results into a single dataframe.

    - Prioritizes FFB data for overlapping fields when available.

    - Complements FFB records with POWO botanical countries, introduced
      range, conservation status, and POWO URLs.

    - Infers endemism from POWO country-level distribution when FFB
      endemism data are unavailable.

4.  **Genus-level summaries**

    - Extracts the genus from each accepted taxon name.

    - Calculates the number of accepted species per genus in FFB.

    - Adds genus richness and genus rank to the final dataframe.

    - Adds multilingual genus curiosity notes using internal helper
      functions, when available.

5.  **IUCN Red List status**

    - If the rredlist package is installed and an IUCN Red List API
      token is stored in the `IUCN_REDLIST_KEY` environment variable (a
      free token can be requested at <https://api.iucnredlist.org>), the
      latest global assessment is retrieved from the official IUCN Red
      List API.

    - Otherwise, the status is retrieved without a token from the IUCN
      Red List checklist mirrored by GBIF (<https://www.gbif.org>).

Before processing, `spp_list` is cleaned using internal helper
functions. Leading and trailing whitespace are removed, names are
standardized, and each element is checked to ensure that it contains a
space. The function stops with an error if any element appears not to be
a binomial species name.

If `dir` already contains a CSV or XLSX data file, the species data are
read from that file instead of being retrieved again, and the data
reviewing guide is rebuilt from it. Delete or move that file to retrieve
the data again.

If `save = TRUE`, the function creates the output directory if needed
and saves the resulting dataframe either as a CSV file using
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html) or as
an Excel file using
[`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html).
When `verbose = TRUE`, a message reports the saved file path.

The downloaded FFB dataset is kept in `ffb_dir`, so later runs reuse it
instead of downloading it again.

## Note

- The floraR package is required to download and parse the FFB Darwin
  Core Archive.

- The rWCVPdata package is required to access WCVP data.

- The rredlist package is optional and only used when an IUCN Red List
  API token is available.

- The openxlsx package is required only when `format = "xlsx"`.

- The function queries both FFB and POWO; there is currently no argument
  to select only one database.

- An internet connection is required.

- The initial FFB Darwin Core Archive download can be large and may take
  some time depending on the connection.

- POWO and FFB data are dynamic external resources, so results may
  change across database versions or query dates.

## See also

[`arboretum_phrases`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md),
[`flora_download`](https://rdrr.io/pkg/floraR/man/flora_download.html),
[`flora_parse`](https://rdrr.io/pkg/floraR/man/flora_parse.html),
[`rl_species`](https://docs.ropensci.org/rredlist/reference/rl_species.html)

## Author

Martin Boucknooghe & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
# Single species, without saving the result
result <- arboretum_data(
  spp_list = "Luetzelburgia bahiensis",
  save = FALSE
)

# Multiple species, saving the result as an Excel file
spp <- c("Cybianthus collinus",
         "Paubrasilia echinata",
         "Luetzelburgia bahiensis")

result <- arboretum_data(
  spp_list = spp,
  save = TRUE,
  format = "xlsx",
  filename = "my_plant_data",
  dir = "results"
)

# Suppress progress messages
result <- arboretum_data(
  spp_list = c("Euterpe edulis", "Coffea arabica"),
  verbose = FALSE
)
} # }
```
