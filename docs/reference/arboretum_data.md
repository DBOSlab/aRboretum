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
phrase guide (`__phrase_generating_guide.html`) to `dir`, which allows
users to review, edit annotation fields, and export the updated data
directly from the browser.

## Usage

``` r
arboretum_data(
  spp_list = NULL,
  printed_lang = c("pt", "en", "fr", "es"),
  add_lang = NULL,
  verbose = TRUE,
  save = TRUE,
  format = c("csv", "xlsx"),
  filename = "arboretum_data",
  dir = "arboretum_data"
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

  Character vector. Built-in language(s) for which phrases are generated
  in the HTML guide. Accepted values are `"pt"`, `"en"`, `"fr"`, and
  `"es"`. Default is `c("pt", "en", "fr", "es")`.

- add_lang:

  Character string or `NULL`. Optional code for one additional language,
  for example `"PANARA"` or `"TUKANO"`. When supplied, the HTML phrase
  guide includes a dedicated editable field for entering a full phrase
  translation for the specified custom language. The content entered
  there is stored in the `full_phrases_ADD_LANGUAGE` column and can be
  used on subsequent runs of
  [`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
  to include that language in the species labels. Default is `NULL`.

- verbose:

  Logical. If `TRUE`, progress messages are printed to the console.
  Default is `TRUE`.

- save:

  Logical. If `TRUE`, the resulting dataframe is saved to disk. Default
  is `TRUE`.

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

## Value

A dataframe combining data retrieved from FFB and POWO. The returned
columns include:

- `original_query`: The species name as originally supplied in
  `spp_list`.

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
  for user annotation via the HTML phrase guide or direct editing of the
  saved file.

- `free_notes_EN`, `free_notes_PT`, `free_notes_ES`, `free_notes_FR`:
  Free-text note fields in English, Portuguese, Spanish, and French,
  intended for user annotation via the HTML phrase guide or direct
  editing of the saved file.

- `IUCN.status`: Global IUCN Red List category, for example
  `"Endangered (EN)"`, or `NA` when the species has not been assessed.

- `full_phrases_EN`, `full_phrases_PT`, `full_phrases_ES`,
  `full_phrases_FR`: Natural-language species descriptions automatically
  generated for each language in `printed_lang`. Phrases stored in these
  columns are reused as they are on later runs, so they can be manually
  edited in the saved file or in the HTML phrase guide; clearing a cell
  makes the phrase be generated again. Plant uses and free notes are
  appended after the phrase.

- `full_phrases_ADD_LANGUAGE`: Reserved field for a complete phrase
  translation in the custom language specified by `add_lang`. Always
  present in the output; populated by the user via the HTML phrase guide
  or direct editing.

- `POWO.url`: URL to the species page in POWO.

- `FFB.url`: URL to the species page in FFB.

If a species is found in only one database, fields from the missing
database are returned as `NA`. Species not found in either database are
omitted from the final dataframe. For overlapping fields, FFB data
generally take precedence, except for selected WCVP-derived fields such
as botanical country, introduced range, and POWO URL.

When `save = TRUE`, the function also writes a standalone HTML phrase
guide (`__phrase_generating_guide.html`) to `dir`. This file embeds the
full dataframe and allows users to review generated phrases, edit
annotation fields (`FFB.vernacularName`, `plant_uses_*`, `free_notes_*`,
`full_phrases_*`), and export the updated data as CSV or XLSX without
rerunning the function.

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

6.  **HTML phrase guide generation**

    - Generates natural-language species descriptions for each
      `printed_lang` and stores them in the `full_phrases_*` columns.
      Phrases already present in these columns are kept, so manual edits
      are never overwritten. If `add_lang` is supplied, a dedicated
      editable field is included for the custom language.

    - Saves a standalone `__phrase_generating_guide.html` to `dir` when
      `save = TRUE`. The guide embeds the full dataframe and provides
      browser-based editing and export functionality.

Before processing, `spp_list` is cleaned using internal helper
functions. Leading and trailing whitespace are removed, names are
standardized, and each element is checked to ensure that it contains a
space. The function stops with an error if any element appears not to be
a binomial species name.

If `dir` already contains a CSV or XLSX data file, the species data are
read from that file instead of being retrieved again, and only empty
`full_phrases_*` cells are filled; the file is rewritten only when new
phrases were generated.

If `save = TRUE`, the function creates the output directory if needed
and saves the resulting dataframe either as a CSV file using
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html) or as
an Excel file using
[`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html).
When `verbose = TRUE`, a message reports the saved file path.

The temporary FFB download folder named `"flora_download"` is removed
when the function exits.

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
