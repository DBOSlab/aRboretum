# Extract and compile flora data from multiple taxonomic databases

This function queries both [Flora e Funga do Brasil
(FFB)](https://floradobrasil.jbrj.gov.br/consulta/) and [Plants of the
World Online (POWO)](https://powo.science.kew.org/) to retrieve
taxonomic, distributional, vernacular, conservation, and
occurrence-related information for a given list of plant species. It
standardizes input names, handles synonyms, merges data from both
sources, and returns a single dataframe that can optionally be saved as
a CSV or Excel file. The output also includes genus-level richness and
rank information derived from FFB.

## Usage

``` r
arboretum_data(
  spp_list = NULL,
  verbose = TRUE,
  save = TRUE,
  format = c("csv", "xlsx"),
  filename = "arboretum_mined_data",
  dir = "arboretum_mined_data"
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
  Default is `"arboretum_mined_data"`.

- dir:

  Character string. Directory path where the output file will be saved.
  Trailing slashes are automatically removed. The directory is created
  if it does not exist. Default is `"arboretum_mined_data"`.

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

- `IUCN.status`: IUCN conservation status retrieved from POWO, when
  available.

- `FFB.genusRichness`: Number of accepted species of the genus recorded
  in FFB.

- `FFB.genusRank`: Rank of the genus by species richness in FFB, with 1
  representing the richest genus.

- `plant_uses_EN`, `plant_uses_PT`, `plant_uses_ES`, `plant_uses_FR`:
  Plant-use fields in English, Portuguese, Spanish, and French, reserved
  for downstream annotation or future use.

- `free_notes_EN`, `free_notes_PT`, `free_notes_ES`, `free_notes_FR`:
  Free-text note fields in English, Portuguese, Spanish, and French,
  reserved for downstream annotation or future use.

- `POWO.url`: URL to the species page in POWO.

- `FFB.url`: URL to the species page in FFB.

If a species is found in only one database, fields from the missing
database are returned as `NA`. Species not found in either database are
omitted from the final dataframe. For overlapping fields, FFB data
generally take precedence, except for selected POWO-derived fields such
as botanical country, introduced range, IUCN status, and POWO URL.

## Details

The function follows four main steps:

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

2.  **POWO data extraction**

    - Searches POWO using
      [`taxize::pow_search()`](https://docs.ropensci.org/taxize/reference/pow_search.html).

    - Resolves synonyms to accepted names when possible.

    - Retrieves detailed information using
      [`taxize::pow_lookup()`](https://docs.ropensci.org/taxize/reference/pow_lookup.html),
      including taxonomy, distribution, introduced range, IUCN status,
      and POWO URL.

    - Converts POWO botanical countries to standard country names using
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

Before processing, `spp_list` is cleaned using internal helper
functions. Leading and trailing whitespace are removed, names are
standardized, and each element is checked to ensure that it contains a
space. The function stops with an error if any element appears not to be
a binomial species name.

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

- The taxize package is required to query POWO.

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
[`pow_search`](https://docs.ropensci.org/taxize/reference/pow_search.html),
[`pow_lookup`](https://docs.ropensci.org/taxize/reference/pow_lookup.html)

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
