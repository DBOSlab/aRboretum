# Generate editable multilingual species phrases

This function builds the natural-language species descriptions used by
the species labels and audio guides. It is meant to be run once the
species data retrieved with
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
have been reviewed. The phrases are generated from the data file and
stored in it, in the columns `full_phrases_EN`, `full_phrases_PT`,
`full_phrases_ES`, `full_phrases_FR`, and `full_phrases_ADD_LANGUAGE`,
which are created at this step only. The function also writes a
standalone HTML phrase guide (`__phrase_generating_guide.html`) next to
the data file, which allows users to review and edit the phrases and
annotation fields, and to export the updated data directly from the
browser.

## Usage

``` r
arboretum_phrases(
  data_path = NULL,
  printed_lang = c("pt", "en", "fr", "es"),
  add_lang = NULL,
  overwrite = FALSE,
  save = TRUE,
  verbose = TRUE
)
```

## Arguments

- data_path:

  Required. Path to the CSV or XLSX species data file created by
  [`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md).
  The file is updated in place with the phrase columns.

- printed_lang:

  Character vector. Built-in language(s) for which phrases are
  generated. Accepted values are `"pt"`, `"en"`, `"fr"`, and `"es"`.
  Default is `c("pt", "en", "fr", "es")`.

- add_lang:

  Character string or `NULL`. Optional code for one additional language,
  for example `"PANARA"` or `"TUKANO"`. When supplied, the HTML phrase
  guide includes a dedicated editable field for entering a full phrase
  translation for the specified custom language. The content entered
  there is stored in the `full_phrases_ADD_LANGUAGE` column and is used
  by
  [`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
  to include that language in the species labels. Default is `NULL`.

- overwrite:

  Logical. If `FALSE` (default), phrases already stored in the
  `full_phrases_*` columns are kept, so manual edits are never lost, and
  only empty cells are filled. If `TRUE`, all phrases of `printed_lang`
  are generated again from the data, for example after correcting the
  species data. `full_phrases_ADD_LANGUAGE` is never overwritten.

- save:

  Logical. If `TRUE` (default), the data file is updated with the phrase
  columns and the HTML phrase guide is written next to it.

- verbose:

  Logical. If `TRUE`, progress messages are printed to the console.
  Default is `TRUE`.

## Value

Invisibly, the species dataframe with the phrase columns:

- `full_phrases_EN`, `full_phrases_PT`, `full_phrases_ES`,
  `full_phrases_FR`: Natural-language species descriptions generated for
  each language in `printed_lang`. They can be manually edited in the
  data file or in the HTML phrase guide. Plant uses and free notes are
  appended after the phrase in the labels, so there is no need to repeat
  them here.

- `full_phrases_ADD_LANGUAGE`: Complete phrase in the custom language
  specified by `add_lang`, entered by the user via the HTML phrase guide
  or direct editing.

## Details

Phrases describe the taxonomy, vernacular names, distribution and
endemism, establishment means, introduced range, phytogeographic
domains, vegetation types, and IUCN Red List status of each species,
using the multilingual internal dictionary of the package.

Running `arboretum_phrases()` again only fills empty phrase cells,
unless `overwrite = TRUE`. Clearing a phrase in the data file or in the
HTML guide therefore makes it be generated again on the next run.

[`arboretum_audios()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)
and
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
only read the stored phrases and never generate them, so
`arboretum_phrases()` must be run before them.

## See also

[`arboretum_data`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md),
[`arboretum_audios`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md),
[`arboretum_labels`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)

## Author

Martin Boucknooghe & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Retrieve and review the species data
arboretum_data(
  spp_list = c("Euterpe edulis", "Paubrasilia echinata"),
  dir = "arboretum_data"
)

# 2. Generate the phrases once the data are fine
arboretum_phrases(
  data_path = "arboretum_data/arboretum_data.csv",
  printed_lang = c("pt", "en"),
  add_lang = "TUKANO"
)

# Generate all phrases again after correcting the data
arboretum_phrases(
  data_path = "arboretum_data/arboretum_data.csv",
  overwrite = TRUE
)
} # }
```
