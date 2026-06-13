# Community-language accessibility

## Overview

In some projects, a full translation of the website or minisite
interface is not necessary, but it is still important to make species
information accessible in a community language. This can be especially
relevant when co-developing interpretive materials with Indigenous
peoples.

For this purpose, `aRboretum` provides the `add_lang` argument in both
[`arboretum_audios()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)
and
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md).
This argument supports one additional language as a flexible
accessibility layer, without requiring the full multilingual translation
workflow used for the built-in package languages.

In this example, we use **Tukano** as an additional language.

## How `add_lang` works

When `add_lang` is supplied:

- [`arboretum_audios()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)
  creates one extra folder per species for optional personal recordings
  in the additional language.
- [`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
  adds one extra text option to the generated label when the dataset
  contains non-empty values in the column `full_phrases_ADD_LANGUAGE`.
- The extra language text is read directly from
  `full_phrases_ADD_LANGUAGE`.
- The extra language does **not** pass through the standard
  phrase-generation and translation workflow.

This is particularly useful when the website interface can remain in
Portuguese, English, French, or Spanish, while species-level content is
also made available in a community language.

## Example workflow

### Step 1. Prepare the dataset

Start from a dataset created with
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
or from your own formatted input file. To include Tukano, add a column
named `full_phrases_ADD_LANGUAGE`. This column should contain the
complete label text you want to display in the extra language.

``` r
tukano_example <- data.frame(
  taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
  family = c("Fabaceae", "Arecaceae"),
  full_phrases_ADD_LANGUAGE = c(
    "Texto completo em Tukano para Paubrasilia echinata.",
    "Texto completo em Tukano para Euterpe edulis."
  ),
  stringsAsFactors = FALSE
)

tukano_example
#>              taxonName    family
#> 1 Paubrasilia echinata  Fabaceae
#> 2       Euterpe edulis Arecaceae
#>                             full_phrases_ADD_LANGUAGE
#> 1 Texto completo em Tukano para Paubrasilia echinata.
#> 2       Texto completo em Tukano para Euterpe edulis.
```

In practice, your file will also include the standard columns used by
`aRboretum`, such as taxonomy, native distribution, uses, and notes.

### Minimal column schema

For the `add_lang = "TUKANO"` workflow, the most important requirement
is the presence of the column `full_phrases_ADD_LANGUAGE`.

A minimal input file should include at least the following columns:

| Column name | Required | Purpose |
|:---|:--:|:---|
| `taxonName` | Yes | Scientific name used to identify the species |
| `family` | Yes | Family name used in file naming and label generation |
| `full_phrases_ADD_LANGUAGE` | Yes, for `add_lang` labels | Full label text to display in the extra language |
| `plant_uses_PT`, `plant_uses_EN`, etc. | No | Language-specific uses text for built-in languages |
| `free_notes_PT`, `free_notes_EN`, etc. | No | Language-specific notes text for built-in languages |
| Other standard [`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md) fields | Usually yes | Taxonomy, native distribution, status, and other content used in the label |

A more realistic toy example may also include built-in language note
fields:

``` r
tukano_example_extended <- data.frame(
  taxonName = c("Paubrasilia echinata", "Euterpe edulis"),
  family = c("Fabaceae", "Arecaceae"),
  plant_uses_PT = c("Madeira e uso ornamental.", "Alimentação e paisagismo."),
  free_notes_PT = c(
    "Espécie simbólica no Brasil.",
    "Espécie importante da Mata Atlântica."
  ),
  full_phrases_ADD_LANGUAGE = c(
    "Texto completo em Tukano para Paubrasilia echinata.",
    "Texto completo em Tukano para Euterpe edulis."
  ),
  stringsAsFactors = FALSE
)

tukano_example_extended
#>              taxonName    family             plant_uses_PT
#> 1 Paubrasilia echinata  Fabaceae Madeira e uso ornamental.
#> 2       Euterpe edulis Arecaceae Alimentação e paisagismo.
#>                           free_notes_PT
#> 1          Espécie simbólica no Brasil.
#> 2 Espécie importante da Mata Atlântica.
#>                             full_phrases_ADD_LANGUAGE
#> 1 Texto completo em Tukano para Paubrasilia echinata.
#> 2       Texto completo em Tukano para Euterpe edulis.
```

### Step 2. Create folders for optional personal recordings

Use
[`arboretum_audios()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)
with `add_lang = "TUKANO"`:

``` r
library(aRboretum)

arboretum_audios(
  data_path = "extracted_data/my_species_data.xlsx",
  printed_lang = c("pt", "en"),
  add_lang = "TUKANO",
  verbose = TRUE
)
```

This creates the `arboretum_audios/` folder and one additional recording
folder per species using the `TUKANO` code.

A simplified folder structure looks like this:

``` text
arboretum_audios/
├── FABACEAE_Paubrasilia_echinata_PT/
├── FABACEAE_Paubrasilia_echinata_EN/
├── FABACEAE_Paubrasilia_echinata_TUKANO/
├── ARECACEAE_Euterpe_edulis_PT/
├── ARECACEAE_Euterpe_edulis_EN/
├── ARECACEAE_Euterpe_edulis_TUKANO/
└── __personal_audio_recording_guide.html
```

The HTML guide can be used to help organize custom recordings for each
species and language.

### Step 3. Add Tukano recordings if available

You may optionally place one personal recording file inside each
`*_TUKANO/` folder.

When a personal recording is available, it is used in the final label.
When it is not available, the HTML label may fall back to browser
text-to-speech. For additional community languages, browser speech
synthesis may not work reliably unless the device has a compatible voice
installed. Personal recordings are therefore recommended whenever
possible.

### Step 4. Generate HTML labels with the extra language included

Now generate the labels with
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md):

``` r
arboretum_labels(
  data_path = "extracted_data/my_species_data.xlsx",
  audio_dir = "arboretum_audios",
  printed_lang = c("pt", "en"),
  add_lang = "TUKANO",
  verbose = TRUE,
  dir = "html_species_labels"
)
```

When `add_lang` is provided and `full_phrases_ADD_LANGUAGE` contains
non-empty text, the generated HTML labels include **Tukano** as an
additional language option.

In this workflow:

- Portuguese and English are handled through the standard package
  workflow.
- Tukano text is read directly from `full_phrases_ADD_LANGUAGE`.
- Tukano audio is used when a personal recording is present.
- The rest of the website or minisite interface can remain in the
  built-in package languages.

## Why this workflow matters

This approach provides a practical intermediate solution for
multilingual accessibility. It allows species-level information to be
shared in a community language even when a full translation of the
broader interface is not needed or not yet available.

This can be especially useful in collaborative projects where local
knowledge holders or community members contribute curated text and
recorded audio directly.

## Practical notes

- `full_phrases_ADD_LANGUAGE` should contain the **complete final text**
  to be shown in the extra language.
- The extra language text is not automatically translated by
  `aRboretum`.
- If this column is empty for a species, that species will not have
  meaningful custom-language text to display.
- For custom languages, personal audio recordings are strongly
  recommended.
