# Quick start with aRboretum

## Introduction

This vignette provides a concise overview of the core `aRboretum`
workflow: preparing species data, generating interactive HTML labels,
and building a searchable minisite for a living plant collection.

If you have not already installed the package from GitHub:

``` r
# Install from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("DBOSlab/aRboretum")
```

Then load it:

``` r
library(aRboretum)
```

## Step 1. Prepare species data

Start with a character vector containing the species names of interest.

The function
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
queries [Flora e Funga do Brasil
(FFB)](https://reflora.jbrj.gov.br/reflora/listaBrasil/) and [Plants of
the World Online (POWO)](https://powo.science.kew.org/), resolves
accepted names and synonyms, and compiles a structured dataset for
downstream use in `aRboretum`.

Both `.csv` and `.xlsx` outputs are supported.

``` r
species_list <- c("Luetzelburgia bahiensis", "Paubrasilia echinata")

arboretum_data(
  spp_list = species_list,
  save = TRUE,
  format = "xlsx",
  dir = "arboretum_data"
)
```

This saves a data file inside `arboretum_data/`, which can then be used
to generate labels, optional personal audio folders, QR codes, and the
minisite.

## Step 2. Generate HTML labels

Use the saved data file to create one interactive HTML label per
species.

``` r
arboretum_labels(
  data_path = "arboretum_data/arboretum_data.xlsx",
  printed_lang = c("pt", "en", "fr", "es"),
  dir = "arboretum_labels"
)
```

Each label can include:

- taxonomic information and authorship;
- species description in multiple languages;
- world and Brazil distribution maps;
- conservation status and source links;
- browser-based text-to-speech;
- optional personal audio recordings and photos when available.

## Step 3. Build a multilingual minisite

Create a searchable `index.html` page linking all generated species
labels.

``` r
arboretum_minisite(
  labels_dir = "arboretum_labels",
  data_path = "arboretum_data/arboretum_data.xlsx",
  site_title = "My Plant Collection",
  group_by_family = TRUE
)
```

This creates a minisite homepage inside `arboretum_labels/`. You may
want to open it in your browser by just clicking on the `index.html`
file.

## Optional step. Add personal audio recordings

If you want to provide recorded audio instead of relying only on browser
text-to-speech, first create the folder structure for recordings:

``` r
arboretum_audios(
  data_path = "arboretum_data/arboretum_data.xlsx",
  printed_lang = c("pt", "en", "fr", "es")
)
```

This creates an `arboretum_audios/` directory and a recording guide
named:

`arboretum_audios/__personal_audio_recording_guide.html`

If audio files are added to the expected folders,
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
will automatically use them.

## Optional step. Add one community or local language

In some projects, it is useful to include one additional language in the
species labels without translating the full website or minisite
interface. This can be especially relevant in collaborative work with
Indigenous peoples or other local communities.

For this purpose, `aRboretum` provides the `add_lang` argument in both
[`arboretum_audios()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)
and
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md).

To use this workflow:

1.  Add a column named `full_phrases_ADD_LANGUAGE` to your input
    dataset.
2.  Fill this column with the complete label text for each species in
    the additional language.
3.  Use `add_lang` to include the extra language in the audio-folder
    workflow and in the final labels.

Example with Tukano:

``` r
arboretum_audios(
  data_path = "arboretum_data/arboretum_data.xlsx",
  printed_lang = c("pt", "en"),
  add_lang = "TUKANO"
)

arboretum_labels(
  data_path = "arboretum_data/arboretum_data.xlsx",
  audio_dir = "arboretum_audios",
  printed_lang = c("pt", "en"),
  add_lang = "TUKANO",
  dir = "arboretum_labels"
)
```

In this workflow:

- Portuguese and English are handled through the standard package
  workflow;
- Panará text is read directly from `full_phrases_ADD_LANGUAGE`;
- Panará does not need to pass through the full translation pipeline;
- personal recordings in the `TUKANO` folders are used when available.

After running the steps above, the `arboretum_labels/` folder will
typically contain:

1.  One HTML file per species, for example:

`FABACEAE_Paubrasilia_echinata_label.html`

2.  One `index.html` file for the minisite homepage.

Open the species label files in your browser to explore:

- language selection buttons;
- text and audio for each species;
- interactive maps;
- logos and source links;
- optional personal photos.

Open `index.html` to explore the collection homepage, including:

- search tools;
- grouped species listings;
- summary cards and dashboard elements when data are available.

## Next steps

To continue customizing your project:

- add your own photos with the workflow described in the
  [howto_photos](https://dboslab.github.io/InNOutBT/articles/howto_photos.html)
  vignette;
- add personal audio recordings with the workflow described in the
  [howto_audios](https://dboslab.github.io/InNOutBT/articles/howto_audios.html)
  vignette;
- generate QR-code labels for printing with
  [`arboretum_qrcodes()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_qrcodes.md).
