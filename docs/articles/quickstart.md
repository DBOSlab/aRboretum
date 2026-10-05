# Quick start with aRboretum

## Introduction

This vignette provides a concise overview of the core `aRboretum`
workflow: retrieving and reviewing species data, generating the species
phrases, building interactive HTML labels, and creating a searchable
minisite for a living plant collection.

### Installation

`aRboretum` depends on two packages that are not on CRAN, so install
them first:

- [`rWCVPdata`](https://github.com/matildabrown/rWCVPdata), which
  provides the World Checklist of Vascular Plants data and is
  distributed through its own repository;
- [`floraR`](https://github.com/DBOSlab/floraR), which downloads and
  parses the Flora e Funga do Brasil dataset used by
  [`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md).

\
`if`` ``(``!`[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)`(``"rWCVPdata"``, quietly ``=`` ``TRUE``)``)`` ``{`\
`  `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"rWCVPdata"``,`\
`                   repos ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`                     ``"https://matildabrown.github.io/drat"``,`\
`                     ``"https://cloud.r-project.org"`\
`                   ``)`\
`  ``)`\
`}`\
\
`if`` ``(``!`[`requireNamespace`](https://rdrr.io/r/base/ns-load.html)`(``"BiocManager"``, quietly ``=`` ``TRUE``)``)`\
`  `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"BiocManager"``)`\
\
`# Install the development version of floraR from GitHub,`\
`# together with its required dependencies`\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"DBOSlab/floraR"``, dependencies ``=`` ``TRUE``)`

Then install the development version of `aRboretum` from
[GitHub](https://github.com/DBOSlab/aRboretum):

\
`# Install the development version of aRboretum from GitHub,`\
`# together with its required dependencies`\
`BiocManager``::`[`install`](https://bioconductor.github.io/BiocManager/reference/install.html)`(``"DBOSlab/aRboretum"``, dependencies ``=`` ``TRUE``)`

#### Optional: IUCN Red List API token

By default, IUCN Red List categories are retrieved from the IUCN Red
List checklist mirrored by [GBIF](https://www.gbif.org/), which requires
no registration. To query the official [IUCN Red List
API](https://api.iucnredlist.org/) instead, install the
[`rredlist`](https://docs.ropensci.org/rredlist/) package, request a
free API token, and store it in your `.Renviron` file:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"rredlist"``)`\
`usethis``::`[`edit_r_environ`](https://usethis.r-lib.org/reference/edit.html)`(``)``   ``# add the line: IUCN_REDLIST_KEY=your_token`

Then load it:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`aRboretum`](https://DBOSlab.github.io/aRboretum)`)`

## Step 1. Retrieve and review the species data

Start with a character vector containing the species names of interest.

The function
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
queries [Flora e Funga do Brasil
(FFB)](https://floradobrasil.jbrj.gov.br/) and the [World Checklist of
Vascular Plants (WCVP)](https://powo.science.kew.org/), resolves
accepted names and synonyms, adds the [IUCN Red
List](https://www.iucnredlist.org/) status of each species, and compiles
a structured dataset for downstream use in `aRboretum`.

Both `.csv` and `.xlsx` outputs are supported.

\
`species_list`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Luetzelburgia bahiensis"``, ``"Paubrasilia echinata"``)`\
\
[`arboretum_data`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)`(`\
`  spp_list ``=`` ``species_list``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``, ``"fr"``, ``"es"``)``,`\
`  save ``=`` ``TRUE``,`\
`  format ``=`` ``"xlsx"``,`\
`  dir ``=`` ``"arboretum_data"`\
`)`

This saves `arboretum_data/arboretum_data.xlsx` together with
`arboretum_data/__data_reviewing_guide.html`. Review the data before
going further, either in the spreadsheet or in the guide, which lets you
edit the retrieved fields (vernacular names, distribution, endemism in
Brazil, establishment means, IUCN status, and so on) and the optional
plant uses and notes in your browser, then save or export the corrected
file. `printed_lang` sets the languages of the guide.

The FFB dataset is downloaded once into a `flora_download/` folder and
reused by later runs, so they also work offline (see the `ffb_dir`
argument). Running
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
again on the same `dir` reads the existing data file instead of querying
the databases again.

## Step 2. Generate the species phrases

Once you agree with the data, generate the natural-language species
descriptions:

\
[`arboretum_phrases`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``, ``"fr"``, ``"es"``)`\
`)`

The phrases are stored in the data file, in the new columns
`full_phrases_EN`, `full_phrases_PT`, `full_phrases_ES`,
`full_phrases_FR`, and `full_phrases_ADD_LANGUAGE`. The function also
writes `arboretum_data/__phrase_generating_guide.html`, where you can
read and edit the phrases and export the updated data. Edited phrases
are used as they are by the labels and audio guides; running
[`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md)
again only fills empty phrases (use `overwrite = TRUE` to regenerate
them all).

## Optional step. Add personal photos and audio recordings

Personal photos and recordings must be in place before the labels are
generated.

\
`# One folder per species for photos`\
[`arboretum_photos`](https://DBOSlab.github.io/aRboretum/reference/arboretum_photos.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"`\
`)`\
\
`# One folder per species and language for recordings, plus a recording guide`\
[`arboretum_audios`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``, ``"fr"``, ``"es"``)`\
`)`

Add images to the `arboretum_photos/` folders and recordings to the
`arboretum_audios/` folders; the recording guide
`arboretum_audios/__personal_audio_recording_guide.html` shows the
phrase to read for each species and language. See the
[howto_photos](https://DBOSlab.github.io/aRboretum/articles/howto_photos.md)
and
[howto_audios](https://DBOSlab.github.io/aRboretum/articles/howto_audios.md)
articles for details.

## Step 3. Generate HTML labels

Use the data file, now completed with phrases, to create one interactive
HTML label per species. The labels only read the stored phrases and
never generate them, so every language in `printed_lang` must have been
included in
[`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md).

\
[`arboretum_labels`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``, ``"fr"``, ``"es"``)``,`\
`  dir ``=`` ``"arboretum_labels"`\
`)`

Each label can include:

- taxonomic information and authorship;
- species description in multiple languages;
- world and Brazil distribution maps;
- conservation status and source links;
- browser-based text-to-speech;
- personal audio recordings and photos when available.

## Step 4. Build a multilingual minisite

Create a searchable `index.html` page linking all generated species
labels.

\
[`arboretum_minisite`](https://DBOSlab.github.io/aRboretum/reference/arboretum_minisite.md)`(`\
`  labels_dir ``=`` ``"arboretum_labels"``,`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  site_title ``=`` ``"My Plant Collection"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``, ``"fr"``, ``"es"``)``,`\
`  group_by_family ``=`` ``TRUE`\
`)`

This writes `index.html` into the labels folder and then renames that
folder after the site title, here `My_Plant_Collection_minisite/`. Open
its `index.html` in your browser to explore the collection homepage. The
whole folder can be published as is.

## Optional step. Add one community or local language

In some projects, it is useful to include one additional language in the
species labels without translating the full website or minisite
interface. This can be especially relevant in collaborative work with
Indigenous peoples or other local communities.

For this purpose, `aRboretum` provides the `add_lang` argument in
[`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md),
[`arboretum_audios()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)
and
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md).

To use this workflow:

1.  Run
    [`arboretum_phrases()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md)
    with `add_lang`: it adds an editable field for the extra language to
    the phrase guide.
2.  Enter the complete label text for each species in the additional
    language, in the guide or directly in the
    `full_phrases_ADD_LANGUAGE` column of the data file.
3.  Use `add_lang` to include the extra language in the audio-folder
    workflow and in the final labels.

Example with Tukano:

\
[`arboretum_phrases`](https://DBOSlab.github.io/aRboretum/reference/arboretum_phrases.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``)``,`\
`  add_lang ``=`` ``"TUKANO"`\
`)`\
\
[`arboretum_audios`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``)``,`\
`  add_lang ``=`` ``"TUKANO"`\
`)`\
\
[`arboretum_labels`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  audio_dir ``=`` ``"arboretum_audios"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``)``,`\
`  add_lang ``=`` ``"TUKANO"``,`\
`  dir ``=`` ``"arboretum_labels"`\
`)`

In this workflow:

- Portuguese and English phrases come from the standard phrase
  generation;
- Tukano text is read directly from `full_phrases_ADD_LANGUAGE`;
- Tukano does not need to pass through the translation pipeline;
- personal recordings in the `TUKANO` folders are used when available.

See the
[add_lang](https://DBOSlab.github.io/aRboretum/articles/add_lang.md)
article for a complete example.

## What you get

After running the steps above, the minisite folder
(`My_Plant_Collection_minisite/` in this example) contains:

1.  One HTML file per species, for example
    `FABACEAE_Paubrasilia_echinata_label.html`.
2.  One `index.html` file for the minisite homepage.
3.  Supporting folders with maps, icons, and any personal photos and
    recordings.

Open the species label files in your browser to explore:

- language selection buttons;
- text and audio for each species;
- interactive maps;
- logos and source links;
- personal photos, when provided.

Open `index.html` to explore the collection homepage, including:

- search tools;
- grouped species listings;
- summary cards and dashboard charts built from the species data.

## Next steps

To continue customizing your project:

- add your own photos with the workflow described in the
  [howto_photos](https://DBOSlab.github.io/aRboretum/articles/howto_photos.md)
  article;
- add personal audio recordings with the workflow described in the
  [howto_audios](https://DBOSlab.github.io/aRboretum/articles/howto_audios.md)
  article;
- generate QR-code labels for printing with
  [`arboretum_qrcodes()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_qrcodes.md)
  (see the
  [aRboretum](https://DBOSlab.github.io/aRboretum/articles/aRboretum.md)
  article).
