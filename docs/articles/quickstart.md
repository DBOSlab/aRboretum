# Quick start with aRboretum

## Introduction

This vignette provides a concise overview of the core `aRboretum`
workflow: preparing species data, generating interactive HTML labels,
and building a searchable minisite for a living plant collection.

If you have not already installed the package from GitHub:

\
`# Install from GitHub`\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"devtools"`](https://devtools.r-lib.org/)`)``)`` `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"devtools"``)`\
`devtools``::`[`install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"DBOSlab/aRboretum"``)`

Then load it:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`aRboretum`](https://DBOSlab.github.io/aRboretum)`)`

## Step 1. Prepare species data

Start with a character vector containing the species names of interest.

The function
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
queries [Flora e Funga do Brasil
(FFB)](https://reflora.jbrj.gov.br/reflora/listaBrasil/) and the [World
Checklist of Vascular Plants (WCVP)](https://powo.science.kew.org/),
adds the IUCN Red List status, resolves accepted names and synonyms, and
compiles a structured dataset for downstream use in `aRboretum`.

Both `.csv` and `.xlsx` outputs are supported.

\
`species_list`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Luetzelburgia bahiensis"``, ``"Paubrasilia echinata"``)`\
\
[`arboretum_data`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)`(`\
`  spp_list ``=`` ``species_list``,`\
`  save ``=`` ``TRUE``,`\
`  format ``=`` ``"xlsx"``,`\
`  dir ``=`` ``"arboretum_data"`\
`)`

This saves a data file inside `arboretum_data/`, together with
`arboretum_data/__data_reviewing_guide.html`. Review the data before
going further, either in the spreadsheet or in that guide, which lets
you edit the retrieved fields, plant uses and notes in your browser and
save or export the corrected file. The `printed_lang` argument sets the
languages of the guide.

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

## Step 3. Generate HTML labels

Use the data file, now completed with phrases, to create one interactive
HTML label per species. The labels only read the stored phrases and
never generate them.

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
- optional personal audio recordings and photos when available.

## Step 4. Build a multilingual minisite

Create a searchable `index.html` page linking all generated species
labels.

\
[`arboretum_minisite`](https://DBOSlab.github.io/aRboretum/reference/arboretum_minisite.md)`(`\
`  labels_dir ``=`` ``"arboretum_labels"``,`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  site_title ``=`` ``"My Plant Collection"``,`\
`  group_by_family ``=`` ``TRUE`\
`)`

This creates a minisite homepage inside `arboretum_labels/`. You may
want to open it in your browser by just clicking on the `index.html`
file.

## Optional step. Add personal audio recordings

If you want to provide recorded audio instead of relying only on browser
text-to-speech, first create the folder structure for recordings:

\
[`arboretum_audios`](https://DBOSlab.github.io/aRboretum/reference/arboretum_audios.md)`(`\
`  data_path ``=`` ``"arboretum_data/arboretum_data.xlsx"``,`\
`  printed_lang ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"pt"``, ``"en"``, ``"fr"``, ``"es"``)`\
`)`

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
