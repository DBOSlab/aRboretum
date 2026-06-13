# How to add photos with aRboretum

## Introduction

The `aRboretum` package allows you to enrich your HTML species labels
with personal photos, such as herbarium specimens, field photographs,
botanical garden shots, or other curated images. This vignette shows how
to:

- create the required folder structure using
  [`arboretum_photos()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_photos.md);
- place image files in the correct species folders; and
- generate labels that automatically include a photo slideshow.

### Prerequisites

Before adding photos, you should already have a species dataset
generated with
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md)
or a compatible CSV/XLSX file prepared with the same structure.

You should also have one or more image files for the species you want to
illustrate.

## Step 1. Prepare the species data

If you have not yet created your dataset, start with
[`arboretum_data()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_data.md):

``` r
library(aRboretum)

spp_list <- c("Luetzelburgia bahiensis", "Paubrasilia echinata")

arboretum_data(
  spp_list = spp_list,
  save = TRUE,
  format = "csv",
  dir = "arboretum_data"
)
```

This creates a folder such as `arboretum_data/` containing the exported
species dataset.

## Step 2. Create the photo folder structure

Run
[`arboretum_photos()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_photos.md)
with the path to your species data file:

``` r
arboretum_photos(
  data_path = "arboretum_data/arboretum_data.csv",
  verbose = TRUE
)
```

This creates a directory named `arboretum_photos/` in your working
directory. Inside, you will find one subfolder per species, named like
this:

``` text
arboretum_photos/
├── FABACEAE_Luetzelburgia_bahiensis_photos/
└── FABACEAE_Paubrasilia_echinata_photos/
```

## Step 3. Add your photo files

Place your image files inside the corresponding species folder.

Supported formats include:

- `.jpg`
- `.jpeg`
- `.png`
- `.gif`
- `.webp`
- `.bmp`
- `.svg`

For example, for *Paubrasilia echinata* you might place:

``` text
arboretum_photos/FABACEAE_Paubrasilia_echinata_photos/
├── whole_paubrasilia.jpg
└── paubrasilia_fruit.png
```

There is no required file naming convention. All supported image files
found in the folder are detected automatically.

It is usually best to keep image files reasonably small so the final
HTML labels remain fast to load.

## Step 4. Generate labels with photos

Now run
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
and indicate the photo folder with the `photo_dir` argument:

``` r
arboretum_labels(
  data_path = "arboretum_data/arboretum_data.csv",
  photo_dir = "arboretum_photos",
  dir = "arboretum_species_labels"
)
```

The function will:

- copy the full `arboretum_photos/` folder into the output directory;
- rename the copied folder with a leading `__` to avoid conflicts; and
- embed the available species photos as a slideshow in each HTML label.

## Step 5. View the labels

Open one of the generated HTML files in your browser, for example:

``` text
arboretum_species_labels/FABACEAE_Paubrasilia_echinata_label.html
```

If photos were found for that species, the label will include a
slideshow section with:

- image navigation arrows;
- progress indicators; and
- automatic cycling across the available photos.

## How the output is organized

When
[`arboretum_labels()`](https://DBOSlab.github.io/aRboretum/reference/arboretum_labels.md)
is run with `photo_dir = "arboretum_photos"`, the output directory
typically looks like this:

``` text
arboretum_species_labels/
├── __arboretum_photos/
│   ├── FABACEAE_Luetzelburgia_bahiensis_photos/
│   │   └── ...
│   └── FABACEAE_Paubrasilia_echinata_photos/
│       ├── whole_paubrasilia.jpg
│       └── paubrasilia_fruit.png
├── FABACEAE_Luetzelburgia_bahiensis_label.html
└── FABACEAE_Paubrasilia_echinata_label.html
```

This keeps the photo assets associated with the generated labels while
preserving portability of the output folder.

## Next steps

To add personal recordings to the same labels, see the vignette
`howto_audios`.

To build a quick end-to-end workflow from species list to final labels,
see the `quickstart` vignette.
