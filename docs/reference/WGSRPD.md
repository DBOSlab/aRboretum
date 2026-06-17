# World Geographical Scheme for Recording Plant Distributions - Spatial Map

Spatial map data (polygon boundaries) for the World Geographical Scheme
for Recording Plant Distributions (WGSRPD). This dataset provides the
geographic boundaries for the standard set of botanical recording units
used in plant distribution studies.

This dataset represents **Level 3** (Botanical Countries) of the WGSRPD
standard, which are "botanical countries" that may ignore purely
political considerations. Antarctica has been excluded, and country
names for DR Congo and Sudan-South Sudan have been corrected for
consistency.

## Usage

``` r
data(WGSRPD)
```

## Format

A simple features (`sf`) object with polygon geometry and the following
attributes:

- LEVEL3_COD:

  Character. Level 3 code (botanical country code)

- LEVEL3_NAM:

  Character. Level 3 name (botanical country name)

- LEVEL1_COD:

  Character. Level 1 code (continental code)

- LEVEL1_NAM:

  Character. Level 1 name (continental name)

- geometry:

  sfc polygon. The boundary polygons for each botanical country

## Source

<https://www.tdwg.org/standards/wgsrpd/>

The original shapefiles are maintained by the TDWG Geographical Schemes
Interest Group and were sourced from: <https://github.com/tdwg/wgsrpd>

The standard is published as: Brummitt, R.K. (2001). World Geographic
Scheme for Recording Plant Distributions, Edition 2. Hunt Institute for
Botanical Documentation, Carnegie Mellon University, Pittsburgh.

## See also

[`botregions`](https://DBOSlab.github.io/aRboretum/reference/botregions.md)
for the multilingual tabular look-up table of botanical divisions.

## Examples

``` r
if (FALSE) { # \dontrun{
library(aRboretum)
library(sf)
data(WGSRPD)

# Check coordinate reference system
st_crs(WGSRPD)

# Plot all botanical countries
plot(WGSRPD["LEVEL3_NAM"], main = "WGSRPD Botanical Countries")

# Subset to South America (Level 1 code "84")
south_america <- subset(WGSRPD, LEVEL1_COD == "84")
plot(south_america["LEVEL3_NAM"],
     main = "Botanical Countries of South America")

# Count botanical countries by continent
table(WGSRPD$LEVEL1_NAM)

# Merge with multilingual names from botregions
data(botregions)
merged <- merge(WGSRPD, botregions,
                by.x = "LEVEL3_NAM",
                by.y = "country_EN")
} # }
```
