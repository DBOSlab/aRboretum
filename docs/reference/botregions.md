# Countries and associated classification of botanical divisions

Countries and associated classification of botanical divisions according
to the World Geographical Scheme for Recording Plant Distributions
(WGSRPD). This dataset provides the tabular look-up table with
multilingual names for all botanical regions recognized by the standard.

## Usage

``` r
data(botregions)
```

## Format

A `data.frame` with the following columns:

- country:

  Character. Botanical Country names (Level 3)

- botanical_division:

  Character. Basic Recording Unit names (Level 4)

- continent:

  Character. Continental level names (Level 1)

- country_EN, country_ES, country_FR, country_PT:

  Character. Botanical Country names in English, Spanish, French, and
  Portuguese

- botanical_division_EN, botanical_division_ES, botanical_division_FR,
  botanical_division_PT:

  Character. Basic Recording Unit names in English, Spanish, French, and
  Portuguese

- continent_EN, continent_ES, continent_FR, continent_PT:

  Character. Continental names in English, Spanish, French, and
  Portuguese

## Source

<https://www.tdwg.org/standards/wgsrpd/>

Brummitt, R.K. (2001). World Geographic Scheme for Recording Plant
Distributions, Edition 2. Hunt Institute for Botanical Documentation,
Carnegie Mellon University, Pittsburgh.

## Examples

``` r
if (FALSE) { # \dontrun{
library(aRboretum)
data(botregions)
head(botregions)

# Show botanical divisions in South America (Portuguese names)
subset(botregions, continent_EN == "South America",
       select = c(botanical_division_PT, country_PT))

# Count botanical countries by continent
table(botregions$continent_EN)
} # }
```
