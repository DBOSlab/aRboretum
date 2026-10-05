# Shared fixtures for the aRboretum tests

# A complete species table, as written by arboretum_data()
.fixture_species_df <- function() {
  data.frame(
    family = c("Fabaceae", "Rubiaceae"),
    genus = c("Paubrasilia", "Coffea"),
    taxonName = c("Paubrasilia echinata", "Coffea arabica"),
    scientificNameAuthorship = c("(Lam.) Gagnon, H.C.Lima & G.P.Lewis", "L."),
    FFB.vernacularName = c("pau-brasil | ibirapitanga", NA),
    country = c("Brazil", "Ethiopia | Kenya"),
    endemism = c("Endemic", "Non-endemic"),
    botanical_country = c("Brazil Northeast", "Ethiopia | Kenya"),
    introduced_to = c("Cuba | Trinidad-Tobago", "Cuba"),
    FFB.establishmentMeans = c("Native", "Cultivated"),
    FFB.stateProvince = c("BA | PE", "SP"),
    FFB.phytogeographicDomain = c("Atlantic Forest", NA),
    FFB.vegetationType = c(NA, NA),
    FFB.genusRichness = c(1, 2),
    FFB.genusRank = c(900, 500),
    IUCN.status = c("Endangered (EN)", NA),
    plant_uses_EN = c("Dye and timber", NA),
    plant_uses_PT = c(NA, NA),
    plant_uses_ES = c(NA, NA),
    plant_uses_FR = c(NA, NA),
    free_notes_EN = c(NA, "Source of coffee"),
    free_notes_PT = c("Árvore nacional do Brasil", NA),
    free_notes_ES = c(NA, NA),
    free_notes_FR = c(NA, NA),
    full_phrases_EN = c(NA, NA),
    full_phrases_PT = c(NA, NA),
    full_phrases_ES = c(NA, NA),
    full_phrases_FR = c(NA, NA),
    full_phrases_ADD_LANGUAGE = c("Texto em panará", NA),
    POWO.url = c("https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1", NA),
    FFB.url = c("https://floradobrasil.jbrj.gov.br/FB1", NA),
    stringsAsFactors = FALSE
  )
}

.write_fixture_csv <- function(path, df = .fixture_species_df()) {
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

# Small offline replacement for geobr::read_state()
.fixture_br_states <- function() {
  sq <- function(x0, y0) {
    sf::st_polygon(list(rbind(c(x0, y0), c(x0 + 5, y0), c(x0 + 5, y0 + 5),
                              c(x0, y0 + 5), c(x0, y0))))
  }
  sf::st_sf(
    abbrev_state = c("BA", "PE", "SP"),
    name_state = c("Bahia", "Pernambuco", "São Paulo"),
    geometry = sf::st_sfc(sq(-45, -15), sq(-40, -9), sq(-50, -25), crs = 4326)
  )
}

# Write a small PNG image to disk
.write_fixture_png <- function(path, n = 20) {
  img <- array(stats::runif(n * n * 3), dim = c(n, n, 3))
  png::writePNG(img, path)
  invisible(path)
}

# Run code with a temporary working directory
.with_tmp_wd <- function(name, code) {
  wd <- file.path(tempdir(), name)
  unlink(wd, recursive = TRUE, force = TRUE)
  dir.create(wd, recursive = TRUE)
  old <- setwd(wd)
  on.exit({
    setwd(old)
    unlink(wd, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  force(code)
}
