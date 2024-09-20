if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer', 'sf')

# Data management --------------------------------------------------------------
## Import data 
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                       sheet = "Lavorare", skip = 1)
Abitare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Abitare", skip = 1)
geoCod <- read_excel("Codici-statistici-e-denominazioni-al-30_06_2024.xlsx")
sf <- read_sf('geoData/ISTAT - confini amministrativi/Com01012024_g/Com01012024_g_WGS84.shp')
# Data cleaning/wrangling -----------------------------------------------------
## Domicilio - cleaning
Abitare$dom <- as.character(Abitare$dom)
Abitare$dom <- ifelse(Abitare$dom == 'Stesso della residenza', Abitare$res, Abitare$dom)

## Province
prov <- geoCod |> 
  select(`Denominazione (Italiana e straniera)`, `Unità territoriale sovracomunale`) |> 
  rename(denom = `Denominazione (Italiana e straniera)`,
         prov = `Unità territoriale sovracomunale`)

Abitare <- left_join(Abitare, prov, by = join_by(dom == denom))
Abitare <- Abitare |> 
  relocate(prov, .after = dom) |> 
  rename(prov_dom = prov)

## Select and merge quest data
df <- left_join(Lavorare |> 
                  select(id, occ, gen),
                Abitare |> 
                  select(id, dom))
## dom frequency table
domdf <- df |> 
  group_by(dom) |> 
  count()
### geospatial data merge
domdf <- left_join(domdf,
                sf |>
                  select(COMUNE, geometry),
                by = join_by(dom == COMUNE))

# Dataviz ----------------------------------------------------------------------
## Mappa copertura
domdf |> 
  ggplot(aes(x = dom, y = n, data_id = dom, tooltip = n)) +
  geom_sf(aes(geometry = geometry))
