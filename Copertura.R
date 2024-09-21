if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer', 'sf')

# Data management --------------------------------------------------------------
## Import data 
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                       sheet = "Lavorare", skip = 1) |> 
            filter(id < 256)
Abitare <- read_excel("QUEST_DEF.xlsx",
                      sheet = "Abitare", skip = 1) |> 
            filter(id < 256)
geoCod <- read_excel("Codici-statistici-e-denominazioni-al-30_06_2024.xlsx")
sf <- read_sf('geoData/ISTAT - confini amministrativi/Com01012024_g/Com01012024_g_WGS84.shp')
umbriasf <- read_sf('geoData/ISTAT - confini amministrativi/ProvCM01012024_g/ProvCM01012024_g_WGS84.shp') |> 
  filter(DEN_UTS == 'Terni' | DEN_UTS == 'Perugia')
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
                  select(id, dom, eta))
### factor età
df$eta <- as.numeric(df$eta)
df <- df |> 
  mutate(class_eta = case_match(eta,
                    c(15:19) ~ '15-20',
                    c(20:24) ~ '20-25',
                    c(25:29) ~ '25-30',
                    c(30:34) ~ '30-35',
                    c(35:39) ~ '35-40',
                    c(40:45) ~ '40-45'
  ))

### occ cleaning
df <- df |> 
  mutate(occ = gsub((' (inclusi contratti a nero, precari,  di ricerca, stage, servizio civile)'), '', occ, fixed = T),
         occ = gsub((' (inclusi contratti a nero, precari, di ricerca, stage, servizio civile)'), '', occ, fixed = T))

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
## Mappa copertura geo

ggdom <- domdf |> 
  ggplot() +
  geom_sf(data = umbriasf, aes(geometry = geometry), colour = 'black', fill = 'gray98') +
  geom_sf_interactive(aes(geometry = geometry, fill = n, data_id = dom, tooltip = n)) +
  geom_sf_text(data = ~. |> filter(dom != 'Perugia'),
               aes(geometry = geometry, label = dom), colour = 'black', size = 3) +
  geom_sf_text(data = ~. |> filter(dom == 'Perugia'),
               aes(geometry = geometry, label = dom), colour = 'white', size = 3) +
  scale_fill_viridis_c(option = 'mako', direction = -1) +
  labs(title = 'Mappatura copertura questionario') +
  theme_void() +
  theme(legend.position = 'none',
        plot.title = element_text(family = 'Helvetica', hjust = .5, size = 20))

ggsave('img/mapCop.png', plot = ggdom, width = 8, height = 9)

### Interactive

girafe(ggobj = ggdom,
       width_svg = 8,
       height_svg = 9,
       options = list(
         opts_hover(css = ''),
         opts_hover_inv(css = 'opacity:0.3;'),
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')))

## Univariate
df |> 
  rename(Età = class_eta,
         Genere = gen,
         Occupazione = occ) |> 
  pivot_longer(cols = c(Genere, Occupazione, Età)) |> 
  drop_na() |> 
  ggplot(aes(x=value, fill = name)) +
  scale_fill_met_d('Degas') +
  geom_bar(width = .8) +
  facet_grid(~name, scales = 'free', space = 'free') +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        strip.text = element_text(size = 15),
        axis.text = element_text(size = 11))

ggsave('img/univPlot.png', width = 13, height = 7.5)

## Bivariate       
### Età/occ
df |> 
  drop_na() |> 
  ggplot(aes(x = class_eta, fill = occ)) +
  geom_bar(position = position_dodge2(preserve = 'single')) +
  scale_fill_met_d('Degas', direction = -1) +
  labs(title = 'Copertura per classe di età ed occupazione') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 18))
ggsave('img/etaOccPlot.png', width = 8, height = 7.5)

### Età/gen
df |> 
  drop_na() |> 
  ggplot(aes(x = class_eta, fill = gen)) +
  geom_bar(position = position_dodge2(preserve = 'single')) +
  scale_fill_met_d('Degas', direction = -1) +
  labs(title = 'Copertura per classe di età e genere') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        axis.title = element_blank(),
        plot.title = element_text(hjust = .5, size = 18))
ggsave('img/etaGenPlot.png', width = 8, height = 7.5)

### Genere/età/occ
df |> 
  ggplot(aes(x = class_eta, fill = occ)) +
  geom_bar() +
  scale_fill_met_d('Degas', direction = -1) +
  facet_wrap(vars(gen)) +
  labs(title = 'Copertura per classe di età ed occupazione') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        axis.title = element_blank(),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = .5, size = 18))

ggsave('img/etaGenOccPlot.png', width = 13, height = 10)