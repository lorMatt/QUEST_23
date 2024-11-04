if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer')

# Data management --------------------------------------------------------------
## Import data
Abitare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Abitare", skip = 1) |> 
  filter(eta <= 35)
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                       sheet = "Lavorare", skip = 1) |> 
  filter(eta <= 35)
geoCod <- read_excel("Codici-statistici-e-denominazioni-al-30_06_2024.xlsx")

## Data cleaning/wrangling -----------------------------------------------------
### Domicilio - cleaning
Abitare$dom <- as.character(Abitare$dom)
Abitare$dom <- ifelse(Abitare$dom == 'Stesso della residenza', Abitare$res, Abitare$dom)

## Index recode
abRecode <- function(x) {case_match(paste(x),
        'Per nulla' ~ 1,
        'Poco' ~ 2,
        'Abbastanza' ~ 3,
        'Molto' ~ 4
      )}

Abitare <- Abitare |> 
    mutate(across(c(abQual, abCost, abPriv, abVic, abColl),
                  abRecode
                  )
           )
rm(abRecode)

### Additive index def
Abitare <- Abitare |> 
  mutate(abInd = rowSums(across(c(abQual, abCost, abPriv, abVic, abColl)))) |> 
  mutate(abInd = (abInd-5)/15 * 100) |> 
  mutate(abInd = round(abInd, 2)) |> 
  relocate(abInd, .after = abColl)

## Province
prov <- geoCod |> 
  select(`Denominazione (Italiana e straniera)`, `Unità territoriale sovracomunale`) |> 
  rename(denom = `Denominazione (Italiana e straniera)`,
         prov = `Unità territoriale sovracomunale`)

Abitare <- left_join(Abitare, prov, by = join_by(dom == denom))
Abitare <- Abitare |> 
  relocate(prov, .after = dom) |> 
  rename(prov_dom = prov)

# Dataviz ----------------------------------------------------------------------
## Index by province ----
abPlotProv <- Abitare |> 
  group_by(prov_dom) |>
  summarise(meanprov = mean(abInd)) |> 
  mutate(ovmean = mean(meanprov),
         flag = ifelse(meanprov > ovmean, T, F),
         prov_dom = factor(prov_dom,
                           levels = prov_dom[order(meanprov)])) |> 
  filter(!is.na(prov_dom))
  

abPlotProv |> 
  ggplot(aes(x = prov_dom, y = meanprov, colour = flag)) +
  geom_point(size = 6) +
  geom_segment(aes(y = ovmean, yend = meanprov, x = prov_dom, xend = prov_dom)) +
  geom_point(size = 4, colour = 'white') +
  labs(title = 'Indice di soddisfazione con la condizione abitativa',
       subtitle = 'Scomposizione per province') + 
  geom_hline(yintercept = abPlotProv$ovmean[1], colour = 'gray70', size = 0.3) +
  scale_y_continuous(n.breaks = 4) +
  scale_color_met_d('Degas') +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.text.y = element_text(size = 11))

ggsave('img/abProv.pdf', width = 8, height = 7)

## Index by urban zone ----
abPlotZon <- Abitare |> 
  filter(prov_dom == 'Terni' | prov_dom == 'Perugia') |> 
  group_by(zon) |>
  summarise(meanzon = mean(abInd)) |> 
  mutate(ovmean = mean(meanzon),
         flag = ifelse(meanzon > ovmean, T, F),
         zon = factor(zon,
                           levels = zon[order(meanzon)]))

ggPlotZon <- abPlotZon |> 
  ggplot(aes(x = zon, y = meanzon, colour = flag, data_id = zon, tooltip = round(meanzon, 2))) +
  geom_point_interactive(size = 6) +
  geom_segment_interactive(aes(y = ovmean, yend = meanzon, x = zon, xend = zon)) +
  geom_point(size = 4, colour = 'white') +
  scale_y_continuous(n.breaks = 4) +
  geom_hline(yintercept = abPlotZon$ovmean[1], colour = 'gray70', size = 0.3) +
  scale_color_met_d('Degas') +
  coord_flip() +
  labs(title = 'Indice di soddisfazione con la condizione abitativa',
       subtitle = 'Scomposizione per tipo di insediamento') + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.text.y = element_text(size = 11))

ggsave('img/abZon.pdf', width = 8, height = 5)

### Interactive graph
girafe(ggobj = ggPlotZon,
       width_svg = 8,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')))

### Unpacking abInd for city centre ----
Abitare |> 
  drop_na(abInd) |> 
  filter(zon == 'Centro Cittadino') |> 
  select(abQual, abCost, abPriv, abVic, abColl) |> 
  pivot_longer(c(abQual, abCost, abPriv, abVic, abColl), names_to = 'abSodd', values_to = 'value') |> 
  mutate(abInd = (value-1)/3 * 100) |> 
  group_by(abSodd) |> 
  summarise(abInd = mean(abInd)) |> 
  mutate(abSodd = case_match(abSodd,
                               'abQual'  ~ 'Qualità dell\'immobile (luminosità, dimensioni, riscaldamento)',
                               'abCost'  ~ 'Costi da sostenere (per te o la tua famiglia)',
                               'abPriv'  ~ 'Privacy garantita',
                               'abVic'   ~ 'Vicinanza al centro/ai tuoi interessi',
                               'abColl'  ~ 'Collegamento a zone di interesse con servizio di mobilità pubblica'
  )) |> 
  ggplot(aes(x = reorder(abSodd, -abInd), y = abInd, fill = abInd)) +
  scale_fill_gradient2(high = '#EFD953', mid = '#EFD953',  midpoint = 70, low = '#EF7D4D') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  geom_col(width = .3, show.legend = F) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave('img/abSoddCent.pdf', width = 8, height = 3)

### Unpacking abInd for outskirts ----
Abitare |> 
  drop_na(abInd) |> 
  filter(zon == 'Periferia') |> 
  select(abQual, abCost, abPriv, abVic, abColl) |> 
  pivot_longer(c(abQual, abCost, abPriv, abVic, abColl), names_to = 'abSodd', values_to = 'value') |> 
  mutate(abInd = (value-1)/3 * 100) |> 
  group_by(abSodd) |> 
  summarise(abInd = mean(abInd)) |> 
  mutate(abSodd = case_match(abSodd,
                             'abQual'  ~ 'Qualità dell\'immobile (luminosità, dimensioni, riscaldamento)',
                             'abCost'  ~ 'Costi da sostenere (per te o la tua famiglia)',
                             'abPriv'  ~ 'Privacy garantita',
                             'abVic'   ~ 'Vicinanza al centro/ai tuoi interessi',
                             'abColl'  ~ 'Collegamento a zone di interesse con servizio di mobilità pubblica'
  )) |> 
  ggplot(aes(x = reorder(abSodd, -abInd), y = abInd, fill = abInd)) +
  scale_fill_gradient2(high = '#EFD953', mid = '#EFD953',  midpoint = 70, low = '#EF7D4D') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  geom_col(width = .3, show.legend = F) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave('img/abSoddPer.pdf', width = 8, height = 3)

### Unpacking abInd for rural areas ----
Abitare |> 
  drop_na(abInd) |> 
  filter(zon == 'Area Rurale') |> 
  select(abQual, abCost, abPriv, abVic, abColl) |> 
  pivot_longer(c(abQual, abCost, abPriv, abVic, abColl), names_to = 'abSodd', values_to = 'value') |> 
  mutate(abInd = (value-1)/3 * 100) |> 
  group_by(abSodd) |> 
  summarise(abInd = mean(abInd)) |> 
  mutate(abSodd = case_match(abSodd,
                             'abQual'  ~ 'Qualità dell\'immobile (luminosità, dimensioni, riscaldamento)',
                             'abCost'  ~ 'Costi da sostenere (per te o la tua famiglia)',
                             'abPriv'  ~ 'Privacy garantita',
                             'abVic'   ~ 'Vicinanza al centro/ai tuoi interessi',
                             'abColl'  ~ 'Collegamento a zone di interesse con servizio di mobilità pubblica'
  )) |> 
  ggplot(aes(x = reorder(abSodd, -abInd), y = abInd, fill = abInd)) +
  scale_fill_gradient2(high = '#EFD953', mid = '#EFD953',  midpoint = 70, low = '#EF7D4D') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  geom_col(width = .3, show.legend = F) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave('img/abSoddRur.pdf', width = 8, height = 3)


### Index by urban zone - over occ
abPlotDem <- left_join(Lavorare |> 
                  select(id, occ, gen),
                Abitare) |> mutate(occ = gsub((' (inclusi contratti a nero, precari,  di ricerca, stage, servizio civile)'), '', occ, fixed = T),
                                   occ = gsub((' (inclusi contratti a nero, precari, di ricerca, stage, servizio civile)'), '', occ, fixed = T))

abPlotDem <- abPlotDem |> 
  filter(prov_dom == 'Terni' | prov_dom == 'Perugia') |> 
  group_by(zon, occ) |> 
  summarise(meanzon = mean(abInd)) |> 
  group_by(occ) |> 
  mutate(ovmean = mean(meanzon),
         flag = ifelse(meanzon > ovmean, T, F),
         zon = factor(zon,
                      levels = zon[order(meanzon)]))

abPlotDem |> 
  ggplot(aes(x = zon, y = meanzon, colour = flag, data_id = zon, tooltip = round(meanzon, 2))) +
  geom_point_interactive(size = 6) +
  geom_segment_interactive(aes(y = ovmean, yend = meanzon, x = zon, xend = zon)) +
  geom_point(size = 4, colour = 'white') +
  scale_y_continuous(n.breaks = 4) +
  scale_color_met_d('Degas') +
  coord_flip() +
  labs(title = 'Indice di soddisfazione con la condizione abitativa',
       subtitle = 'Scomposizione per tipo di insediamento') + 
  facet_wrap(~occ, scales = 'free') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.text.y = element_text(size = 11))

ggsave('img/abPlotDem.pdf', width = 13, height = 10)
