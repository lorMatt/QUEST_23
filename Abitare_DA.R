if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt')

# Data management --------------------------------------------------------------
## Import data
Abitare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Abitare", skip = 1)
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
## Index by province
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
  geom_point() +
  coord_flip() +
  geom_segment(aes(y = ovmean, yend = meanprov, x = prov_dom, xend = prov_dom)) +
  theme_minimal() +
  labs(title = 'Indice di soddisfazione con la condizione abitativa',
       subtitle = 'Scomposizione per province') + 
  theme(axis.title = element_blank(),
        legend.position = 'none')

ggsave('img/abProv.jpeg', width = 6, height = 7)

## Index by urban zone
abPlotZon <- Abitare |> 
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
  geom_hline(yintercept = 60.33645, colour = 'gray70', size = 0.3) +
  coord_flip() +
  labs(title = 'Indice di soddisfazione con la condizione abitativa',
       subtitle = 'Scomposizione per tipo di insediamento') + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.text.y = element_text(size = 11))

ggsave('img/abZon.jpeg', width = 8, height = 5)

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


