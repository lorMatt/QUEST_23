if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'plotly', 'ggplot2', 'readxl', 'gt')

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

abPlotZon |> 
  ggplot(aes(x = zon, y = meanzon, colour = flag)) +
  geom_point() +
  coord_flip() +
  geom_segment(aes(y = ovmean, yend = meanzon, x = zon, xend = zon)) +
  theme_minimal() +
  labs(title = 'Indice di soddisfazione con la condizione abitativa',
       subtitle = 'Scomposizione per tipo di insediamento') + 
  theme(axis.title = element_blank(),
        legend.position = 'none')

ggsave('img/abZon.jpeg', width = 6, height = 7)
