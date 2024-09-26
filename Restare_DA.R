if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer', 'waffle', 'extrafont')


# Data management --------------------------------------------------------------
## Font import
font_import (path ="~/Documents/Progetti/RxR/Osservatorio/Data Analysis/QUEST_23", pattern = 'fa-', prompt =  FALSE)


## Data import -----------------------------------------------------------------

Restare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Restare", skip = 1) |> 
  filter(eta <= 35)
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                                   sheet = "Lavorare", skip = 1) |> 
  filter(eta <= 35)
## Data wrangling/cleaning -----------------------------------------------------

### rest factor 
restRecode <- function(x) {factor(paste(x), levels = c(
                                      'Per nulla',
                                      'Poco',
                                      'Abbastanza',
                                      'Molto')
)}
Restare <- Restare |> 
  mutate(across(names(Restare[startsWith(names(Restare),"rest")]),
                restRecode))
Restare <- Restare |> 
  mutate(across(names(Restare[startsWith(names(Restare),"lasc")]),
                restRecode))
limRecode <- function(x) {factor(paste(x), levels = c('Difficilmente lo accetterei',
                                                      'È inaccettabile',
                                                      'L\'ho accettato',
                                                      'Potrei accettarlo')
)}
Restare <- Restare |> 
  mutate(across(names(Restare[startsWith(names(Restare),"lim")]),
                limRecode))

### Rapporto con il luogo di residenza, recode + factor
Restare <- Restare |> 
  mutate(rapp = case_match(rapp,
    c('Anche se non vorrei penso che partirò dall\'Umbria; preferirei di no ma non penso riuscirei a vivere come vorrei',
    'Anche se non vorrei penso che partirò dal posto in cui vivo; preferirei di no ma non penso riuscirei a vivere come vorrei') ~ 'Vorrei restare ma non posso',
    c('Anche se non vorrei penso che resterò, preferirei partire ma non penso riuscirò a farlo',
      'Anche se non vorrei penso che resterò dove sono, preferirei partire ma non penso riuscirò a farlo') ~ 'Vorrei partire ma non posso',
    c('Vorrei restare nel posto in cui vivo',
    'Vorrei restare nel posto in cui vivo, progettando qui la mia vita',
    'Vorrei restare in Umbria, progettando qui la mia vita') ~ 'Vorrei restare nel posto in cui vivo',
    'Vorrei vivere e lavorare altrove, sono convinto di voler partire' ~ 'Sarei contento di vivere e lavorare altrove',
    .default = rapp),
    rapp = factor(rapp, levels = c(
      'Vorrei restare nel posto in cui vivo',
      'Vorrei restare ma non posso',
      'Vorrei partire ma non posso',
      'Sarei contento di vivere e lavorare altrove'
    )
    ))
### occ merge + cleaning
Restare <- full_join(Restare,
                     Lavorare |> select(id, occ))

Restare <- Restare |> 
  mutate(occ = gsub((' (inclusi contratti a nero, precari,  di ricerca, stage, servizio civile)'), '', occ, fixed = T),
         occ = gsub((' (inclusi contratti a nero, precari, di ricerca, stage, servizio civile)'), '', occ, fixed = T))
rm(Lavorare)
# Dataviz ----------------------------------------------------------------------

## Rapporto con la regione -----------------------------------------------------
Restare |> 
  group_by(rapp, occ) |> 
  count() |> 
  ggplot(aes(values = n, label = rapp, colour = rapp)) +
  geom_pictogram(make_proportional = T, radius = unit(.5, "npc"), flip = T, size = 5) +
  scale_label_pictogram(values = 'circle') +
  scale_color_manual(values = met.brewer('Tiepolo', 4)) +
  facet_wrap(~occ) +
  labs(title = 'Attitudine al restare') +
  theme_void() +
  guides(label=guide_legend(nrow=2, byrow=TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 20, hjust = .5))

ggsave('img/attRest.pdf', width = 5, height = 8, device = pdf)
## Motivi per restare ----------------------------------------------------------

### graphics df
#### counting each column
rest <- tibble(.rows = 4, choice = c('Per nulla',
                                     'Poco',
                                     'Abbastanza',
                                     'Molto'))
for (i in 1:10) {
vec <- Restare[startsWith(names(Restare),"rest")] |> 
    group_by(Restare[startsWith(names(Restare),"rest")][i]) |>
    drop_na() |> 
    count(name = paste(names(Restare[startsWith(names(Restare),"rest")][i]), '_n')) |> 
    rename('choice' = names(Restare[startsWith(names(Restare),"rest")][i]))

rest <- full_join(rest, vec |> mutate(choice = choice))

}
rm(i)
#### data wrangling
rest2 <- data.frame(t(rest[-1])) # swapping columns-rows
colnames(rest2) <- rest$choice

rest <- rownames_to_column(rest2) |>
  mutate(rowname = gsub('_n', '', rowname)) |> # column 
  rename(choice = rowname)

rm(rest2)

rest <- rest |> 
  mutate(index = round(((Abbastanza + Molto)/89)*100, 2)) # % di abbastanza + molto importante

### graphics
rest |> 
  filter(choice != 'restFort ') |> 
  mutate(choice = case_match(choice,
                             'restLeg '	~ 'Legame/impegno per la comunità',
                             'restSoc '	~ 'Contatti sociali e umani più gratificanti',
                             'restNat '	~ 'Contatto con la natura',
                             'restQual '	~ 'Qualità e stile di vita',
                             'restOpp '	~ 'Opportunità anche nel restare',
                             'restImp '	~ 'Idea imprenditoriale',
                             'restFam '	~ 'Esigenze personali/familiari',
                             'restCost '	~ 'Costo della vita più basso',
                             'restAmb '	~ 'Scarsa importanza alla carriera'
                             )) |> 
  ggplot(aes(x = choice, y = index, fill = choice)) +
  geom_col() +
  coord_polar() +
  scale_y_continuous(limits = c(0, 85)) +
  labs(title = 'Motivi per restare') +
  scale_fill_manual(values = met.brewer('Tiepolo', 9)) +
  theme_void() +
  theme(axis.title = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = .5))
ggsave('img/motRest.pdf', width = 7, height = 5)

## Motivi per lasciare ---------------------------------------------------------

### graphics df
#### counting each column
lasc <- tibble(.rows = 4, choice = c('Abbastanza', 'Molto', 'Per nulla', 'Poco'))
for (i in 1:10) {
  vec <- Restare[startsWith(names(Restare),"lasc")] |> 
    group_by(Restare[startsWith(names(Restare),"lasc")][i]) |>
    drop_na() |> 
    count(name = paste(names(Restare[startsWith(names(Restare),"lasc")][i]), '_n')) |> 
    rename('choice' = names(Restare[startsWith(names(Restare),"lasc")][i]))
  
  lasc <- full_join(lasc, vec)
  
}
rm(i)
#### data wrangling
lasc2 <- data.frame(t(lasc[-1])) # swapping columns-rows
colnames(lasc2) <-  lasc$choice

lasc <- rownames_to_column(lasc2) |>
  mutate(rowname = gsub('_n', '', rowname)) |> # column 
  rename(choice = rowname)

rm(lasc2)

lasc <- lasc |> 
  mutate(index = round(((Abbastanza + Molto)/166)*100, 2)) # % di abbastanza + molto importante

### graphics
lasc |> 
  mutate(choice = case_match(choice,
                             'lascEsp '	~ 'Ampliare i propri orizzonti',
                             'lascOpp '	~ 'Formazione/offerte di lavoro',
                             'lascImp '	~ 'Idea imprenditoriale',
                             'lascEst '	~ 'Bellezza estetica delle città',
                             'lascSoc '	~ 'Relazioni sociali',
                             'lascFam '	~ 'Realizzazione familiare',
                             'lascServ '	~ 'Offerta di servizi',
                             'lascRit '	~ 'Realizzarsi per poi tornare',
                             'lascTent '	~ 'Tentare a realizzarsi',
                             'lascCult '	~ 'Vita culturale più intensa'
  )) |> 
  ggplot(aes(x = choice, y = index, fill = choice)) +
  geom_col() +
  coord_polar() +
  scale_y_continuous(limits = c(0, 95)) +
  labs(title = 'Motivi per andare') +
  scale_fill_manual(values = met.brewer('Tiepolo', 10)) +
  theme_void() +
  theme(axis.title = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = .5))
ggsave('img/motLasc.pdf', width = 7, height = 5)

## Limiti a restare ------------------------------------------------------------

### graphics df
#### counting each column
lim <- tibble(.rows = 4, choice = c('Difficilmente lo accetterei',
                                    'È inaccettabile',
                                    'L\'ho accettato',
                                    'Potrei accettarlo'))
for (i in 1:11) {
  vec <- Restare[startsWith(names(Restare),"lim")] |> 
    group_by(Restare[startsWith(names(Restare),"lim")][i]) |>
    drop_na() |> 
    count(name = paste(names(Restare[startsWith(names(Restare),"lim")][i]), '_n')) |> 
    rename('choice' = names(Restare[startsWith(names(Restare),"lim")][i]))
  
  lim <- full_join(lim, vec)
  
}
rm(i)
#### data wrangling
lim2 <- data.frame(t(lim[-1])) # swapping columns-rows
colnames(lim2) <-  lim$choice

lim <- rownames_to_column(lim2) |>
  mutate(rowname = gsub('_n', '', rowname)) |> # column 
  rename(choice = rowname)

rm(lim2)

lim <- lim |> 
  mutate(index = round(((`Difficilmente lo accetterei` + `È inaccettabile`)/255)*100, 2)) # % di abbastanza + molto importante

### graphics
lim |> 
  mutate(choice = case_match(choice,
                             'limSpost '~ 'Traspoirti/viaggi difficili e costosi',
                             'limRec '	~ 'Scarsi servizi per il tempo libero',
                             'limConn '	~ 'Problemi di connettività',
                             'limForm '	~ 'Minore offerta formativa',
                             'limOcc '  ~ 'Minori opportunità lavorative',
                             'limCarr '	~ 'Minori opportunità di avanzamento di carriera',
                             'limSal '  ~ 'Stipendio inferiore',
                             'limQual '	~ 'Lavorare al di sotto della propria qualifica',
                             'limAb '	  ~ 'Alloggi non soddisfacenti',
                             'limSan '  ~ 'Sanità lontana, inefficiente, costosa',
                             'limCom '  ~ 'Esclusione dalla comunità per le proprie scelte'
  )) |> 
  ggplot(aes(x = choice, y = index, fill = choice)) +
  geom_col() +
  coord_polar() +
  scale_y_continuous(limits = c(0, 95)) +
  labs(title = 'Limiti a rimanere',
       subtitle = 'Indice assegnato per numerosità') +
  scale_fill_manual(values = met.brewer('Tiepolo', 11)) +
  theme_void() +
  theme(axis.title = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = .5),
        plot.subtitle = element_text(size = 12, hjust = .5))
ggsave('img/limRest.pdf', width = 8, height = 5)

