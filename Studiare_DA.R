if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer', 'ggridges')

# Data management --------------------------------------------------------------
## Data import ----
Studiare <- read_excel("QUEST_DEF.xlsx", 
                       sheet = "Studiare", skip = 1) |> 
  filter(eta <= 35) |> 
  filter(studLev != 'Accademia delle Belle Arti' & studLev != 'Istituto Tecnici Superiore (ITS)')

## Index recode
studRecode <- function(x) {case_match(paste(x),
                                    'Per nulla' ~ 1,
                                    'Poco' ~ 2,
                                    'Abbastanza' ~ 3,
                                    'Molto' ~ 4
)}

Studiare <- Studiare |> 
  mutate(across(c(studSoddInt, studSoddEq, studSoddCost, studSoddQual, studSoddSal),
                studRecode
  )
  )
rm(studRecode)

## Additive index def
Studiare <- Studiare |> 
  mutate(studInd = rowSums(across(c(studSoddInt, studSoddEq, studSoddCost, studSoddQual, studSoddSal)))) |> 
  mutate(studInd = (studInd-5)/15 * 100) |> 
  mutate(studInd = round(studInd, 2)) |> 
  relocate(studInd, .after = studLev)

# Dataviz ----------------------------------------------------------------------

## Satisfaction index by edu

studPlotEdu <- Studiare |> 
  group_by(studLev) |>
  filter(!is.na(studLev)) |> 
  summarise(meanStud = mean(studInd)) |> 
  mutate(ovmean = mean(meanStud),
         flag = ifelse(meanStud > ovmean, T, F),
         studLev = factor(studLev,
                           levels = studLev[order(meanStud)]))


studPlotEdu |> 
  ggplot(aes(x = studLev, y = meanStud, colour = flag)) +
  geom_point(size = 6) +
  geom_segment(aes(y = ovmean, yend = meanStud, x = studLev, xend = studLev)) +
  geom_point(size = 4, colour = 'white') +
  labs(title = 'Indice di soddisfazione con il percorso di studi') + 
  geom_hline(yintercept = studPlotEdu$ovmean[1], colour = 'gray70', size = 0.3) +
  scale_y_continuous(n.breaks = 4) +
  scale_color_met_d('Degas') +
  coord_flip() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = .5, size = 20),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.text.y = element_text(size = 11))

ggsave('img/studSoddEdu.pdf', width = 8, height = 3.5)

## Unpacking studInd for university students
Studiare |> 
  drop_na(studInd) |> 
  filter(studLev == 'Università') |> 
  select(studSoddInt, studSoddQual, studSoddSal, studSoddEq, studSoddCost) |> 
  pivot_longer(c(studSoddInt, studSoddQual, studSoddSal, studSoddEq, studSoddCost), names_to = 'studSodd', values_to = 'value') |> 
  mutate(studInd = (value-1)/3 * 100) |> 
  group_by(studSodd) |> 
  summarise(studInd = mean(studInd)) |> 
  mutate(studSodd = case_match(studSodd,
             'studSoddInt'   ~ 'Coerenza con i tuoi interessi',
             'studSoddEq'    ~ 'Carico di lavoro (e bilanciamento con la vita privata)',
             'studSoddQual'  ~ 'Qualità della didattica',
             'studSoddCost'  ~ 'Costi da sostenere (per te o la tua famiglia)',
             'studSoddSal'   ~ 'Salubrità e sicurezza degli spazi'
  )) |> 
  ggplot(aes(x = reorder(studSodd, -studInd), y = studInd, fill = studInd)) +
  scale_fill_gradient2(high = '#EFD953', mid = '#EFD953',  midpoint = 70, low = '#EF7D4D') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  geom_col(width = .3, show.legend = F) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave('img/studSoddUni.pdf', width = 8, height = 3.5)

## Unpacking studInd for high school students
Studiare |> 
  drop_na(studInd) |> 
  filter(studLev == 'Liceo' | studLev == 'Istituto Tecnico/Professionale') |> 
  select(studSoddInt, studSoddQual, studSoddSal, studSoddEq, studSoddCost) |> 
  pivot_longer(c(studSoddInt, studSoddQual, studSoddSal, studSoddEq, studSoddCost), names_to = 'studSodd', values_to = 'value') |> 
  mutate(studInd = (value-1)/3 * 100) |> 
  group_by(studSodd) |> 
  summarise(studInd = mean(studInd)) |> 
  mutate(studSodd = case_match(studSodd,
                               'studSoddInt'   ~ 'Coerenza con i tuoi interessi',
                               'studSoddEq'    ~ 'Carico di lavoro (e bilanciamento con la vita privata)',
                               'studSoddQual'  ~ 'Qualità della didattica',
                               'studSoddCost'  ~ 'Costi da sostenere (per te o la tua famiglia)',
                               'studSoddSal'   ~ 'Salubrità e sicurezza degli spazi'
  )) |> 
  ggplot(aes(x = reorder(studSodd, -studInd), y = studInd, fill = studInd)) +
  scale_fill_gradient2(high = '#EFD953', mid = '#EFD953',  midpoint = 70, low = '#EF7D4D') +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90)) +
  geom_col(width = .3, show.legend = F) +
  theme_minimal() +
  theme(axis.title = element_blank())
  
ggsave('img/studSoddSM.pdf', width = 8, height = 3.5)
