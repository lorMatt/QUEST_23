if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer')

# Data management --------------------------------------------------------------
## Data import ----
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                       sheet = "Lavorare", skip = 1) |> 
  filter(eta <= 35)

## Data wrangling/cleaning -----------------------------------------------------

### rest factor 
lavRecode <- function(x) {factor(paste(x), levels = c(
  'Per nulla',
  'Poco',
  'Abbastanza',
  'Molto')
)}
Lavorare <- Lavorare |> 
  mutate(across(names(Lavorare[startsWith(names(Lavorare),"risc")]),
                lavRecode))

# Dataviz ----

## Future risks
risc <- tibble(.rows = 4, choice = c('Per nulla',
                                     'Poco',
                                     'Abbastanza',
                                     'Molto'))
for (i in 1:6) {
  vec <- Lavorare[startsWith(names(Lavorare),"risc")] |> 
    group_by(Lavorare[startsWith(names(Lavorare),"risc")][i]) |>
    drop_na() |> 
    count(name = paste(names(Lavorare[startsWith(names(Lavorare),"risc")][i]), '_n')) |> 
    rename('choice' = names(Lavorare[startsWith(names(Lavorare),"risc")][i]))
  
  risc <- full_join(risc, vec |> mutate(choice = choice))
  
}
rm(i)
#### data wrangling
risc2 <- data.frame(t(risc[-1])) # swapping columns-rows
colnames(risc2) <- risc$choice

risc <- rownames_to_column(risc2) |>
  mutate(rowname = gsub('_n', '', rowname)) |> # column 
  rename(choice = rowname)

rm(risc2)

risc <- risc |> 
  mutate(index = round(((Abbastanza + Molto)/252)*100, 2)) # % di abbastanza + molto importante

### graphics
risc |> 
  mutate(choice = case_match(choice,
                             'riscPrec ' ~	'Precarietà, incertezza, disoccupazione',
                             'riscPov '  ~  'Lavorare sotto la soglia di povertà',
                             'riscMism ' ~	'Lavoro inadeguato rispetto alla formazione/attitudini',
                             'riscFam '  ~  'Non poter costruire una famiglia',
                             'riscProp ' ~	'Non poter avere una casa di proprietà',
                             'riscPens ' ~	'Pensione inadeguata o assente'
  )) |> 
  ggplot(aes(x = choice, y = index, fill = choice)) +
  geom_col() +
  coord_polar() +
  scale_y_continuous(limits = c(0, 80)) +
  labs(title = 'Rischi percepiti dai nostri iscritti') +
  scale_fill_manual(values = met.brewer('Tiepolo', 9)) +
  theme_void() +
  theme(axis.title = element_blank(),
        legend.position = 'right',
        legend.title = element_blank(),
        plot.title = element_text(size = 20, hjust = .5))
ggsave('img/riscLav.pdf', width = 7, height = 5)

## Ambitions
Lavorare |> 
  ggplot(aes(y = aspiraz)) +
  geom_bar() +
  theme_minimal() +
  labs(title = 'Aspirazioni per il futuro') +
  theme(axis.title = element_blank())
ggsave('img/aspiraz.pdf', width = 7, height = 5)
