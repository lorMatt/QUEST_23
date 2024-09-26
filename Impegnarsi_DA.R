if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggplot2', 'readxl', 'MetBrewer')

# Data management --------------------------------------------------------------
## Data import -----------------------------------------------------------------

Impegnarsi <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Impegnarsi", skip = 1) |> 
  filter(eta <= 35)

## Data wrangling/cleaning -----------------------------------------------------

### Imp factor
impRecode <- function(x) {factor(paste(x), levels = c(
  'Per nulla',
  'Poco',
  'Abbastanza',
  'Molto')
)}

Impegnarsi <- Impegnarsi |> 
  mutate(across(names(Impegnarsi[startsWith(names(Impegnarsi),"rest")]),
                impRecode))

rm(impRecode)
# Dataviz ----------------------------------------------------------------------
## Sviluppo --------------------------------------------------------------------
### Graphics df building
svil <- tibble(.rows = 4, choice = c('Per nulla',
                                     'Poco',
                                     'Abbastanza',
                                     'Molto'))
for (i in 1:9) {
  vec <- Impegnarsi[startsWith(names(Impegnarsi),"svil")] |> 
    group_by(Impegnarsi[startsWith(names(Impegnarsi),"svil")][i]) |>
    drop_na() |> 
    count(name = paste(names(Impegnarsi[startsWith(names(Impegnarsi),"svil")][i]), '_n')) |> 
    rename('choice' = names(Impegnarsi[startsWith(names(Impegnarsi),"svil")][i]))
  
  svil <- full_join(svil, vec |> mutate(choice = choice))
  
}
rm(i)

#### data wrangling
svil2 <- data.frame(t(svil[-1])) # swapping columns-rows
colnames(svil2) <- svil$choice

svil <- rownames_to_column(svil2) |>
  mutate(rowname = gsub(' _n', '', rowname)) |> # column 
  rename(choice = rowname)
rm(svil2, vec)

svil <- svil |> 
  mutate(index = round(((Abbastanza + Molto)/252)*100, 2), # % di abbastanza + molto importante
         choice = case_match(choice, # recode
                             'svilImp'   ~	'Imprenditorialità e autoimpiego',
                             'svilDig'   ~	'Digitalizzazione nella comunicazione e nei servizi',
                             'svilPart'  ~	'Partecipazione, ruolo e potere dei giovani',
                             'svilMob'   ~	'Potenziamento della mobilità',
                             'svilAgr'   ~	'Priorità sui settori attrattivi (es. turismo,\nenogastronomia, agricoltura, pastorizia, pesca etc.)',
                             'svilCom'   ~	'Incentivazioni delle reti di comunità (solidarietà,\neconomia civile, socialità, etc.)',
                             'svilCult'  ~	'Potenziamento dell’offerta culturale, educativa\nnon formale e del tempo libero',
                             'svilCreat' ~  'Promozione della creatività, dell’arte e dell’innovazione',
                             'svilMark'	 ~	'Marketing e comunicazione territoriale'
                             ),
         cat = c(
           'Economia e servizi',
           'Economia e servizi',
           'Rappresentanza e comunità',
           'Economia e servizi',
           'Economia e servizi',
           'Rappresentanza e comunità',
           'Cultura e creatività',
           'Cultura e creatività',
           'Rappresentanza e comunità'
         ))



## Lollipop chart
svil |> 
  arrange(index) |> 
  mutate(choice=factor(choice, levels=choice)) |> 
  ggplot(aes(y = choice, x = index, colour = cat)) +
  geom_segment(aes(xend = 70, yend = choice)) +
  geom_vline(xintercept = 90, colour = 'gray70') +
  geom_point(size = 4) +
  geom_point(size = 2, colour = 'white') +
  labs(title = 'Servizi, rappresentanza e cultura al centro\ndelle esigenze dei nostri iscritti') +
  scale_x_continuous(limits = c(70,100)) +
  scale_color_met_d('Tiepolo') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title = element_blank())

ggsave('img/svilInd.pdf', width = 8, height = 4.5)
  
## Attualità --------------------------------------------------------------------
### Graphics df building
att <- tibble(.rows = 4, choice = c('Per nulla',
                                     'Poco',
                                     'Abbastanza',
                                     'Molto'))
for (i in 1:12) {
  vec <- Impegnarsi[startsWith(names(Impegnarsi),"att")] |> 
    group_by(Impegnarsi[startsWith(names(Impegnarsi),"att")][i]) |>
    drop_na() |> 
    count(name = paste(names(Impegnarsi[startsWith(names(Impegnarsi),"att")][i]), '_n')) |> 
    rename('choice' = names(Impegnarsi[startsWith(names(Impegnarsi),"att")][i]))
  
  att <- full_join(att, vec |> mutate(choice = choice))
  
}
rm(i)

#### data wrangling
att2 <- data.frame(t(att[-1])) # swapping columns-rows
colnames(att2) <- att$choice

att <- rownames_to_column(att2) |>
  mutate(rowname = gsub(' _n', '', rowname)) |> # column 
  rename(choice = rowname)
rm(att2, vec)
table(Impegnarsi$attAb)

att <- att |> 
  mutate(index = round(((Abbastanza + Molto)/252)*100, 2), # % di abbastanza + molto importante
         choice = case_match(choice, # recode
                             'attSal'	 ~ 'Salario minimo',
                             'attInd'	 ~ 'Industrializzazione',
                             'attEdu'	 ~ 'Istruzione gratuita',
                             'attCost' ~ 'Costo di trasporti e cultura per i giovani',
                             'attAmb'	 ~ 'Salvaguardia dell\'ambiente, crisi climatica',
                             'attViol' ~ 'Violenza di genere',
                             'attSan'	 ~ 'Sanità pubblica e garantita',
                             'attAb'	 ~ 'Diritto all\'aborto libero e sicuro',
                             'attEut'	 ~ 'Legge sul fine-vita',
                             'attFob'	 ~ 'Contrasto all\'omobilesbotransfobia',
                             'attUbi'	 ~ 'Reddito Universale di Base/di Autodeterminazione',
                             'attDrog' ~ 'Legalizzazione delle droghe leggere'),
         cat = c('Mercato del lavoro',
                 'Mercato del lavoro',
                 'Welfare',
                 'Welfare',
                 'Ambiente',
                 'Diritti civili',
                 'Welfare',
                 'Diritti civili',
                 'Diritti civili',
                 'Diritti civili',
                 'Welfare',
                 'Diritti civili')
         )



## Lollipop chart
att |> 
  arrange(index) |> 
  mutate(choice=factor(choice, levels=choice)) |> 
  ggplot(aes(y = choice, x = index, colour = cat)) +
  geom_segment(aes(xend = 50, yend = choice)) +
  geom_vline(xintercept = 90, colour = 'gray70') +
  geom_point(size = 4) +
  geom_point(size = 2, colour = 'white') +
  labs(title = 'Welfare, ambiente e diritti civili sono i temi\nche stanno più a cuore ai nostri iscritti') +
  scale_x_continuous(limits = c(50,100)) +
  scale_color_met_d('Tiepolo') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title = element_blank())

ggsave('img/attInd.pdf', width = 8, height = 4.5)
