if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'plotly', 'ggplot2', 'readxl', 'gt')

# Data import ------------------------------------------------------------------
Abitare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Abitare", skip = 1)
Studiare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Studiare", skip = 1)
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Lavorare", skip = 1)
Restare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Restare", skip = 1)
Impegnarsi <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Impegnarsi", skip = 1)

# save.image(file = 'QData_23.RData')