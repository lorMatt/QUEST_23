if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'plotly', 'ggplot2', 'readxl', 'gt')

# Data import ------------------------------------------------------------------
Abitare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Abitare", skip = 1) |> 
  filter(id < 256)
Studiare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Studiare", skip = 1) |> 
  filter(id < 256)
Lavorare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Lavorare", skip = 1) |> 
  filter(id < 256)
Restare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Restare", skip = 1) |> 
  filter(id < 256)
Impegnarsi <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Impegnarsi", skip = 1) |> 
  filter(id < 256)

# save.image(file = 'QData_23.RData')