if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load('tidyverse', 'ggiraph', 'ggplot2', 'readxl', 'gt', 'MetBrewer', 'sf')

Restare <- read_excel("QUEST_DEF.xlsx", 
                      sheet = "Restare", skip = 1)
