packs <- c("forcats", "Publish", "data.table", "doParallel", "dplyr",
           "purrr", "stringr", "broom", "rms", "splines", "officer",
           "RColorBrewer", "circlize", "ggpubr", "ggthemes", "lubridate",
           "emmeans", "marginaleffects", "forestploter", "grid", "ggbeeswarm",
           "RColorBrewer", "readxl", "janitor", "colorspace")
lapply(packs, require, character.only = TRUE)
source("C:/Ahmed's Stuff/ResearchStuff/Multi-purpose R code/Multi-purpose functions.R")
source("Specific Functions for FAERS.R")