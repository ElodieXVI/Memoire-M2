# ACT en question ####

library(pacman)
p_load(tidyverse,knitr,questionr,readxl,survey,gtsummary, rstatix, magrittr, #Pour analyses et manipulation
       ggcharts, huxtable, #pour les tableaux
       GGally, extrafont)
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
source("Code/Recodage.R")
mecs <- subset(act, CATEG_rec == "MECS")

rep <- mecs[, c("REPAGE", "REPSEX", "REPPED", "REPDUR", "REPCIV","REPAUC", "AGEMIN", "AGEMAX", "PUBACC")]

t1 <- rep %>% tbl_cross(row = REPAGE, col = PUBACC)
t2 <- rep %>% tbl_cross(row = REPSEX, col = PUBACC)
t3 <- rep %>% tbl_cross(row = REPPED, col = PUBACC)
t4 <- rep %>% tbl_cross(row = REPDUR, col = PUBACC)
t5 <- rep %>% tbl_cross(row = REPCIV, col = PUBACC)
t6 <- rep %>% tbl_cross(row = REPAUC, col = PUBACC)

tbl_stack(list(t1,t2,t3, t4,t5, t6)) %>% 
  modify_caption("Tableau ")%>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)
