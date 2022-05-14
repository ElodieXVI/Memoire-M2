# Modèles entrée MECS ####

setwd("/Users/elodielemaire/Desktop/Mémoire M2")
library(pacman)
p_load(tidyverse,knitr,questionr,readxl,survey,gtsummary, rstatix, magrittr, #Pour analyses et manipulation
       ggcharts, huxtable, #pour les tableaux
       GGally, extrafont) #pour les graphiques
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
source("Recodage.R")
source("Fonctions.R")


enf$ARES_rec <- recode_res(enf$ARES)

enf$mecs <- case_when(enf$CATEG_rec == "MECS" ~ "En mecs",
                      T ~ "Pas en mecs")

modele <- enf %>% select("CATEG_rec","age_ed","age_ppd","SEX", "MES_rec","AMES_rec", "ARES_rec", "HEBE_rec3","MNA", "mecs", "HAND") %>% drop_na()

modele %<>%  subset(CATEG_rec != "Pouponnières" & CATEG_rec != "Foyers")


## Recodage de modele$mecs
modele$mecs <- modele$mecs %>%
  fct_recode("1" = "En mecs", "0" = "Pas en mecs")

## Réordonnancement de modele$ARES_rec
modele$ARES_rec <- modele$ARES_rec %>%
  fct_relevel("Autres",
              "EtabASEPJJ", "Famille", "Assfam", "Logtacc", "Logthors")

## Réordonnancement de modele$SEX
modele$SEX <- modele$SEX %>% fct_relevel("Homme", "Femme")
## Réordonnancement de modele$mecs
modele$mecs <- modele$mecs %>%
  fct_relevel(
    "0", "1")

m1 <- glm(mecs ~ age_ppd + age_ed + SEX + ARES_rec, data = modele, family = binomial(logit))

m1 %>% tbl_regression(exponentiate = TRUE) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Pas en MECS**") %>%
  modify_caption("Modèle 1 réduit : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

m2 <- glm(mecs ~ age_ed + SEX + ARES_rec + MNA + HAND, data = modele, family = binomial(logit))

m2 %>% tbl_regression(exponentiate = TRUE) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Pas en MECS**") %>%
  modify_caption("Modèle 2 MNA + HAND : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)


m3 <- glm(mecs ~ age_ed + SEX + ARES_rec + MNA + HAND + ARES_rec*SEX, data = modele, family = binomial(logit))

m3 %>% tbl_regression(exponentiate = TRUE) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Pas en MECS**") %>%
  modify_caption("Modèle 3 interaction : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

m4 <- glm(mecs ~ age_ed + SEX + ARES_rec + MNA + HAND + ARES_rec*SEX + MNA*SEX, data = modele, family = binomial(logit))

m4 %>% tbl_regression(exponentiate = TRUE) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Pas en MECS**") %>%
  modify_caption("Modèle 3 interaction : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

t1 <- modele %>% tbl_cross(row = ARES_rec, col = mecs)
t2 <- modele %>% tbl_cross(row = SEX, col = mecs)
t3 <- modele %>% tbl_cross(row = age_ed, col = mecs)
t4 <- modele %>% tbl_cross(row = age_ppd, col = mecs)
t5 <- modele %>% tbl_cross(row = MNA, col = mecs)
tbl_stack(list(t1,t2,t3, t4, t5)) %>% 
  modify_caption("Tableau de vérification des effectifs en vue du modèle")%>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

test <- subset(modele, SEX == "Femme")
test %>% tbl_cross(row = ARES_rec, col = mecs)

test <- subset(modele, SEX == "Homme")
test %>% tbl_cross(row = ARES_rec, col = mecs)