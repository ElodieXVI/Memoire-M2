# Modèles sortie MECS ####

require(nnet)

mecs$MES_rec <- fct_recode(mecs$MES_rec,NULL = "NA")

modele <- mecs %>% select("age_ed", "SEX", "MES_rec", "SMES_rec","ARES_rec", "SRES_rec", "HEBE_rec3","MNA") %>% drop_na()

modele$SRES_rec <- fct_recode(modele$SRES_rec,
                              "Famille_assfam" = "Famille",
                              "Famille_assfam" = "Assfam",
                              "Logthors_autres" = "Logthors",
                              "Logthors_autres" = "Autres")
## Réordonnancement de modele$SRES_rec
modele$SRES_rec <- modele$SRES_rec %>%
  fct_relevel("EtabASEPJJ","Famille_assfam","Logthors_autres", "Logtacc")

modele$ARES_rec <- fct_recode(modele$ARES_rec,
                              "Famille_assfam" = "Famille",
                              "Famille_assfam" = "Assfam",
                              "Logthors_autres" = "Logthors",
                              "Logthors_autres" = "Autres")
## Réordonnancement de modele$SRES_rec
modele$ARES_rec <- modele$ARES_rec %>%
  fct_relevel("EtabASEPJJ","Logthors_autres", "Logtacc", "Famille_assfam")

## Réordonnancement de modele$HEBE_rec3
modele$HEBE_rec3 <- modele$HEBE_rec3 %>%
  fct_relevel("Collect","Autonomautre", "Assfam", "Domicile")

## Réordonnancement de modele$SEX
modele$SEX <- modele$SEX %>% fct_relevel("Homme", "Femme")

mod1 <- multinom(SRES_rec ~ age_ed + SEX + HEBE_rec3 + MNA, data = modele)

mod1 %>% tbl_regression(exponentiate = TRUE, 
                        label = list(age_ed ~ "Âge entrée en MECS",
                                     SEX ~ "Sexe",
                                     HEBE_rec3 ~ "Type d'hébergement en MECS")) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Collectif**") %>%
  modify_caption("Modèle 1 réduit : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  multinom_pivot_wider() %>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)


mod2 <- multinom(SRES_rec ~ age_ed + SEX + HEBE_rec3 + ARES_rec + MNA, data = modele)
mod2 %>% tbl_regression(exponentiate = TRUE, 
                        label = list(age_ed ~ "Âge entrée en MECS",
                                     SEX ~ "Sexe",
                                     HEBE_rec3 ~ "Type d'hébergement en MECS",
                                     ARES_rec ~ "Type d'hébergement avant MECS")) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Collectif**") %>%
  modify_caption("Modèle 2 complet : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  multinom_pivot_wider()%>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)


mod3 <- multinom(SRES_rec ~ age_ed + SEX + MNA + HEBE_rec3 + ARES_rec + ARES_rec*SEX, data = modele)
mod3 %>% tbl_regression(exponentiate = TRUE, 
                        label = list(age_ed ~ "Âge entrée en MECS",
                                     SEX ~ "Sexe",
                                     HEBE_rec3 ~ "Type d'hébergement en MECS",
                                     ARES_rec ~ "Type d'hébergement avant MECS")) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Collectif**") %>%
  modify_caption("Modèle 3 avec interaction : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  multinom_pivot_wider()%>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

mod4 <- multinom(SRES_rec ~ age_ed + SEX + MNA + HEBE_rec3 + ARES_rec + HEBE_rec3*SEX + ARES_rec*SEX, data = modele)
mod4 %>% tbl_regression(exponentiate = TRUE, 
                        label = list(age_ed ~ "Âge entrée en MECS",
                                     SEX ~ "Sexe",
                                     HEBE_rec3 ~ "Type d'hébergement en MECS",
                                     ARES_rec ~ "Type d'hébergement avant MECS")) %>% 
  add_significance_stars(hide_se = TRUE, hide_ci = TRUE) %>%
  modify_header(label = "**Référence : Collectif**") %>%
  modify_caption("Modèle 4 avec interaction 2 : type d'hébergement à la sortie de MECS")  %>% 
  modify_column_hide("ci") %>% 
  bold_labels()%>%
  multinom_pivot_wider()%>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

t1 <- modele %>% tbl_cross(row = ARES_rec, col = SRES_rec)
t2 <- modele %>% tbl_cross(row = SEX, col = SRES_rec)
t3 <- modele %>% tbl_cross(row = age_ed, col = SRES_rec)
t4 <- modele %>% tbl_cross(row = HEBE_rec3, col = SRES_rec)
t5 <- modele %>% tbl_cross(row = MNA, col = SRES_rec)
tbl_stack(list(t1,t2,t3, t4, t5)) %>% 
  modify_caption("Tableau de vérification des effectifs en vue du modèle")%>% 
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 10 983 enfants sortis au cours de 2017.") %>%
  set_number_format(NA)

test <- subset(modele, SEX == "Femme")
test %>% tbl_cross(row = ARES_rec, col = SRES_rec)

test <- subset(modele, SEX == "Homme")
test %>% tbl_cross(row = ARES_rec, col = SRES_rec)