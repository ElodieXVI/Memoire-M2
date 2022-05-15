# ggalluvial ####
## Chargement des packages ####
library(pacman)
p_load(ggalluvial, MetBrewer, tidyverse,extrafont)
source("Recodage.R")
source("Fonctions.R")

## Ajout des pct aux étiquettes ####--------------------------------------------
sor$HEBE_rec1_t <- add_pct(sor$HEBE_rec1)
sor$HEBE_rec2_t <- add_pct(sor$HEBE_rec2)
sor$HEBE_rec3_t <- add_pct(sor$HEBE_rec3)
sor$ARES_rec_t <- add_pct(sor$ARES_rec)
sor$SRES_rec_t <- add_pct(sor$SRES_rec)

## Grpahiques avec les MNA ####-------------------------------------------------
sor$age_e <-sor$ANE - sor$ANN
sor$age_e_d2 <- cut(sor$age_e,
                        include.lowest = TRUE,
                        right = FALSE,
                        dig.lab = 4,
                        breaks = c(0, 9, 14, 16, 26))

sor$age_e_d2 <- sor$age_e_d2 %>%
  fct_recode("0_8" = "[0,9)", "9_13" = "[9,14)", "14_15" = "[14,16)", "16_+" = "[16,26]")

ggplot(na.omit(sor)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec1_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = age_e_d2)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 1 - Rec 1 :Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 30 412 enfants sortis au cours de l'année 2017.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

ggplot(na.omit(sor)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill= ARES_rec)) +
  geom_flow() +
  geom_stratum( fill = "white", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 1 - Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 30 412 enfants sortis au cours de l'année 2017.") +
  theme_classic() +
  theme(legend.position = "bottom")

ggplot(na.omit(sor)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec2_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec2)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 2 - Rec 2 : Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 30 412 enfants sortis au cours de l'année 2017.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

ggplot(na.omit(sor)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 3 - Rec3 : Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 30 412 enfants sortis au cours de l'année 2017.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

ggplot(na.omit(sor)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec4)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 4 - Rec4 : Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 30 412 enfants sortis au cours de l'année 2017.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")


## Sans les MNA ####------------------------------------------------------------

sor_red <- subset(sor, MNA == 0)
sor_red$HEBE_rec3_t <- add_pct(sor_red$HEBE_rec3)
sor_red$ARES_rec_t <- add_pct(sor_red$ARES_rec)
sor_red$SRES_rec_t <- add_pct(sor_red$SRES_rec)

ggplot(na.omit(sor_red)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 2 - Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 20 389 enfants sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

sor_mna <- subset(sor, MNA == 1)
sor_mna$HEBE_rec3_t <- add_pct(sor_mna$HEBE_rec3)
sor_mna$ARES_rec_t <- add_pct(sor_mna$ARES_rec)
sor_mna$SRES_rec_t <- add_pct(sor_mna$SRES_rec)

ggplot(na.omit(sor_mna)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 3 - Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 9 646 enfants sortis au cours de l'année 2017, que les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

## Par tranches d'âge d'entrée (sans les MNA) ####------------------------------

sor_red$age_e <- sor_red$ANE-sor_red$ANN
## Recodage de sor_red$age_e en sor_red$age_ed
sor_red$age_ed <- cut(sor_red$age_e,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 4, 7, 13, 15, 18, 26))
## Recodage de sor_red$age_ed en sor_red$age_ed_rec
sor_red$age_ed <- sor_red$age_ed %>%
  fct_recode("0_3" = "[0,4)",
    "4_6" = "[4,7)",
    "7_12" = "[7,13)",
    "13_14" = "[13,15)",
    "15_17" = "[15,18)",
    "18_+" = "[18,26]")

### Pour les 0 à 3 ans ####
age03 <- subset(sor_red, age_ed == "0_3")
age03$HEBE_rec3_t <- add_pct(age03$HEBE_rec3)
age03$ARES_rec_t <- add_pct(age03$ARES_rec)
age03$SRES_rec_t <- add_pct(age03$SRES_rec)

ggplot(na.omit(age03)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("0 à 3 ans"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 0 à 3 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 2 055 enfants de 0 à 3 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 4 à 6 ans ####
age46 <- subset(sor_red, age_ed == "4_6")
age46$HEBE_rec3_t <- add_pct(age46$HEBE_rec3)
age46$ARES_rec_t <- add_pct(age46$ARES_rec)
age46$SRES_rec_t <- add_pct(age46$SRES_rec)

ggplot(na.omit(age46)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 4 à 6 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 1 620 enfants de 4 à 6 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 7 à 12 ans ####
age712 <- subset(sor_red, age_ed == "7_12")
age712$HEBE_rec3_t <- add_pct(age712$HEBE_rec3)
age712$ARES_rec_t <- add_pct(age712$ARES_rec)
age712$SRES_rec_t <- add_pct(age712$SRES_rec)

ggplot(na.omit(age712)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 7 à 12 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 4 518 enfants de 7 à 12 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 13 à 14 ans ####
age1314 <- subset(sor_red, age_ed == "13_14")
age1314$HEBE_rec3_t <- add_pct(age1314$HEBE_rec3)
age1314$ARES_rec_t <- add_pct(age1314$ARES_rec)
age1314$SRES_rec_t <- add_pct(age1314$SRES_rec)

ggplot(na.omit(age1314)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 13 à 14 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 2 950 enfants de 13 à 14 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 15 à 17 ans ####
age1517 <- subset(sor_red, age_ed == "15_17")
age1517$HEBE_rec3_t <- add_pct(age1517$HEBE_rec3)
age1517$ARES_rec_t <- add_pct(age1517$ARES_rec)
age1517$SRES_rec_t <- add_pct(age1517$SRES_rec)

ggplot(na.omit(age1517)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 15 à 17 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 7 411 enfants de 15 à 17 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 18 ans à + ####
age18 <- subset(sor_red, age_ed == "18_+")
age18$HEBE_rec3_t <- add_pct(age18$HEBE_rec3)
age18$ARES_rec_t <- add_pct(age18$ARES_rec)
age18$SRES_rec_t <- add_pct(age18$SRES_rec)

ggplot(na.omit(age18)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 18 ans et plus") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 1 666 enfants de 18 ans et plus sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

## Par tranches d'âge de premier placement (sans les MNA) ####------------------------------

sor_red$age_pp <- sor_red$ANPP-sor_red$ANN
## Recodage de sor_red$age_e en sor_red$age_ed
sor_red$age_ppd <- cut(sor_red$age_pp,
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(0, 4, 7, 13, 15, 18, 26))
## Recodage de sor_red$age_ed en sor_red$age_ed_rec
sor_red$age_ppd <- sor_red$age_ppd %>%
  fct_recode("0_3" = "[0,4)",
             "4_6" = "[4,7)",
             "7_12" = "[7,13)",
             "13_14" = "[13,15)",
             "15_17" = "[15,18)",
             "18_+" = "[18,26]")

### Pour les 0 à 3 ans ####
age03 <- subset(sor_red, age_ppd == "0_3")
age03$HEBE_rec3_t <- add_pct(age03$HEBE_rec3)
age03$ARES_rec_t <- add_pct(age03$ARES_rec)
age03$SRES_rec_t <- add_pct(age03$SRES_rec)

ggplot(na.omit(age03)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("0 à 3 ans"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge de 1er placement : 0 à 3 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 2 185 enfants dont le 1er placement était entre leurs 0 et 3 ans et qui sont sortis au cours \n de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 4 à 6 ans ####
age46 <- subset(sor_red, age_ppd == "4_6")
age46$HEBE_rec3_t <- add_pct(age46$HEBE_rec3)
age46$ARES_rec_t <- add_pct(age46$ARES_rec)
age46$SRES_rec_t <- add_pct(age46$SRES_rec)

ggplot(na.omit(age46)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge 1er placement : 4 à 6 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 1 493 enfants dont le 1er placement était entre leurs 4 à 6 ans et qui sont sortis au cours de \n l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 7 à 12 ans ####
age712 <- subset(sor_red, age_ppd == "7_12")
age712$HEBE_rec3_t <- add_pct(age712$HEBE_rec3)
age712$ARES_rec_t <- add_pct(age712$ARES_rec)
age712$SRES_rec_t <- add_pct(age712$SRES_rec)

ggplot(na.omit(age712)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge 1er placement : 7 à 12 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 3 253 enfants dont le 1er placement était entre leurs 7 à 12 ans et qui sont sortis au cours de \n l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 13 à 14 ans ####
age1314 <- subset(sor_red, age_ppd == "13_14")
age1314$HEBE_rec3_t <- add_pct(age1314$HEBE_rec3)
age1314$ARES_rec_t <- add_pct(age1314$ARES_rec)
age1314$SRES_rec_t <- add_pct(age1314$SRES_rec)

ggplot(na.omit(age1314)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge 1er placement : 13 à 14 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 1 818 enfants dont le 1er placement était entre leurs 13 à 14 ans et qui sont sortis au cours de \n l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 15 à 17 ans ####
age1517 <- subset(sor_red, age_ppd == "15_17")
age1517$HEBE_rec3_t <- add_pct(age1517$HEBE_rec3)
age1517$ARES_rec_t <- add_pct(age1517$ARES_rec)
age1517$SRES_rec_t <- add_pct(age1517$SRES_rec)

ggplot(na.omit(age1517)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge d'entrée en HEBE : 15 à 17 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 3 401 enfants dont le 1er placement était entre leurs 15 à 17 ans et qui sont sortis au cours de \n l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 18 ans à + ####
age18 <- subset(sor_red, age_ppd == "18_+")
age18$HEBE_rec3_t <- add_pct(age18$HEBE_rec3)
age18$ARES_rec_t <- add_pct(age18$ARES_rec)
age18$SRES_rec_t <- add_pct(age18$SRES_rec)

ggplot(na.omit(age18)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Âge 1er placement : 18 ans et plus") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 429 enfants dont le 1er placement était entre leurs 18 ans et plus et qui sont sortis au cours de \n l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")



# Test 2eme découpage de l'âge ####-------------------------------------------
#Recodage par quantile 
## Recodage de sor_red$age_e en sor_red$age_e_d2
sor_red$age_e_d2 <- cut(sor_red$age_e,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 9, 14, 16, 26))

## Recodage de sor_red$age_e_d2
sor_red$age_e_d2 <- sor_red$age_e_d2 %>%
  fct_recode("0_8" = "[0,9)", "9_13" = "[9,14)", "14_15" = "[14,16)", "16_+" = "[16,26]")

### Pour les 0 à 8 ans ####
age08 <- subset(sor_red, age_e_d2 == "0_8")
age08$HEBE_rec3_t <- add_pct(age08$HEBE_rec3)
age08$ARES_rec_t <- add_pct(age08$ARES_rec)
age08$SRES_rec_t <- add_pct(age08$SRES_rec)

ggplot(na.omit(age08)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = ARES_rec)) +
  geom_flow() +
  geom_stratum( fill = "white", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("0 à 8 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 4 981 enfants de 0 à 8 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 9 à 13 ans ####
age913 <- subset(sor_red, age_e_d2 == "9_13")
age913$HEBE_rec3_t <- add_pct(age913$HEBE_rec3)
age913$ARES_rec_t <- add_pct(age913$ARES_rec)
age913$SRES_rec_t <- add_pct(age913$SRES_rec)

ggplot(na.omit(age913)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum( fill = "white", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("9 à 13 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 4 405 enfants de 9 à 13 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 14 à 15 ans ####
age1415 <- subset(sor_red, age_e_d2 == "14_15")
age1415$HEBE_rec3_t <- add_pct(age1415$HEBE_rec3)
age1415$ARES_rec_t <- add_pct(age1415$ARES_rec)
age1415$SRES_rec_t <- add_pct(age1415$SRES_rec)

ggplot(na.omit(age1415)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum( fill = "white", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("14 à 15 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 3 962 enfants de 14 à 15 ans sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 16 ans et + ####
age16 <- subset(sor_red, age_e_d2 == "16_+")
age16$HEBE_rec3_t <- add_pct(age16$HEBE_rec3)
age16$ARES_rec_t <- add_pct(age16$ARES_rec)
age16$SRES_rec_t <- add_pct(age16$SRES_rec)

ggplot(na.omit(age16)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum( fill = "white", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("16 ans à plus") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 6 812 enfants de 16 ans à plus sortis au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

## Par tranches d'âge de 1er placement (sans les MNA) ####------------------------------
sor_red$age_pp <- sor_red$ANPP-sor_red$ANN
## Recodage de sor_red$age_pp en sor_red$age_ppd
sor_red$age_ppd <- cut(sor_red$age_pp,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 5, 11, 15, 22))

## Recodage de sor_red$age_ppd
sor_red$age_ppd <- sor_red$age_ppd %>%
  fct_recode(
    "0_4" = "[0,5)",
    "5_10" = "[5,11)",
    "11_14" = "[11,15)",
    "15_+" = "[15,22]")

### Pour les 0 à 4 ans ####
age04 <- subset(sor_red, age_ppd == "0_4")
age04$HEBE_rec3_t <- add_pct(age04$HEBE_rec3)
age04$ARES_rec_t <- add_pct(age04$ARES_rec)
age04$SRES_rec_t <- add_pct(age04$SRES_rec)

ggplot(na.omit(age04)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Premier placement entre 0 et 4 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 2 672 enfants ayant eu premier placement entre 0 et 4 ans et sortis de l'établissement d'observation 
       \n au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 5 à 10 ans ####
age510 <- subset(sor_red, age_ppd == "5_10")
age510$HEBE_rec3_t <- add_pct(age510$HEBE_rec3)
age510$ARES_rec_t <- add_pct(age510$ARES_rec)
age510$SRES_rec_t <- add_pct(age510$SRES_rec)

ggplot(na.omit(age510)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Premier placement entre 5 et 10 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 3 103 enfants ayant eu premier placement entre 5 et 10 ans et sortis de l'établissement d'observation 
       \n au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 11 à 14 ans ####
age1114 <- subset(sor_red, age_ppd == "11_14")
age1114$HEBE_rec3_t <- add_pct(age1114$HEBE_rec3)
age1114$ARES_rec_t <- add_pct(age1114$ARES_rec)
age1114$SRES_rec_t <- add_pct(age1114$SRES_rec)

ggplot(na.omit(age1114)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Premier placement entre 11 et 14 ans") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 2 974 enfants ayant eu premier placement entre entre 11 et 14 ans et sortis de l'établissement d'observation 
       \n au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")

### Pour les 15 ans et + ####
age15 <- subset(sor_red, age_ppd == "15_+")
age15$HEBE_rec3_t <- add_pct(age15$HEBE_rec3)
age15$ARES_rec_t <- add_pct(age15$ARES_rec)
age15$SRES_rec_t <- add_pct(age15$SRES_rec)

ggplot(na.omit(age15)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec3)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Premier placement entre 15 ans et plus") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 3 380 enfants ayant connus un premier placement entre 15 ans et plus et sortis de l'établissement d'observation 
       \n au cours de l'année 2017, sans les mineurs isolés étrangers.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")



## Capacité d'accueil par type d'établissement ####-----------------------------
obs %>%
  tbl_summary(include = c("CATEG_rec", "CAPAA"), 
              by = CATEG_rec,
              missing = "no",
              statistic = list(all_continuous() ~ "{min} / {mean} / {max}")) %>% 
  modify_header(label = " ")

sor$age_e <- sor$ANE-sor$ANN
sorw %>%
  tbl_svysummary(include = c("CATEG_rec", "age_e"), 
                 by = CATEG_rec,
                 missing = "no",
                 statistic = list(all_continuous() ~ "{min} / {mean} / {max}")) %>% 
  modify_header(label = " ")

## Table ARES / SRES ####-----------------------------
require(GGally)
ggtable(data = na.omit(sor), columnsX = "ARES_rec", columnsY = "SRES_rec", cells = "col.prop",
        mapping = aes(weight = poids_sor), fill = "std.resid", legend = 1) +
  labs(fill = "Résidus\ndu Chi²") + 
  coord_flip() + 
  ggtitle("ARES par SRES")

require(gtsummary)
require(huxtable)

## Réordonnancement de sor$SRES_rec
sor$SRES_rec <- sor$SRES_rec %>%
  fct_relevel(
    "Famille", "Assfam", "EtabASEPJJ", "Logtacc", "Logthors", "Autres")
sor$ARES_rec <- sor$ARES_rec %>%
  fct_relevel(
    "Famille", "Assfam", "EtabASEPJJ", "Logtacc", "Logthors", "Autres")

sor$SRES_rec_t <- add_n(sor$SRES_rec)

sorw <- survey::svydesign(ids = ~1, data = sor, weights = ~ sor$poids_sor)

sorw %>%
  tbl_svysummary(include = c("ARES_rec", "SRES_rec_t"), 
              by = ARES_rec,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}% ({n_unweighted})"),
              label = list(SRES_rec_t ~ "Hébergement à la sortie")) %>% 
  modify_header(label = " ") %>% 
  bold_labels() %>% 
  bold_levels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4","stat_5", "stat_6") ~ "**Hébergement avant l'entrée**") %>%
  add_p() %>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 30 412 enfants sortis de l'établissement d'observation au cours de 2017, données pondérées.") %>%
  set_number_format(NA) %>% 
  set_caption("Type d'hébergement avant l'entrée dans l'établissement d'observation et juste à la sortie") %>% 
  quick_xlsx()


chisq.test(table(sor$ARES_rec, sor$SRES_rec))
questionr::chisq.residuals(table(sor$ARES_rec, sor$SRES_rec))


### essai #####-------------------------------------------------------------
source("Code/Recodage.R")

mecs <- subset(sor, CATEG_rec == "MECS")
mecs$HEBE_rec3_t <- add_pct(mecs$HEBE_rec3)
mecs$ARES_rec_t <- add_pct(mecs$ARES_rec)
mecs$SRES_rec_t <- add_pct(mecs$SRES_rec)

ggplot(na.omit(mecs)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec3_t, axis3 = SRES_rec_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)", "Sortie 2017 (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = ARES_rec)) +
  geom_flow() +
  geom_stratum( fill = "white", color = "grey") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Graphique 1 - Rec 1 :Type d'hébergement avant l'entrée et à la sortie de l'établissement d'observation") +
  labs(caption = "Source : ES-PE 2017. \n
Champ : Sur les 30 412 enfants sortis au cours de l'année 2017.", fill = "HEBE :") +
  theme_classic() +
  theme(legend.position = "bottom")


mecs %>%
  tbl_summary(include = c("ARES_rec", "SRES_rec_t"), 
              by = ARES_rec,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = list(SRES_rec_t ~ "Hébergement à la sortie")) %>% 
  modify_header(label = " ") %>% 
  bold_labels() %>% 
  bold_levels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4","stat_5", "stat_6") ~ "**Hébergement avant l'entrée**") %>%
  add_p() %>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 30 412 enfants sortis de l'établissement d'observation au cours de 2017, données pondérées.") %>%
  set_number_format(NA) %>% 
  set_caption("Type d'hébergement avant l'entrée dans l'établissement d'observation et juste à la sortie") %>% 
  quick_xlsx()


essor <- read_sas("Data/Données intactes/ES-DS 2012/SAS/enf_esd.sas7bdat")
esact <- read_sas("Data/Données intactes/ES-DS 2012/SAS/enf_ace.sas7bdat")

## Recodage de essor$HEBE
essor$HEBE <- essor$HEBE %>% fct_recode(
    NULL = "",
    NULL = "0",
    NULL = "C",
    NULL = "H")
essor$HEBE_rec <- recode_hebe3(essor$HEBE)

essor$SRES_rec <- recode_res(essor$SRES)

essor %>%
  tbl_summary(include = c("HEBE_rec", "SRES_rec"), 
              by = HEBE_rec,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = list(SRES_rec ~ "Hébergement à la sortie")) %>% 
  modify_header(label = " ") %>% 
  modify_caption("**ESSOR en 2012**") %>% 
  bold_labels() %>% 
  bold_levels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4") ~ "**HEBE**")

sor %>%
  tbl_summary(include = c("HEBE_rec3", "SRES_rec"), 
              by = HEBE_rec3,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = list(SRES_rec ~ "Hébergement à la sortie")) %>% 
  modify_header(label = " ") %>% 
  modify_caption("**SOR en 2017**") %>% 
  bold_labels() %>% 
  bold_levels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4") ~ "**HEBE**")

essor_mecs <- subset(essor, CAT == "177")

t1 <-  essor_mecs %>%
  tbl_summary(include = c("HEBE_rec", "SRES_rec"), 
              by = HEBE_rec,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = list(SRES_rec ~ "Hébergement à la sortie")) %>% 
  modify_header(label = " ") %>% 
  bold_labels() %>% 
  bold_levels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4") ~ "**HEBE**")
t2 <- mecs %>%
  tbl_summary(include = c("HEBE_rec3", "SRES_rec"), 
              by = HEBE_rec3,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}% ({n})"),
              label = list(SRES_rec ~ "Hébergement à la sortie")) %>% 
  modify_header(label = " ") %>% 
  bold_labels() %>% 
  bold_levels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3", "stat_4") ~ "**HEBE**")

tbl_stack(tbls = list(t1,t2), group_header = c("2012", "2017")) 

essor_mecs$age <- 2012 - as.numeric(essor_mecs$ANN)



essai <- read_sas("Data/ED-DS_enfsor.sas7bdat")

