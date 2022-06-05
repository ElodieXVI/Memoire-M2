# Flux de sortie de MECS et d'entrée en MECS ####

library(pacman)
p_load(ggalluvial, MetBrewer, tidyverse,extrafont)
source("Recodage.R")
source("Fonctions.R")

## Tout d'abord sur enf ####------------------

e_mecs <- subset(enf, CATEG_rec == "MECS")

e_mecs$ARES_rec <- as.factor(e_mecs$ARES_rec)

e_mecs$HEBE_rec1_t <- add_pct(e_mecs$HEBE_rec1)
e_mecs$HEBE_rec3_t <- add_pct(e_mecs$HEBE_rec3)
e_mecs$ARES_rec_t <- add_pct(e_mecs$ARES_rec)

f1 <- ggplot(na.omit(e_mecs)) +
  aes(axis1 = ARES_rec_t, axis2 = HEBE_rec1_t) +
  scale_x_discrete(limits = c("Avant entrée (ARES)", "Entrée dans l'établissement (HEBE)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = ARES_rec)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = met.brewer("Klimt")) +
  ggtitle("Graphique - Type d'hébergement avant l'entrée et à l'entrée en MECS") +
  labs(caption = "Source : Enquête ES-PE 2017, DREES. \n
Champ : Sur les 25 525 enfants présents en MECS le 15/12/2017.", fill = "Type d'hébergement avant MECS:") +
  theme_bw() +
  theme(legend.position = "bottom")

f1 + theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"))


## Tout d'abord sur sor ####------------------

s_mecs <- subset(sor, CATEG_rec == "MECS")

s_mecs$ARES_rec <- as.factor(s_mecs$ARES_rec)

s_mecs$HEBE_rec1_t <- add_pct(s_mecs$HEBE_rec1)
s_mecs$HEBE_rec3_t <- add_pct(s_mecs$HEBE_rec3)
s_mecs$SRES_rec_t <- add_pct(s_mecs$SRES_rec)

f1 <- ggplot(na.omit(s_mecs)) +
  aes(axis1 = HEBE_rec1_t, axis2 = SRES_rec_t) +
  scale_x_discrete(limits = c("En MECS (HEBE)", "Sortie de MECS (SRES)"), expand = c(.1, .05)) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  geom_alluvium(aes(fill = HEBE_rec1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values = met.brewer("Klimt")) +
  ggtitle("Graphique - Type d'hébergement en MECS et à la sortie de MECS") +
  labs(caption = "Source : Enquête ES-PE 2017, DREES. \n
Champ : Sur les 15 777 enfants sortis de MECS en 2017.", fill = "Type d'hébergement en MECS :") +
  theme_bw() +
  theme(legend.position = "bottom")

f1 + theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"))

