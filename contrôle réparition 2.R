library(pacman)
p_load(tidyverse, questionr, FactoMineR, Factoshiny, survey, missMDA, knitr, 
       explor, webshot, gtsummary, gt, survey, factoextra, magrittr,GGally,wesanderson,
       MetBrewer, extrafont, huxtable)


# Répartition par sexe -----

repsex <- subset(act, REPSEX == "1")
ex1 <- repsex$ID

repsex2 <- subset(act, REPSEX == "0")
ex2 <- repsex2$ID

enf$repsex <- case_when(enf$ID %in% ex1 ~ "Oui",
                        enf$ID %in% ex2 ~ "Non",
                        T ~ "NA")

enfw <- svydesign(ids = ~1, data = enf, weights = ~ enf$poids2)

t1 <-  enfw %>% subset(repsex == "Oui") %>% 
  tbl_svysummary(include = c(HEBE_rec3, SEX),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_categorical() ~ "{p}"), 
                               label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  enfw %>% subset(repsex == "Non") %>%
  tbl_summary(include = c(HEBE_rec3, SEX),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_categorical() ~ "{p}"), 
                               label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()


t1 <-  enfw %>% subset(repsex == "Oui") %>% 
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, SEX),
                 by = HEBE_rec3,
                 missing = "no",
                 statistic = list(all_categorical() ~ "{p}"), 
                 label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  enfw %>% subset(repsex == "Non") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_summary(include = c(HEBE_rec3, SEX),
              by = HEBE_rec3,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}"), 
              label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

enf %>% subset(CATEG_rec == "MECS") %$% 
  table(repsex)

enf %>% subset(CATEG_rec == "MECS") %$% 
  table(HEBE_rec3)

tbl_stack(tbls=list(t1,t2), group_header = list("oui", "non"))


## Répartition par âge -------

repage <- subset(act, REPAGE == "1")
ex1 <- repage$ID

repagen <- subset(act, REPAGE == "0")
ex2 <- repagen$ID

enf$repage <- case_when(enf$ID %in% ex1 ~ "Oui",
                        enf$ID %in% ex2 ~ "Non",
                        T ~ "NA")

enfw <- svydesign(ids = ~1, data = enf, weights = ~ enf$poids2)

t1 <-  enfw %>% subset(repage == "Oui") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, age_ed),
              by = HEBE_rec3,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  enfw %>% subset(repage == "Non") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, age_ed),
              by = HEBE_rec3,
              missing = "no",
              statistic = list(all_categorical() ~ "{p}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2), group_header= list("Oui", "Non"))

enf %>% subset(CATEG_rec == "MECS") %$% 
  table(repage)


# Répartition par civil / pénal ----

table(enf$MES)
enf$civpen <- enf$MES %>%
  fct_recode(
    "Administrative" = "01",
    "Administrative" = "02",
    "Administrative" = "03",
    "Civil" = "04",
    "Civil" = "05",
    "Civil" = "06",
    "Civil" = "07",
    "Civil" = "08",
    "Civil" = "09",
    "Civil" = "10",
    "Civil" = "11",
    "Pénal" = "12",
    "Pénal" = "13",
    "Civil" = "14",
    "Administrative" = "15",
    "Pénal" = "16",
    "Autre" = "17",
    "Autre" = "18",
    "Autre" = "19")

repciv <- subset(act, REPCIV == "1")
ex1 <- repciv$ID

repcivn <- subset(act, REPCIV == "0")
ex2 <- recivn$ID

enf$repciv <- case_when(enf$ID %in% ex1 ~ "Oui",
                        enf$ID %in% ex2 ~ "Non",
                        T ~ "NA")

enfw <- svydesign(ids = ~1, data = enf, weights = ~ enf$poids2)

t1 <-  enfw %>% subset(repciv == "Oui") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, civpen),
                 by = HEBE_rec3,
                 missing = "no",
                 statistic = list(all_categorical() ~ "{n}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  enfw %>% subset(repciv == "Non") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, civpen),
                 by = HEBE_rec3,
                 missing = "no",
                 statistic = list(all_categorical() ~ "{n}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2), group_header= list("Oui", "Non"))

enf %>% subset(CATEG_rec == "MECS") %$% 
  table(repciv)
