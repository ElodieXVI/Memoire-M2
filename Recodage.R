###
### Recodage et bases propres :###
###
library(pacman)
p_load(tidyverse, survey, haven, magrittr, lubridate)

act <- read_sas("ES-PE 2017/act.sas7bdat")
enf <- read_sas("ES-PE 2017/enf.sas7bdat")
sor <- read_sas("ES-PE 2017/sor.sas7bdat")
obs <- read_sas("ES-PE 2017/obs.sas7bdat")
per <- read_sas("ES-PE 2017/per.sas7bdat")

#Délimitation des données ####
recode_categ <- function(var) {
  var_recodee <- fct_recode(as.factor(var),
                            "1" = "172",
                            "2" = "175",
                            "3" = "176",
                            "4" = "177",
                            NULL = "236",
                            "5" = "462",
                            NULL = "CEF",
                            NULL = "CER",
                            NULL = "EPE",
                            NULL = "EPI",
                            NULL = "SAH")
  return(var_recodee)
}

per$CATEG <- recode_categ(per$CATEG)
per <- subset(per, as.numeric(CATEG) >=1)

obs$CATEG <- recode_categ(obs$CATEG)
obs$CATEG <- fct_recode(obs$CATEG,
  NULL = "286",
  NULL = "295")
obs <- subset(obs, as.numeric(CATEG) >=1)
obs$CATEG <- obs$CATEG %>%fct_recode(NULL = "")

sor$CATEG <- recode_categ(sor$CATEG)
sor <- subset(sor, as.numeric(CATEG) >=1)

enf$CATEG <- recode_categ(enf$CATEG)
enf <- subset(enf, as.numeric(CATEG) >=1)

act$CATEG <- recode_categ(act$CATEG)
act <- subset(act, as.numeric(CATEG) >=1)

recode_categ_chr <- function(var) {
  var_recodee <- fct_recode(as.character(var),
                            "Pouponnières" = "1",
                            "Foyers" = "2",
                            "Villages" = "3",
                            "MECS" = "4",
                            "Lieux de vie" = "5")
  return(var_recodee)
}

sor$CATEG_rec <- recode_categ_chr(sor$CATEG)
enf$CATEG_rec <- recode_categ_chr(enf$CATEG)
act$CATEG_rec <- recode_categ_chr(act$CATEG)
obs$CATEG_rec <- recode_categ_chr(obs$CATEG)

#Création de nouvelles variables : ######
####Capacité autorisée vs capacité installée ####
obs %<>% mutate(STATUT_REP_r2 = fct_recode(STATUT_REP,
                                           NULL = "Hors-champ"),
                CAP_R = (CAPAA - CAPAI),
                CAP_R_rec = cut(CAP_R,
                                include.lowest = FALSE,
                                right = FALSE,
                                dig.lab = 4,
                                breaks = c(-214, 0, 1, 309)))

obs %<>% mutate(CAP_R_rec = fct_recode(CAP_R_rec,
                                       "Sureffectif" = "[-214,0)",
                                       "Plein" = "[0,1)",
                                       "Souseffectif" = "[1,309)"),
                CAP_R_reg = fct_recode(CAP_R_rec,
                                            "Sur-Souseffectif" = "Sureffectif",
                                            "Sur-Souseffectif" = "Souseffectif"))

####Age enfant et durée de placement####
enf %<>% mutate(age = (2017 - ANN),
                pla_duree = (2017 - ANPP))

sor %<>% mutate(age = (2017 - ANN),
                pla_duree = (2017 - ANPP))

## Recodage personnel mission principale ####
per$FPE <- as.numeric(per$FPE)
per$fonction_ge <- case_when(per$FPE >= 61 ~ "En formation",
                                 per$FPE >= 54 ~ "Psychologue et personnel paramédical",
                                 per$FPE >= 50 ~ "Personnel médical",
                                 per$FPE >= 24 ~ "Personnel éducatif, pédagogique et social",
                                 per$FPE >= 10 ~ "Personnel d'encadrement sanitaire et social",
                                 per$FPE >= 5 ~ "Personnel des services généraux",
                                 per$FPE >= 1 ~ "Personnel de direction")

## récuction des bases ####
names(enf)
enf <- enf[,c(1,3:4,7:9,11:17,19:29)]

names(sor)
sor <- sor[,c("ID","ENQ","CATEG","poids_sor","TOT_SOR","HEBE","SEX","ANN","MOPP","ANPP",     
              "MOE","ANE","MOS","MES","AMES","ARES","SMES","SRES","HAND","PNAIS","MNA",
              "CATEG_rec","age","pla_duree")]

names(per)
per <- per[,c(1,3:5,8:11, 16)]

## Réunion de deux bases enfants sor et enf ####
act2 <- act %>%  select(ID, REPSEX)
enf <- merge(enf, act2)
sor <- merge(sor, act2)

#La seule chose qui manque dans sor est l'occupation de l'enfant, 
#mais aussi DHEB droit hébergement et la classe suivie
sor2 <- sor[,c("ID","CATEG","poids_sor","HEBE","SEX","ANN","MOPP","ANPP",     
               "MOE","ANE","MES","AMES","ARES","HAND","PNAIS","MNA",
               "CATEG_rec","age","pla_duree","REPSEX")] 
enf2 <- enf[,c("ID","CATEG","poids_enf","HEBE","SEX",      
               "ANN","MOPP","ANPP","MOE","ANE","AMES","ARES",     
               "MES", "HAND","PNAIS","MNA","CATEG_rec","age","pla_duree","REPSEX")]

sor2$enq <- "sor"
enf2$enq <- "enf"

sor2 <- rename(sor2, poids = poids_sor)
enf2 <- rename(enf2, poids = poids_enf)
ens <- rbind(enf2, sor2)

sor2 <- NULL
enf2 <- NULL
act2 <- NULL

## Normalisation de la pondération
#mean(ens$poids, na.rm = T)
ens$poids <- (ens$poids/1.715083)

## Recodage de enf$HEBE en enf$HEBE_rec
recode_hebe1 <- function(var) {
  var <- as.numeric(var)
  var_rec <- fct_recode(as.character(var),
                        "Collect" = "1",
                        "Autonom" = "2",
                        "Hotelautre" = "3",
                        "Assfam" = "4",
                        "Collect" = "5",
                        "Assfam" = "6",
                        "Domicile" = "7",
                        "Hotelautre" = "8")
  return(var_rec)
}
enf$HEBE_rec1 <- recode_hebe1(enf$HEBE)
sor$HEBE_rec1 <- recode_hebe1(sor$HEBE)
ens$HEBE_rec1 <- recode_hebe1(ens$HEBE)

recode_hebe2 <- function(var) {
  var <- as.numeric(var)
  var_rec <- fct_recode(as.character(var),
                        "Collect" = "1",
                        "Autonomautre" = "2",
                        "Autonomautre" = "3",
                        "Assfam_dom" = "4",
                        "Collect" = "5",
                        "Assfam_dom" = "6",
                        "Assfam_dom" = "7",
                        "Autonomautre" = "8")
  return(var_rec)
}

enf$HEBE_rec2 <- recode_hebe2(enf$HEBE)
sor$HEBE_rec2 <- recode_hebe2(sor$HEBE)
ens$HEBE_rec2 <- recode_hebe2(ens$HEBE)

recode_hebe3 <- function(var) {
  var_rec <- fct_recode(as.character(var),
                        "Collect" = "1",
                        "Autonomautre" = "2",
                        "Autonomautre" = "3",
                        "Assfam" = "4",
                        "Collect" = "5",
                        "Assfam" = "6",
                        "Domicile" = "7",
                        "Autonomautre" = "8")
  print(table(var_rec, useNA = "ifany"))
  return(var_rec)
}
enf$HEBE_rec3 <- recode_hebe3(enf$HEBE)
sor$HEBE_rec3 <- recode_hebe3(sor$HEBE)
ens$HEBE_rec3 <- recode_hebe3(ens$HEBE)

## Recodage RES
recode_res <- function(var) {
  var <- as.numeric(var)
  var_rec <- fct_recode(as.character(var),
                        "Famille" = "1",
                        "Famille" = "2",
                        "Logtacc" = "3",
                        "Logthors" = "4",
                        "Logthors" = "5",
                        "Logtacc" = "6",
                        "EtabASEPJJ" = "7",
                        "EtabASEPJJ" = "8",
                        "EtabASEPJJ" = "9",
                        "Assfam" = "10",
                        "Logthors" = "11",
                        "Logthors" = "12",
                        "Logthors" = "13",
                        "Logtacc" = "14", 
                        NULL = "15")
  print(table(var_rec, useNA = "ifany"))
  return(var_rec)
}
sor$ARES_rec <- recode_res(sor$ARES)
sor$SRES_rec <- recode_res(sor$SRES)

enf$ARES_rec <- recode_res(enf$ARES)

## Recodage de sor$SEX
sor$SEX <- as.character(sor$SEX)
sor$SEX %<>% fct_recode("Homme" = "1","Femme" = "2")

enf$SEX <- as.character(enf$SEX)
enf$SEX %<>% fct_recode("Homme" = "1","Femme" = "2")

ens$SEX <- as.character(ens$SEX)
ens$SEX %<>% fct_recode("Homme" = "1","Femme" = "2")

enf$SEX %<>% fct_relevel("Femme", "Homme")
sor$SEX %<>% fct_relevel("Femme", "Homme")
ens$SEX %<>% fct_relevel("Femme", "Homme")

enf$REPSEX <- as.factor(enf$REPSEX)
enf$REPSEX %<>%fct_recode("Non" = "0","Oui" = "1")

sor$REPSEX <- as.factor(sor$REPSEX)
sor$REPSEX %<>%fct_recode("Non" = "0","Oui" = "1")

ens$REPSEX <- as.factor(ens$REPSEX)
ens$REPSEX %<>%fct_recode("Non" = "0","Oui" = "1")


# Dates ####

sor %<>% mutate(d_anpp = dmy(paste0("1/",MOPP,"/",ANPP)),
                  d_ane = dmy(paste0("1/",MOE,"/",ANE)),
                  d_ans = dmy(paste0("1/",MOS,"/2017")),
                  d_ann = dmy(paste0("1/6/",ANN)))

sor %<>% mutate(dur_pla = as.numeric(as.period(interval(d_anpp, d_ans)), "months"),
                dur_dpla = as.numeric(as.period(interval(d_ane, d_ans)), "months"),
                mois_e = as.numeric(as.period(interval(d_ann, d_ane)), "months"),
                dur_av_dpla = as.numeric(as.period(interval(d_anpp, d_ane)), "months"),
                dur_av_ppla = as.numeric(as.period(interval(d_ann, d_anpp)), "months"))

enf %<>%  mutate(d_anpp = dmy(paste0("1/",MOPP,"/",ANPP)),
                 d_ane = dmy(paste0("1/",MOE,"/",ANE)),
                 d_ann = dmy(paste0("1/6/",ANN)),
                 annee = dmy(paste0("15/12/2017")))

enf %<>% mutate(dur_dpla = as.numeric(as.period(interval(d_ane, annee)), "months"),
                dur_av_dpla = as.numeric(as.period(interval(d_anpp, d_ane)), "months"))

# Création d'une variable id ####
sor$id <- rownames(sor)
enf$id <- rownames(enf)

# Recodage MES ####
recode_mes1 <- function(var) {
  var_rec <- fct_recode(var, 
                        "Mesure administrative Mineur" = "02",
                        "Mesure administrative Jeune Majeur" = "03",
                        "Judiciaire confié" = "04",
                        "Judiciaire direct juge" = "09",
                        "Judiciaire direct juge" = "10",
                        "Judiciaire direct juge" = "11",
                        "Judiciaire direct juge" = "12",
                        "Judiciaire direct juge" = "13",
                        "Milieu ouvert" = "14",
                        "Milieu ouvert" = "15",
                        "Milieu ouvert" = "16",
                        "Mesure touchant la responsabilité parentale" = "01",
                        "Mesure touchant la responsabilité parentale" = "17",
                        "Mesure touchant la responsabilité parentale" = "05",
                        "Mesure touchant la responsabilité parentale" = "06",
                        "Mesure touchant la responsabilité parentale" = "07",
                        "Mesure touchant la responsabilité parentale" = "08",
                        "Autre" = "18",
                        "Autre" = "19",
                        "NA" = "")
return(var_rec)
}

recode_mes2 <- function(var) {
  var <- as.factor(var)
  var_rec <- fct_recode(var, 
                        "Mes admin Min" = "02",
                        "Mes admin Maj" = "03",
                        "Judi conf" = "04",
                        "Judi direct" = "09",
                        "Judi direct" = "10",
                        "Judi direct" = "11",
                        "Judi direct" = "12",
                        "Judi direct" = "13",
                        "MO" = "14",
                        "MO" = "15",
                        "MO" = "16",
                        "Mes respo par" = "01",
                        "Mes respo par" = "17",
                        "Mes respo par" = "05",
                        "Mes respo par" = "06",
                        "Mes respo par" = "07",
                        "Mes respo par" = "08",
                        "Autre" = "18",
                        "Autre" = "19",
                        "NA" = "")
  return(var_rec)
}

sor$MES_rec1 <- recode_mes1(sor$MES)
enf$MES_rec1 <- recode_mes1(enf$MES)
ens$MES_rec <- recode_mes1(ens$MES)

sor$AMES_rec1 <- recode_mes1(sor$AMES)
enf$AMES_rec1 <- recode_mes1(enf$AMES)
ens$AMES_rec1 <- recode_mes1(ens$AMES)

sor$SMES_rec1 <- recode_mes1(sor$SMES)

sor$MES_rec <- recode_mes1(sor$MES)
enf$MES_rec <- recode_mes1(enf$MES)
ens$MES_rec <- recode_mes1(ens$MES)

sor$AMES_rec <- recode_mes1(sor$AMES)
enf$AMES_rec <- recode_mes1(enf$AMES)
ens$AMES_rec <- recode_mes1(ens$AMES)

sor$SMES_rec <- recode_mes1(sor$SMES)

## Age par classe ####

recode_age <- function(var) {
  print(table(var, useNA = "ifany"))
  var_rec <- cut(var,
                 include.lowest = TRUE,
                 right = FALSE,
                 dig.lab = 4,
                 breaks = c(0, 4, 7, 13, 15, 18, 27)
  )
  var_rec <- var_rec %>%
    fct_recode("0_3" = "[0,4)", "4_6" = "[4,7)", "7_12" = "[7,13)", "13_14" = "[13,15)", "15_17" = "[15,18)", "18_+" = "[18,27]")
  print(table(var_rec, useNA = "ifany"))
  return(var_rec)
}

# âge entrée 
sor$age_e <- sor$ANE - sor$ANN
sor$age_ed <- recode_age(sor$age_e)

enf$age_e <- enf$ANE - enf$ANN
enf$age_ed <- recode_age(enf$age_e)


# âge sortie 
sor$ANS <- 2017
sor$age_s <- sor$ANS - sor$ANN
sor$age_sd <- recode_age(sor$age_s)

# âge 1er placement
sor$age_pp <- sor$ANPP - sor$ANN
sor$age_ppd <- recode_age(sor$age_pp)

enf$age_pp <- enf$ANPP - enf$ANN
enf$age_ppd <- recode_age(enf$age_pp)


# Normalisation de la pondération 

enf$poids2 <- enf$poids_enf/mean(enf$poids_enf)
sor$poids2 <- sor$poids_sor/mean(sor$poids_sor)

# Pondération ####
sorw <- svydesign(ids = ~1, data = sor, weights = ~ sor$poids2)
enfw <- svydesign(ids = ~1, data = enf, weights = ~ enf$poids2)
ensw <- svydesign(ids = ~ens$enq, data = ens, weights = ~ ens$poids)
