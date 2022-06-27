## ACM
library(pacman)
p_load(tidyverse, questionr, FactoMineR, Factoshiny, survey, missMDA, knitr, 
       explor, webshot, gtsummary, gt, survey, factoextra, magrittr,
       MetBrewer)

source("Code/Recodage.R")

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

sor$MES_rec2 <- recode_mes2(sor$MES)
sor$AMES_rec2 <- recode_mes2(sor$AMES)
sor$SMES_rec2 <- recode_mes2(sor$SMES)

s_acm <- sor %>% subset(CATEG_rec == "MECS") %>% 
  select("SEX","HEBE_rec1", "HEBE_rec3", "HAND", "MNA", "MES_rec2", 
         "AMES_rec2", "SMES_rec2", "age_ed","ARES_rec", "SRES_rec")%>% 
  drop_na()

table(s_acm$HAND)
s_acm %<>% mutate(
  HAND = case_when(
    HAND == 1 ~ "hand_O",
    HAND == 0 ~ "hand_N"))
s_acm$HAND <- as.factor(s_acm$HAND)

table(s_acm$MNA)
s_acm %<>% mutate(
  MNA = case_when(
    MNA == 1 ~ "mna_O",
    MNA == 0 ~ "mna_N"))
s_acm$MNA <- as.factor(s_acm$MNA)

levels(s_acm$HEBE_rec1) <- paste0("hebe_",levels(s_acm$HEBE_rec1))
table(s_acm$HEBE_rec1)
levels(s_acm$HEBE_rec3) <- paste0("hebe_",levels(s_acm$HEBE_rec3))
table(s_acm$HEBE_rec3)


levels(s_acm$MES_rec2) <- paste0("mes_",levels(s_acm$MES_rec2))
table(s_acm$MES_rec2)

levels(s_acm$AMES_rec2) <- paste0("ames_",levels(s_acm$AMES_rec2))
table(s_acm$AMES_rec2)

levels(s_acm$SMES_rec2) <- paste0("smes_",levels(s_acm$SMES_rec2))
table(s_acm$SMES_rec2)

levels(s_acm$ARES_rec) <- paste0("ares_",levels(s_acm$ARES_rec))
table(s_acm$ARES_rec)

levels(s_acm$SRES_rec) <- paste0("sres_",levels(s_acm$SRES_rec))
table(s_acm$SRES_rec)


var_activ  <- s_acm[,c("ARES_rec", "HEBE_rec1", "SRES_rec")]
var_illustrativ  <- s_acm[,c("SEX","age_ed","HAND","MNA")]

baseACM <- cbind.data.frame(var_activ, var_illustrativ)
summary(baseACM)

Factoshiny(baseACM)

res.MCA<-MCA(baseACM,quali.sup=c(4,5,6,7),graph=FALSE)
plot.MCA(res.MCA, choix='var',title="Graphe des variables")
plot.MCA(res.MCA,invisible= 'ind',title="Graphe de l'ACM",label =c('var','quali.sup'))

g1 <- plot.MCA(res.MCA,invisible= 'ind',
               title="Espace des types d'hébergements en MECS",
               autoLab = "yes",
               graph.type = "ggplot",
               col.var = "#4c3b7f", col.quali.sup = "#88a0dc")
g1  + labs(caption="Source : Enquête ES-PE 2017, DREES \n Champ : Sur les 10 522 enfants sortis de MECS au cours de 2017.")+
  theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"), axis.text.x = element_blank())

g2 <- plot.MCA(res.MCA,invisible= c('ind', 'quali.sup'),
               title="Espace des types d'hébergements en MECS",
               autoLab = "yes",
               graph.type = "ggplot",
               col.var = "#4c3b7f", col.quali.sup = "#88a0dc")
g2  + labs(caption="Source : Enquête ES-PE 2017, DREES \n Champ : Sur les 10 522 enfants sortis de MECS au cours de 2017.")+
  theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"), axis.text.x = element_blank())




res.MCA<-MCA(baseACM,ncp=2,quali.sup=c(4,5,6,7,8,9,10),graph=FALSE)
res.HCPC<-HCPC(res.MCA,nb.clust=3,consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC, choice = "tree")

s_acm$grp <- res.HCPC$data.clust$clust

cprop(table(s_acm$SEX,s_acm$grp))
cprop(table(s_acm$age_ed,s_acm$grp))
cprop(table(s_acm$HEBE_rec1,s_acm$grp))


explor(res.MCA)
res <- explor::prepare_results(res.MCA)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, 
                     var_sup_choice = c("SEX","age_ed", "HAND", "MNA", "AMES_rec2", "MES_rec2", "SMES_rec2"), 
                     var_lab_min_contrib = 0,
                     col_var = 1, symbol_var = "Type", size_var = NULL, 
                     size_range = c(10,300), labels_size = 12, point_size = 56, 
                     transitions = TRUE,
                     labels_prepend_var = FALSE, xlim = c(-1.53, 2.46), ylim = c(-1.35, 2.64), 
                     colors = met.brewer("Tiepolo", 9),
                     labels_positions = "auto")





