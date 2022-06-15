## ACM
library(pacman)
p_load(tidyverse, questionr, FactoMineR, Factoshiny, survey, missMDA, knitr, 
       explor, webshot, gtsummary, gt, survey, factoextra, magrittr,
       MetBrewer, extrafont)

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

enf$MES_rec2 <- recode_mes2(enf$MES)
enf$AMES_rec2 <- recode_mes2(enf$AMES)

e_acm <- enf %>% subset(CATEG_rec == "MECS") %>% 
  select("SEX","HEBE_rec1", "HEBE_rec3", "HAND", "MNA", "MES_rec2", 
         "AMES_rec2",  "age_ed","ARES_rec", "SEQ")%>% 
  drop_na() #Sur 18 440 enfants présents

table(e_acm$HAND)
e_acm %<>% mutate(
  HAND = case_when(
    HAND == 1 ~ "hand_O",
    HAND == 0 ~ "hand_N"))
e_acm$HAND <- as.factor(e_acm$HAND)

table(e_acm$MNA)
e_acm %<>% mutate(
  MNA = case_when(
    MNA == 1 ~ "mna_O",
    MNA == 0 ~ "mna_N"))
e_acm$MNA <- as.factor(e_acm$MNA)

levels(e_acm$HEBE_rec1) <- paste0("hebe_",levels(e_acm$HEBE_rec1))
table(e_acm$HEBE_rec1)
levels(e_acm$HEBE_rec3) <- paste0("hebe_",levels(e_acm$HEBE_rec3))
table(e_acm$HEBE_rec3)


levels(e_acm$MES_rec2) <- paste0("mes_",levels(e_acm$MES_rec2))
table(e_acm$MES_rec2)

levels(e_acm$AMES_rec2) <- paste0("ames_",levels(e_acm$AMES_rec2))
table(e_acm$AMES_rec2)

levels(e_acm$ARES_rec) <- paste0("ares_",levels(e_acm$ARES_rec))

table(e_acm$ARES_rec)

e_acm$SEQ_rec <- e_acm$SEQ %>%
  as.character() %>%
  fct_recode(
    "seq_comp" = "1",
    "seq_séquen" = "2",
    "seq_div" = "3")


var_activ  <- e_acm[,c("ARES_rec", "HEBE_rec1", "SEQ_rec")]
var_illustrativ  <- e_acm[,c("SEX","age_ed","HAND","MNA")]

baseACM <- cbind.data.frame(var_activ, var_illustrativ)
summary(baseACM)

Factoshiny(baseACM)
explor(res.MCA)
res.MCA<-MCA(baseACM,quali.sup=c(4,5,6,7),graph=FALSE)
plot.MCA(res.MCA, choix='var',title="Graphe des variables")
g1 <- plot.MCA(res.MCA,invisible= 'ind',
               title="Espace des types d'hébergements en MECS",
               autoLab = "yes",
               graph.type = "ggplot",
               col.var = "#4c3b7f", col.quali.sup = "#88a0dc")
g1  + labs(caption="Source : Enquête ES-PE 2017, DREES \n Champ : Sur les 18 440 enfants présents en MECS en 2017.")+
  theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"), axis.text.x = element_blank())

g2 <- plot.MCA(res.MCA,invisible= c('ind', 'quali.sup'),
               title="Espace des types d'hébergements en MECS",
               autoLab = "yes",
               graph.type = "ggplot",
               col.var = "#4c3b7f", col.quali.sup = "#88a0dc")
g2  + labs(caption="Source : Enquête ES-PE 2017, DREES \n Champ : Sur les 18 440 enfants présents en MECS en 2017.")+
  theme(text = element_text(family = "Times"), plot.title = element_text(face = "bold"), axis.text.x = element_blank())

res.MCA<-MCA(baseACM,ncp=2,quali.sup=c(4,5,6,7),graph=FALSE)
res.HCPC<-HCPC(res.MCA,nb.clust=3,kk=100,consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC, choice = "tree")

e_acm$grp <- res.HCPC$data.clust$clust

cprop(table(e_acm$SEX,e_acm$grp))
cprop(table(e_acm$age_ed,e_acm$grp))
cprop(table(e_acm$HEBE_rec1,e_acm$grp))
cprop(table(e_acm$ARES_rec,e_acm$grp))

e_acm %>% tbl_summary(include = c(grp, SEX, age_ed, HEBE_rec1, ARES_rec),
                    by = grp,
                    percent = "col", 
                    statistic = list(all_categorical() ~ "{p} \n({n})")) %>%   
  modify_header(label = " ") %>% 
  bold_labels() %>% 
  modify_spanning_header(c("stat_1", "stat_2","stat_3") ~ "**Groupe de la classification**") %>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 18 440 enfants présents en MECS au 12/12/2017.") %>%
  set_number_format(NA) %>% 
  set_caption("Résultats de la classification") %>% 
  quick_xlsx()


explor(res.MCA)

res <- explor::prepare_results(res.MCA)
g1 <- MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_lab_min_contrib = 0, 
             col_var = "Variable",
             symbol_var = "Type", size_var = NULL, size_range = c(10, 300), labels_size = 16,
             point_size = 56, transitions = TRUE, labels_prepend_var = FALSE,
             xlim = c(-2.09, 2.96), ylim = c(-1.83, 3.22),
             colors = met.brewer("Archambault", 9),
             labels_positions = "auto")

as.ggplot(g1)

MCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, 
             var_sup_choice = c("SEX","age_ed", "HAND", "MNA", "SEQ_rec"), 
             var_lab_min_contrib = 0, col_var = "Variable",
             symbol_var = "Type", size_var = NULL, size_range = c(10, 300), labels_size = 14,
             point_size = 56, transitions = TRUE, labels_prepend_var = FALSE,
             xlim = c(-2.09, 2.96), ylim = c(-1.83, 3.22),
             colors = met.brewer("Archambault", 9),
             labels_positions = "auto")



