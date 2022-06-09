library(pacman)
p_load(tidyverse, questionr, FactoMineR, Factoshiny, survey, missMDA, knitr, 
       explor, webshot, gtsummary, gt, survey, factoextra, magrittr,GGally,wesanderson,
       MetBrewer, extrafont, huxtable)

repsex <- subset(act, REPSEX == "1")
ex1 <- repsex$ID
repsexO <- enf[enf$ID %in% ex1, ]    


repsexn <- subset(act, REPSEX == "0")
ex2 <- repsexn$ID
repsexN <- enf[enf$ID %in% ex2, ] 

t1 <-  repsexO %>% tbl_summary(include = c(HEBE_rec3, SEX),
                        by = HEBE_rec3,
                        missing = "no",
                        statistic = list(all_categorical() ~ "{p}"), 
                        label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  repsexN %>% tbl_summary(include = c(HEBE_rec3, SEX),
                                by = HEBE_rec3,
                                missing = "no",
                                statistic = list(all_categorical() ~ "{p}"), 
                               label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2), group_header = list("oui", "non"))

m_repsexO <-  subset(repsexO, CATEG_rec == "MECS")
m_repsexN <-  subset(repsexN, CATEG_rec == "MECS")

t3 <-  m_repsexO %>% tbl_summary(include = c(HEBE_rec3, SEX),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_categorical() ~ "{p}"), 
                               label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()


t4 <-  m_repsexN %>% tbl_summary(include = c(HEBE_rec3, SEX),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_categorical() ~ "{p}"), 
                               label = list(SEX ~ " ")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2, t3,t4), group_header = list("Oui", "Non","Oui", "Non")) %>% 
  bold_labels()


tbl_stack(tbls=list(t3,t4), group_header = list("Oui", "Non")) %>% 
  bold_labels()




repdur <- subset(act, REPDUR == "1")
ex1 <- repdur$ID
repdurO <- enf[enf$ID %in% ex1, ]    


repdurn <- subset(act, REPDUR == "0")
ex2 <- repdurn$ID
repdurN <- enf[enf$ID %in% ex2, ] 

t1 <-  repdurO %>% tbl_summary(include = c(HEBE_rec3, dur_av_dpla),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_continuous() ~ "{mean}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  repdurN %>% tbl_summary(include = c(HEBE_rec3, dur_av_dpla),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_continuous() ~ "{mean}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2), group_header= list("Oui", "Non"))

m_repdurO <-  subset(repdurO, CATEG_rec == "MECS")
m_repdurN <-  subset(repdurN, CATEG_rec == "MECS")

t3 <-  m_repdurO %>% tbl_summary(include = c(HEBE_rec3, dur_av_dpla),
                                 by = HEBE_rec3,
                                 missing = "no",
                                 statistic = list(all_continuous() ~ "{mean}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()


t4 <-  m_repdurN %>% tbl_summary(include = c(HEBE_rec3, dur_av_dpla),
                                 by = HEBE_rec3,
                                 missing = "no",
                                 statistic = list(all_continuous() ~ "{mean}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2, t3,t4), group_header= list("Oui", "Non", "Oui", "Non"))








repage <- subset(act, REPAGE == "1")
ex1 <- repage$ID
repageO <- enf[enf$ID %in% ex1, ]    


repagen <- subset(act, REPAGE == "0")
ex2 <- repagen$ID
repageN <- enf[enf$ID %in% ex2, ] 

t1 <-  repageO %>% tbl_summary(include = c(HEBE_rec3, age_ed),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_categorical() ~ "{p}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  repageN %>% tbl_summary(include = c(HEBE_rec3, age_ed),
                               by = HEBE_rec3,
                               missing = "no",
                               statistic = list(all_categorical() ~ "{p}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2), group_header= list("Oui", "Non"))


repdur <- subset(act, REPDUR == "1")
ex1 <- repdur$ID

repdur2 <- subset(act, REPDUR == "0")
ex2 <- repdur2$ID


enf$repdur <- case_when(enf$ID %in% ex1 ~ "Oui",
                        enf$ID %in% ex2 ~ "Non",
                        T ~ "NA")

enfw <- svydesign(ids = ~1, data = enf, weights = ~ enf$poids2)

t1 <-   enfw %>% subset(repdur == "Oui") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, dur_av_dpla),
                 by = HEBE_rec3,
                 missing = "no",
                 statistic = list(all_continuous() ~ "{mean}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

t2 <-  enfw %>% subset(repdur == "Non") %>%
  subset(CATEG_rec == "MECS") %>% 
  tbl_svysummary(include = c(HEBE_rec3, dur_av_dpla),
                 by = HEBE_rec3,
                 missing = "no",
                 statistic = list(all_continuous() ~ "{mean}")) %>%   
  modify_header(label = "") %>% 
  bold_labels()

tbl_stack(tbls=list(t1,t2), group_header= list("Oui", "Non"))




