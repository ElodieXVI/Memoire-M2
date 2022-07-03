# profils des enfatns par type d'hébergement


e_mecsw  %>% tbl_svysummary(include = c(SEX, age_ed, MNA,HAND),
                        by = HEBE_rec3,
                        percent = "col", 
                        statistic = list(all_categorical() ~ "{p}"),
                        missing = "no") %>%   
  modify_header(label = "") %>% 
  modify_header(all_stat_cols() ~ "**{level}**, N = {n_unweighted}") %>% 
  bold_labels()

s_mecsw  %>% tbl_svysummary(include = c(SEX, age_ed, MNA,HAND),
                            by = HEBE_rec3,
                            percent = "col", 
                            statistic = list(all_categorical() ~ "{p}"),
                            missing = "no") %>%   
  modify_header(label = "") %>% 
  modify_header(all_stat_cols() ~ "**{level}**, N = {n_unweighted}") %>% 
  bold_labels()


e_mecs$ppla <- case_when(e_mecs$d_anpp == e_mecs$d_ane ~ "Mecs premier placement",
                         T ~"MECS pas premier placement")

freq(e_mecs$ppla)
e_mecsw <- svydesign(ids = ~1, data = e_mecs, weights = ~ e_mecs$poids2)

freq(svytable(~ppla, e_mecsw))
cprop(svytable(~HEBE_rec3+ppla, e_mecsw))
cprop(table(e_mecs$HEBE_rec3, e_mecs$ppla))
cprop(table(e_mecs$HEBE_rec3, e_mecs$ppla))
chisq.test(table(e_mecs$HEBE_rec3, e_mecs$ppla))
chisq.residuals(table(e_mecs$HEBE_rec3, e_mecs$ppla))
cprop(table(e_mecs$ARES_rec, e_mecs$ppla))

cprop(svytable(~ARES_rec+ppla, e_mecsw))

e_mecsw  %>% tbl_svysummary(include = c(ppla,HEBE_rec3),
                            by = ppla,
                            percent = "col", 
                            statistic = list(all_categorical() ~ "{p}"),
                            missing = "no") %>%   
  modify_header(label = "") %>% 
  modify_header(all_stat_cols() ~ "**{level}**, N = {n_unweighted}") %>% 
  bold_labels()
