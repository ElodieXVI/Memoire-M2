# Vérification pour ACM réduction popu ####

## ACM enf
freq(e_mecs$SEX)
freq(e_acm$SEX)

freq(e_mecs$age_ed)
freq(e_acm$age_ed)

freq(e_mecs$HEBE_rec3, exclude = NA)
freq(e_acm$HEBE_rec3)

freq(e_mecs$ARES_rec, exclude = NA)
freq(e_acm$ARES_rec)



freq(wtd.table(e_mecs$HEBE_rec3, weights = e_mecs$poids2), exclude = NA)

e_acm$poids2<- e_acm$poids_enf/mean(e_acm$poids_enf)
freq(wtd.table(e_acm$HEBE_rec3, weights = e_acm$poids2))

## ACM sor
freq(s_mecs$SEX, exclude = NA)
freq(s_acm$SEX)

freq(s_mecs$age_ed, exclude = NA)
freq(s_acm$age_ed)

freq(s_mecs$HEBE_rec3, exclude = NA)
freq(s_acm$HEBE_rec3)

freq(s_mecs$ARES_rec, exclude = NA)
freq(s_acm$ARES_rec)



freq(wtd.table(s_mecs$HEBE_rec3, weights = s_mecs$poids2), exclude = NA)

e_acm$poids2<- e_acm$poids_enf/mean(e_acm$poids_enf)
freq(wtd.table(e_acm$HEBE_rec3, weights = e_acm$poids2))
