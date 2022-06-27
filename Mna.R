# MNA ####

```{r mna1, include = FALSE}
mna_ens <- subset(ens, MNA == "1")
mna_enf <- subset(enf, MNA == "1")
mna_sor <- subset(sor, MNA == "1")

mna_ens$poids2 <- (mna_ens$poids/0.9713142)

mna_ensw <- svydesign(ids = ~1, data = mna_ens, weights = ~ mna_ens$poids2)

mna_ensw %>% 
  tbl_svysummary(include = c("SEX", "age","CATEG_rec","HEBE_rec", "enq"), 
                 by = enq,
                 missing = "no",
                 label =list(CATEG_rec ~ "Type d'établissement", 
                             SEX ~ "Sexe",
                             age ~ "Âge", 
                             HEBE_rec ~ "Type d'hébergement"),
                 statistic = list(all_categorical() ~ "{p}% ({n_unweighted})",
                                  all_continuous()~ "{mean}")) %>% 
  bold_labels()%>%
  as_hux_table() %>% 
  add_footnote("Source : ES-PE 2017.") %>%
  add_footnote("Champ : Sur les 15 110 MNA de l'enquête, données pondérées.") %>%
  set_number_format(NA) %>% 
  set_caption("Mesures principales de placement des MNA présents et sortis placés à l'ASE")
```

```{r chisq, include=FALSE}
chisq_test(mna_ens$MES_rec, mna_ens$enq)
svychisq(~MES_rec + enq, mna_ensw)

require(GGally)
ggtable(data = na.omit(mna_ens),columnsX = "MES_rec",columnsY = "enq",cells = "col.prop",mapping = aes(weight = poids2),fill = "std.resid", legend = 1) + labs(fill = "Résidus\ndu Chi²") + coord_flip() + ggtitle("MNA")
```