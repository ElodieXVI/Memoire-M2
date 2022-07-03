
table(enf$HEBE)
table(enf$HEBE_rec3)

act$collect <- case_when(act$COLCAPA > 0 ~ "1",
                     act$POUCAPA > 0 ~ "1",
                     T ~ "0")

act$autonomautre <- case_when(act$ECLCAPA > 0 ~ "1",
                              act$AUTCAPA > 0 ~ "1",
                     T ~ "0")
act$assfam <- case_when(act$ASFCAPA > 0 ~ "1",
                     act$LVACAPA > 0 ~ "1",
                     T ~ "0")

act$domicile <- case_when(act$PADCAPA > 0 ~ "1",
                    T ~ "0")
table(act$collect)
table(act$autonomautre)
table(act$assfam)
table(act$domicile)

act$hebe_capa <- case_when(act$collect == 1 & act$autonomautre == 0 & act$assfam == 0 & act$domicile == 0 ~ "collectif",
                      act$collect == 0 & act$autonomautre == 0 & act$assfam == 0 & act$domicile == 1 ~ "domicile",
                      act$collect == 0 & act$autonomautre == 0 & act$assfam == 1 & act$domicile == 0 ~ "assfam",
                      act$collect == 0 & act$autonomautre == 1 & act$assfam == 0 & act$domicile == 0 ~ "autonom",
                      act$collect == 1 & act$autonomautre == 1 & act$assfam == 0 & act$domicile == 0 ~ "collectif et autonome",
                      act$collect == 0 & act$autonomautre == 0 & act$assfam == 1 & act$domicile == 1 ~ "assfam et domicile",
                      act$collect == 1 & act$autonomautre == 0 & act$assfam == 0 & act$domicile == 1 ~ "autonom et assfam",
                      act$collect == 0 & act$autonomautre == 0 & act$assfam == 1 & act$domicile == 1 ~ "collectif et domicile",
                      act$collect == 1 & act$autonomautre == 0 & act$assfam == 1 & act$domicile == 0 ~ "assfam et collectif",
                      act$collect == 1 & act$autonomautre == 1 & act$assfam == 1 & act$domicile == 0 ~ "collectif, autonom et assfam",
                      act$collect == 0 & act$autonomautre == 1 & act$assfam == 1 & act$domicile == 1 ~ "autonom, assfam et domicile",
                      act$collect == 1 & act$autonomautre == 0 & act$assfam == 1 & act$domicile == 1 ~ "assfam, domicile et collectif", 
                      T ~ "tout")

