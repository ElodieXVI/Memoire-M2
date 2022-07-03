library(pacman)
p_load(tidyverse, magrittr, pder, plm)

## Recodage de modele$grp2 en modele$autonom
modele$autonom <- modele$grp2 %>%
  fct_recode(
    "0" = "1",
    "0" = "2",
    "1" = "3")


mod2 <- plm(autonom ~ lag(SEX)  + lag(MNA) + lag(HAND), data = modele,
            model = "within")
