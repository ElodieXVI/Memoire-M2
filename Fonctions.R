# Fonctions ####---------

## Pour ajouter les % aprè les étiquettes --------
add_pct <- function(var) {
  require(questionr)
  pct <- freq(var)[,c(2)]
  var_r <- paste0(levels(var),", ", pct, "%")
  levels(var) <- var_r
  return(var)
}

## Pour ajouter les n aprè les étiquettes --------
add_n <- function(var) {
  require(questionr)
  n <- freq(var)[,c(1)]
  l <- which.max(n)
  var_r <- paste0(levels(var),", n =", n[-l])
  levels(var) <- var_r
  return(var)
}


## Pour présentation des modèles multinomniales avec gtsummary --------
multinom_pivot_wider <- function(x) {
  # check inputs match expectatations
  if (!inherits(x, "tbl_regression") || !inherits(x$model_obj, "multinom")) {
    stop("`x=` must be class 'tbl_regression' summary of a `nnet::multinom()` model.")
  }
  
  # create tibble of results
  df <- tibble::tibble(outcome_level = unique(x$table_body$groupname_col))
  df$tbl <- 
    purrr::map(
      df$outcome_level,
      function(lvl) {
        gtsummary::modify_table_body(
          x, 
          ~dplyr::filter(.x, .data$groupname_col %in% lvl) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data$groupname_col)
        )
      }
    )
  
  tbl_merge(df$tbl, tab_spanner = paste0("**", df$outcome_level, "**"))
}


## Pour pourcentages sur certains graphiques ggplot -------------
f <- function(x) {
  res <- scales::percent(x, accuracy = 1)
  res[x < .05] <- scales::percent(x[x < .05], accuracy = 1, suffix = "")
  res[x < .01] <- ""
  res
}