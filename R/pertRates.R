#' Calculates perturbation rates of overall data set and specific variables.
#'
#' This function will calculate the overall perturbation rate of an imputed data set and for specific variables requested.
#'
#' A record in a data set is considered "perturbed" when at least one value in the record is different from the observed data.  The overall perturbation rate is therefore the number of records that are found to be perturbed over the number of records in a data set.
#'
#' The variable perturbation rate is simply the rate at which the values for a given variable are different from those in the observed data set.
#'
#' This function was developed with the intention of making the job of researching synthetic data utility a bit easier by quickly calculating perturbation rates.
#' @param obs_data The original dataset to which the next will be compared, of the type "data.frame".
#' @param new_data The fully or partially synthetic data set to be compared to the observed data, of the type "data.frame".
#' @param imp_vars A vector of variables which were imputed and are to be used in the overall perturbation rate calculation.
#' @param desc Whether or not the variable perturbation rates should be output in descending rate order.  Defaults to FALSE.
#' @param sig The number of significant digits desired for the overall perturbation rate.  Defaults to 4.
#' @return Returns the overall perturbation rate of the synthetic data set and the specific variable perturbation rates in percentages, rounded to 0.1.  The function will also output in list format with the following components:
#' @return \item{overall}{The overall perturbation rate.}
#' @return \item{variable}{A vector of variable perturbation rates.}
#' @keywords perturbation imputation synthetic
#' @export
#' @importFrom magrittr %>% multiply_by divide_by
#' @examples
#' #PPA is observed data set, PPAps2 is a partially synthetic data set derived from the observed data.
#' #age17plus, marriage, and vet are three categorical variables within these data sets.
#'
#' pertRates(PPA, PPAps2, c("age17plus", "marriage", "vet"))

pertRates <- function(obs_data, new_data, imp_vars, desc=FALSE, sig=4){
  #Overall perturbation rate
  iv_inds <- which(names(obs_data) %in% imp_vars)
  pertb1 <- data.frame(obs_data != new_data)[,iv_inds]
  s1 <- pertb1 %>% apply(1, sum, na.rm = T) %>%  table
  pert_rate1 <- (1 - s1[1]/nrow(obs_data))[[1]] %>% round(sig)
  paste("Overall perturbation rate:", pert_rate1) %>% cat(fill=2)

  #imp_vars perturbation rate
  pertb = data.frame(obs_data != new_data) %>% dplyr::select(imp_vars)
  s2 = pertb %>% apply(2, sum, na.rm = T) %>% multiply_by(100) %>% divide_by(nrow(obs_data)) %>% round(1)
  if(desc==TRUE){
    s2 <- s2[order(-s2)]
    paste("Variable perturbation rates:") %>% cat(fill=2)
    s2 %>% print}
  else{paste("Variable perturbation rates:") %>% cat(fill=2); s2 %>% print}

  list(overall=pert_rate1, variable=s2)
}


