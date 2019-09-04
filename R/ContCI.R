#' Confidence intervals and standard errors of multiple imputation for a specific imputed continuous variable.
#'
#' This function will calculate confidence intervals and standard errors from the responses of multiple imputed datasets for a specified continuous variable, and also give a YES/NO indicator for whether or not the observed value is within the confidence interval.
#' The confidence intervals and standard errors are calculated by first taking the means of the variable from the partially synthesized datasets, then using \code{t.test()} to get the confidence intervals.
#'
#' This function was developed with the intention of making the job of researching partially synthetic data utility a bit easier by providing another way of measuring utility.
#' @param obs_data The original dataset to which the next will be compared, of the type "data.frame".
#' @param imp_data_list A list composed of \code{m} synthetic data sets.
#' @param var The continuous variable being checked.
#' @param sig The number of significant digits in the output data frame.  Defaults to 6.
#' @param alpha Test size, defaults to 0.05.
#' @return This function returns a data frame with the variable's observed mean, lower and upper limits of the confidence interval, standard error, and a YES/NO indicating whether or not the observed value is within the confidence interval.
#' @keywords synthetic synth synds utility multiple imputation
#' @export
#' @importFrom stats t.test
#' @importFrom magrittr %>%
#' @examples
#' #"PPA" is the observed data set
#' #"PPAm5" is a list of 5 partially synthetic data sets derived from PPA
#' #"age" is a continuous variable present in the synthesized data sets.
#' #3 significant digits are desired from the output data frame.
#'
#' ContCI(PPA, PPAm5, "age", sig=3)

ContCI <- function(obs_data, imp_data_list, var, sig=6, alpha=0.05){
  m <- length(imp_data_list)

  mat <- matrix(rep(NA, 5*length(var)))

  #Get means of the variable for each partsynth dataset (q_i)
  q_i <- rep(NA, m)
  for(i in 1:m){
    q_i[i] <- mean(imp_data_list[[i]][[var]], na.rm=T)
  }

  #Get observed means of the variable
  obs_mean <- mean(obs_data[[var]], na.rm=T)

  #Get confidence interval for means
  ci <- t.test(q_i, conf.level=1-alpha)
  lower <- ci$conf.int[1]
  upper <- ci$conf.int[2]
  SE <- (ci$conf.int[2] - ci$conf.int[1])/2
  if(obs_mean >= ci$conf.int[1] & obs_mean <= ci$conf.int[2]){ind <- "YES"}
  else{ind <- "NO"}
  values <- data.frame("Variable"=var, "Observed Mean" =obs_mean %>% round(sig), "Lower"=lower %>% round(sig),
                       "Upper"=upper %>% round(sig), "SE"=SE %>% round(sig), "In CI?"=ind)
  values
}
