#' Confidence intervals and standard errors of multiple imputation for the cross-tabulation of two categorical variables.
#'
#' This function will calculate confidence intervals and standard errors from the proportional tabular responses of multiple imputed datasets for two categorical variables, and also give a YES/NO indicator for whether or not the observed value is within the confidence interval.
#' The confidence intervals and standard errors are calculated from formulas that are adapted for partially synthetic data sets.  See reference for more information.
#'
#' This function was developed with the intention of making the job of researching partially synthetic data utility a bit easier by providing another way of measuring utility.
#' @param obs_data The original dataset to which the next will be compared, of the type "data.frame".
#' @param imp_data_list A list composed of \code{m} synthetic data sets.
#' @param type Specifies which type of datasets are in \code{imp_data_list}.  Options are "synthetic" and "imputed".
#' @param vars A vector of the two categorical variable being checked.  Should be of type "factor".
#' @param sig The number of significant digits in the output dataframes.  Defaults to 4.
#' @param alpha Test size, defaults to 0.05.
#' @return This function returns a list of five data frames:
#' @return \item{Observed}{A cross-tabular proportion of observed values}
#' @return \item{Lower}{Lower limit of the confidence interval}
#' @return \item{Upper}{Upper limit of the confidence interval}
#' @return \item{SEs}{Standard Errors}
#' @return \item{CI_Indicator}{"YES"/"NO" indicating whether or not the observed value is within the confidence interval}
#' @keywords synthetic synth synds utility multiple imputation
#' @export
#' @importFrom Rdpack reprompt
#' @importFrom magrittr %>% divide_by
#' @importFrom stats var qt
#' @references \insertRef{adapt}{SynthTools}
#' @examples
#' #PPA is the observed data set.  PPAm5 is a list of 5 partially synthetic data sets derived from PPA.
#' #"sex" and "race" are categorical variables present in the synthesized data sets.
#' #3 significant digits are desired in the output dataframes.
#'
#' twoCatCI(PPA, PPAm5, "synthetic", c("sex", "race"), sig=3)

twoCatCI <- function(obs_data, imp_data_list, type, vars, sig=4, alpha=0.05){
  #Create proportion tables for inference variables
  m <- length(imp_data_list)
  VAR1 <- vars[1]
  VAR2 <- vars[2]
  props <- list()
  for(i in 1:m){
    props[[i]] <- table(imp_data_list[[i]][[VAR1]], imp_data_list[[i]][[VAR2]], useNA="ifany") %>% prop.table
  }

  #Create NA matrices with identical dimensions for SEs and CIs
  se_mat <- matrix(rep(NA, (dim(props[[1]])[1] * dim(props[[1]])[2])), nrow=dim(props[[1]])[1])
  CIind_mat <- matrix(rep(NA, (dim(props[[1]])[1] * dim(props[[1]])[2])), nrow=dim(props[[1]])[1])
  lower_mat <- matrix(rep(NA, (dim(props[[1]])[1] * dim(props[[1]])[2])), nrow=dim(props[[1]])[1])
  upper_mat <- matrix(rep(NA, (dim(props[[1]])[1] * dim(props[[1]])[2])), nrow=dim(props[[1]])[1])

  #Get observed proportions
  obs_mat <- table(obs_data[[VAR1]], obs_data[[VAR2]], useNA="ifany") %>% prop.table

  #Write SE function
  SE_func <- function(vals_vec) {
    v_bar <- sapply(vals_vec, function(x){x*(1-x)}) %>% sum %>% divide_by(nrow(obs_data))
    B <- var(vals_vec)
    if(type=="synthetic"){SE <- sqrt(v_bar + B/m)}
    else if(type=="imputed"){SE <- sqrt((1 + 1/m)*B)}
    list(v_bar=v_bar, B=B, SE=SE)
  }

  #Use SE function on values in datasets
  for(j in 1:ncol(se_mat)){
    for(i in 1:nrow(se_mat)){
      vals <- rep(NA, m)
      for(g in 1:m){vals[g] <- props[[g]][i,j]}
      se_output <- SE_func(vals)
      v_bar <- se_output$v_bar
      B <- se_output$B
      SE <- se_output$SE
      se_mat[i,j] <- SE
      df <- (m-1) * (1+v_bar/ ((1 + 1/m)*B))^2

      p_bar <- mean(vals)
      lower_mat[i,j] <- lower <- p_bar - qt(1 - alpha/2, df=df)*se_mat[i,j]
      upper_mat[i,j] <- upper <- p_bar + qt(1 - alpha/2, df=df)*se_mat[i,j]
      if(obs_mat[i,j] >= lower & obs_mat[i,j] <= upper) {CIind_mat[i,j] <- "YES"}
      else {CIind_mat[i,j] <- "NO"}
    }
  }

  rownames(se_mat) <- rownames(obs_mat)
  colnames(se_mat) <- colnames(obs_mat)
  rownames(CIind_mat) <- rownames(obs_mat)
  colnames(CIind_mat) <- colnames(obs_mat)
  rownames(lower_mat) <- rownames(obs_mat)
  colnames(lower_mat) <- colnames(obs_mat)
  rownames(upper_mat) <- rownames(obs_mat)
  colnames(upper_mat) <- colnames(obs_mat)
  tab_list <- list(obs_mat %>% round(sig), lower_mat %>% round(sig), upper_mat %>% round(sig),
                   se_mat %>% round(sig), CIind_mat)
  names(tab_list) <- c("Observed", "Lower", "Upper", "SEs", "CI_Indicator")
  tab_list
}
