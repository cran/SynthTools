#' Confidence intervals and standard errors of multiple imputation for a specific imputed categorical variable.
#'
#' This function will calculate confidence intervals and standard errors from the proportional responses of multiple imputed datasets for a specified categorical variable, and also gives a YES/NO indicator for whether or not the observed value is within the confidence interval.
#' The confidence intervals and standard errors are calculated from variance formulas that are specific to whether the multiple imputed datasets are synthetic or imputed.  See reference for more information.
#'
#' This function was developed with the intention of making the job of researching imputed and synthetic data utility a bit easier by providing another way of measuring utility.
#' @param obs_data The original dataset to which the next will be compared, of the type "data.frame".
#' @param imp_data_list A list of datasets that are either synthetic or contain imputed values.
#' @param type Specifies which type of datasets are in \code{imp_data_list}.  Options are "synthetic" and "imputed".
#' @param var The categorical variable being checked.  Should be of type "factor".
#' @param sig The number of significant digits in the output dataframe.  Defaults to 6.
#' @param alpha Test size, defaults to 0.05.
#' @return This function returns a dataframe with the variable's responses, observed values, lower and upper limits of the confidence interval, standard error, and "YES"/"NO" indicating whether or not the observed value is within the confidence interval.
#' @keywords synthetic synth synds utility multiple imputation
#' @export
#' @importFrom Rdpack reprompt
#' @importFrom magrittr %>% multiply_by divide_by
#' @importFrom stats qt
#' @references \insertRef{adapt}{SynthTools}
#' @examples
#' #PPA is observed data set, PPAm5 is a list of 5 partially synthetic data sets derived from PPA.
#' #sex is a categorical variable within these data sets. 3 significant digits are desired.
#'
#' oneCatCI(obs_data=PPA, imp_data_list=PPAm5, type="synthetic", var="sex", sig=3)

oneCatCI <- function(obs_data, imp_data_list, type, var, sig=6, alpha=0.05){

  m <- length(imp_data_list)

  #Create matrix of NAs for table values
  if(any(is.na(obs_data[[var]])))
  {len <- (levels(obs_data[[var]]) %>% length)+1
  COLnames <- c(levels(obs_data[[var]]), "NA")}
  else{len <- levels(obs_data[[var]]) %>% length
  COLnames <- levels(obs_data[[var]])}
  mat <- matrix(rep(NA, m*len), nrow=m)
  colnames(mat) <- COLnames

  #Populate matrix
  for(i in 1:m){
    mat[i,1:len] <- table(imp_data_list[[i]][[var]], useNA="ifany") %>% prop.table
  }

  #Get p_bars
  p_bar <- apply(mat, 2, mean)

  #Get v_bars
  v_bar <- NULL
  for(i in 1:len){
    v_bar[i] <- sapply(mat[,i], function(x){x*(1-x)}) %>% sum %>% divide_by(nrow(obs_data))
  }

  #Get B's
  B <- NULL
  for(i in 1:len){
    B[i] <- var(mat[,i])
  }

  #Make matrix to fill with confidence intervals
  ci_mat <- matrix(rep(NA, 5*len), nrow=len)
  row.names(ci_mat) <- colnames(mat)
  colnames(ci_mat) <- c("Obs", "Lower", "Upper", "SE", "In CI?")

  #Make proportion table for observed data
  obs_prop <- table(obs_data[[var]], useNA="ifany") %>% prop.table

  #Make confidence intervals based on proportion values
  for(i in 1:len){
    ci_mat[i,1] <- obs_prop[[i]]
    if(type=="synthetic"){SE <- sqrt(v_bar[i] + B[i]/m)}
    else if(type=="imputed"){SE <- sqrt((1 + 1/m)*B[i])}
    else{stop(paste(type, "is not a valid dataset type."))}

    df <- (m-1) * (1+v_bar[i] / ((1 + 1/m)*B[i]))^2
    ci_mat[i,2] <- p_bar[i] - qt(1 - alpha/2, df=df)*SE
    ci_mat[i,3] <- p_bar[i] + qt(1 - alpha/2, df=df)*SE
    ci_mat[i,4] <- SE
    if(obs_prop[[i]] >= ci_mat[i,2] & obs_prop[[i]] <= ci_mat[i,3]){ci_mat[i,5] <- "YES"}
    else{ci_mat[i,5] <- "NO"}
  }

  rn <- row.names(ci_mat)
  ci_mat <- data.frame(Response=rn, ci_mat, row.names=NULL)
  ci_mat$Obs <- as.numeric(levels(ci_mat$Obs))[ci_mat$Obs] %>% round(sig)
  ci_mat$Lower <- as.numeric(levels(ci_mat$Lower))[ci_mat$Lower] %>% round(sig)
  ci_mat$Upper <- as.numeric(levels(ci_mat$Upper))[ci_mat$Upper] %>% round(sig)
  ci_mat$SE <- as.numeric(levels(ci_mat$SE))[ci_mat$SE] %>% round(sig)

  #Output
  paste0((1-alpha)*100, "% Confidence Intervals and Standard Errors of Multiple Imputation for ", var) %>% cat(fill=2)
  ci_mat %>% print(row.names=FALSE)
}
