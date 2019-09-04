#' Checking for logical consistency between two categorical variables in a synthesized data set.
#'
#' This function will check for logical consistency between two categorical variables in a fully or partially synthesized data set.
#'
#' When a data set is fully or partially synthesized from an observed data set, sometimes there are logical consistencies in the observed data set which must be adhered to in the synthesized data set that may be violated during the course of the synthesis.
#' For example, if there is a data set which contains an age variable and a variable that represents whether or not a person has a drivers license in the state of Pennsylvania, the age variable should indicate that the person is at least 16-years-old if the license indicator shows that the person has a drivers license.
#' It is recommended that you check for data comparability with \code{dataComp()} prior to using this function.
#'
#' This function creates cross-tabulations of the specified variables of both the observed data set and synthesized data set, then checks that the corresponding cell values are either zero or a positive value accordingly.  It was developed with the intention of making the job of researching synthetic data utility a bit easier by quickly checking for logical consistency.
#' @param obs_data The original data set to which the next will be compared, of the type "data.frame".
#' @param new_data The fully or partially synthetic data set to be compared to the observed data, of the type "data.frame".
#' @param vars A vector of two categorical variables in the data sets to check for logical consistency.
#' @param NAopt Defaults to TRUE to use NAs in tables.  If you do not wish to check for NAs, put FALSE.
#' @return This function returns a message stating whether or not there were any potential logical inconsistencies found in the data sets for the variables specified.  Then the cross-tabulations will be printed (in either case) for the analyst to review.
#' @return This function will also return a list of the following components:
#' @return \item{consistent}{A logical value indicating whether the variable cross-tabulation is logically consistent.}
#' @return \item{obs.table}{The original data set cross-tabulation.}
#' @return \item{new.table}{The new data set cross-tabulation.}
#' @return \item{which}{A matrix indicating if values are logically consistent. 0=consistent, otherwise=inconsistent.}
#' @keywords logic logical consistent consistency synthetic
#' @export
#' @importFrom magrittr %>%
#' @examples
#' #PPA is observed data set, PPAps2 is a partially synthetic data set derived from the observed data.
#' #age17plus and marriage are two categorical variables within these data sets.
#'
#' logicCheck(PPA, PPAps2, c("age17plus", "marriage"))

logicCheck <- function(obs_data, new_data, vars, NAopt=T){
  if(NAopt==T){
    if((dim(table(obs_data[[vars[1]]], useNA="ifany")) != dim(table(new_data[[vars[1]]], useNA="ifany"))) |
                    dim(table(obs_data[[vars[2]]], useNA="ifany")) != dim(table(new_data[[vars[2]]], useNA="ifany"))){
    stop("Variables have unequal factor levels. Check NAs and other levels.")
  }
    t1 <- table(obs_data[[vars[1]]], obs_data[[vars[2]]], useNA="ifany")
    t2 <- table(new_data[[vars[1]]], new_data[[vars[2]]], useNA="ifany")
  }
  else{
    if((dim(table(obs_data[[vars[1]]])) != dim(table(new_data[[vars[1]]]))) |
       dim(table(obs_data[[vars[2]]])) != dim(table(new_data[[vars[2]]]))){
      stop("Variables have unequal factor levels. Consider NAopt==T or check levels.")
    }
    t1 <- table(obs_data[[vars[1]]], obs_data[[vars[2]]])
    t2 <- table(new_data[[vars[1]]], new_data[[vars[2]]])
  }
  which_cells <- I(t1==0) - I(t2==0)
  comp <- sum(which_cells)
  if(comp==0){cat(paste(vars[1], "and", vars[2], "OK"), fill=2)
    paste("Observed data:") %>% cat(fill=2); t1 %>% print
    paste("New data:") %>% cat(fill=2); t2 %>% print
    consist=TRUE}
  else{
    paste("logicCheck found potential logical inconsistencies in the following places:") %>% cat(fill=2)
    paste(vars[1], "x", vars[2], ":") %>% cat(fill=2)
    which_cells %>% print
    paste("Observed data:") %>% cat(fill=2); t1 %>% print
    paste("New data:") %>% cat(fill=2); t2 %>% print
    consist=FALSE}
  list(consistent=consist, obs.table=t1, new.table=t2, which=which_cells)
}

