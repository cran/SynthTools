#' Checking for equality in the features of two data sets.
#'
#' This function will check for comparability between two data sets, including dimensions, order of variables, variable classifications, and levels of factors.
#' When a data set is fully or partially synthesized from an observed data set, these are the features that should be equal between the data sets so the utility of the synthetic data can be measured.
#'
#' This function was developed with the intention of making the job of researching synthetic data utility a bit easier by making preliminary data set comparisons quickly.
#' @param obs_data The original data set to which the next will be compared, of the type "data.frame".
#' @param new_data The fully or partially synthetic data set to be compared to the observed data, of the type "data.frame".
#' @return A list containing the following components:
#' @return \item{same.dim}{A logical value indicating whether or not \code{obs_data} and \code{new_data} have the same dimensions.}
#' @return \item{same.order}{A logical value indicating whether or not the variables in \code{obs_data} and \code{new_data} are in the same order.}
#' @return \item{class.identical}{A logical value indicating where or not the variable classifications are identical.}
#' @return \item{class.table}{A table of types of variable classifications.}
#' @return \item{fac.num.same}{A logical value indicating whether or not the factors in the data sets have the same number of levels.}
#' @return \item{fac.lev.same}{A logical value indicating whether or not the factors in the data sets have the same levels.}
#' @keywords data set comparison synthetic
#' @export
#' @importFrom magrittr %>%
#' @examples
#' #PPA is observed data set, PPAps1 is a partially synthetic data set derived from the observed data.
#'
#' dataComp(PPA, PPAps1)

dataComp <- function(obs_data, new_data){

  #Check for same dimensions
  if(mean(dim(new_data)==dim(obs_data)) !=1){
    same_dim=FALSE; same_order=NA; class_identical=NA; cnd=NA; fac_num_same=NA; fac_lev_same=NA
    paste("STOP:  data sets are different dimensions.",
          paste0("Observed data: ", dim(obs_data)[1], " rows, ", dim(obs_data)[2], " columns"),
          paste0("Synthesized data: ", dim(new_data)[1], " rows, ", dim(new_data)[2], " columns"), sep="\n") %>% cat(fill=2)
  }
  else{same_dim=TRUE
    "Observed and synthesized data sets have the same dimensions:  YES" %>% cat(fill=2)

    #Check for same variable order
    if(mean(names(obs_data)==names(new_data))!=1){
      same_order=FALSE
      class_identical=NA; cnd=NA; fac_num_same=NA; fac_lev_same=NA
      paste("STOP: Variables are NOT in the same order. Reorder variables and try again.") %>% cat(fill=2)
    }
    else{same_order=TRUE
        "Variable order the same:  YES" %>% cat(fill=2)

      #Check correct classifications
      class_obs <- rep(NA, length(obs_data))
      class_new_data <- rep(NA, length(new_data))
      for(i in 1:length(obs_data)){class_obs[i] <- class(obs_data[[i]])}
      for(i in 1:length(new_data)){class_new_data[i] <- class(new_data[[i]])}

      if(mean(class_obs == class_new_data)!=1){
        "Variable classification identical:  NO" %>% cat(fill=2)
        classes <- data.frame(names(obs_data), class_obs, class_new_data)
        diff_class <- classes[class_obs != class_new_data,]
        names(diff_class) <- c("Variable: ", "Observed class: ", "New class: ")
        diff_class %>% print
        paste("STOP: Correct variable classifications and try again.") %>% cat(fill=2)
        class_identical=FALSE
        cnd=NA; fac_num_same=NA; fac_lev_same=NA
      }
      else{
        class_identical=TRUE
        "Variable classification identical:  YES" %>% cat(fill=2)
        cnd <- table(class_new_data)
        for(i in 1:length(cnd)){
          paste0(names(cnd)[i], " variables: ", cnd[i]) %>% cat(fill=2)
        }
        #Check number of factor levels
        faclen_obs <- length(obs_data[class_obs=="factor"])
        faclen_new_data <- length(new_data[class_new_data=="factor"])

        if(faclen_obs==0 | faclen_new_data==0){
          fac_num_same=NA; fac_lev_same=NA
          paste("WARNING: There are no variables of the factor classification to check for",
                "proper factor levels. Recommend changing categorical variables to factors.", sep="\n") %>% cat(fill=2)
        }
        else{level_obs <- rep(NA, faclen_obs)
        level_new_data <- rep(NA, faclen_new_data)
        facind <- which(class_obs=="factor")
        facs_obs <- names(obs_data)[facind]
        for(i in 1:faclen_obs){
          level_obs[i] <- levels(obs_data[[facs_obs[i]]]) %>% length
          if(any(obs_data[[facs_obs[i]]] %>% is.na)){
            level_obs[i] <- level_obs[i]+1}
        }
        for(i in 1:faclen_new_data){
          level_new_data[i] <- levels(new_data[[facs_obs[i]]]) %>% length
          if(any(new_data[[facs_obs[i]]] %>% is.na)){
            level_new_data[i] <- level_new_data[i]+1}
        }
        meanlevel <- mean(level_obs==level_new_data)

        if(meanlevel!=1){
          fac_num_same=FALSE; fac_lev_same=NA
          "Number of factor levels are the same:  NO" %>% cat(fill=2)
          difflev <- which(level_obs != level_new_data)
          paste("Factor with different levels:", (facs_obs)[difflev]) %>% cat(fill=2)
        }
        else{fac_num_same=TRUE
        "Number of factor levels are the same:  YES" %>% cat(fill=2)

        #Check that all levels are the same
        levelsame <- rep(0, length(faclen_obs))
        for(i in 1:length(facs_obs)){
          levelsame[i] <- mean(levels(obs_data[[facs_obs[i]]])==levels(new_data[[facs_obs[i]]]))
        }
        if(mean(levelsame)!=1){
          fac_lev_same=FALSE
          "All factor levels are identical:  NO" %>% cat(fill=2)
          diff_levels <- which(levelsame != 1)
          diff_names <- names(obs_data[class_obs=="factor"])[diff_levels] %>% as.vector
          for(i in 1:length(diff_names)){paste0("Factor with different levels: ", diff_names[i]) %>% cat(fill=2)}
        }
        else{fac_lev_same=TRUE
        "All factor levels are identical:  YES" %>% cat(fill=2)
        }

        }
        }
      }
    }
  }

  list(same.dim=same_dim, same.order=same_order, class.identical=class_identical, class.table=cnd,
       fac.num.same=fac_num_same, fac.lev.same=fac_lev_same)
}
