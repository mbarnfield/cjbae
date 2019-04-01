#' @rdname cjbae_formula
#' @title Conjoint formula conversion for bayesian analysis
#' @description Convert standard tidy conjoint function (data, formula, id) input into the input for a `brmsformula()`.
#' @param data A tidy, long-format conjoint dataframe.
#' @param formula A formula specifying an AMCE/MM model, to be converted for bayesian analysis. All variables should be factors.
#' @param id A variable, within data, containing respondent IDs.
#' @details \code{cjbae_formula} takes three arguments: `data`, `formula`, and `id`, and uses these to specify a formula to be passed to `brmsformula()`, to facilitate bayesian analysis of conjoint experiments otherwise analysed using `cregg`.
#' @export
#' @examples 
#' #load example data from cregg
#' library(cregg)
#' data(immigration)
#' #with simple input formula
#' cjbae_formula(data = immigration, formula = ChosenImmigrant ~ Gender + Job, id = CaseID)
#' #or matched positionally
#' cjbae_formula(immigration, ChosenImmigrant ~ Gender + Job, CaseID)
#' #can pre-specify longer input formula
#' f1 <- ChosenImmigrant ~ Gender + Education + LanguageSkills + 
#' CountryOfOrigin + Job + JobExperience + JobPlans + 
#' ReasonForApplication + PriorEntry
#' cjbae_formula(immigration, f1, CaseID)

cjbae_formula <-
  function(data, formula, id) {
    
    library(tidyverse)
    
    id_enq <- enquo(id)
    
    #find lhs and rhs vars
    outcome <- all.vars(stats::update(formula, . ~ 0))
    if (!length(outcome) || outcome == ".") {
      stop("'formula' is missing a left-hand outcome variable")
    }
    predictors <- all.vars(stats::update(formula, 0 ~ .))
    
    #collapse predictors in rhs string
    predictors_string <- paste(predictors, collapse = " + ")  
    
    #get id variable from data
    id <- data %>%
      dplyr::select(!!id_enq)
    
    #create
    bae_formula <- paste0(outcome,
                          " ~ 1 + ",
                          predictors_string,
                          " + (1 | ",
                          colnames(id),
                          ")")
    
    # Output as formula
    formula(bae_formula)
  }
