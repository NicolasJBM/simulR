#' Create a list of companies based on names and the initial template.
#' @param company_names Character vector. names of companies.
#' @param base_company  List. base_company as returned by the function "create_case".
#' @return list of templates for each company.
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr everything
#' @export




create_competition <- function(company_names, base_company){
  
  competition <- list()
  
  for (company in company_names){
    
    base <- base_company
    
    for (i in 1:5){
      base[[i]] <- dplyr::select(dplyr::mutate(base[[i]], company = company), company, dplyr::everything())
    }
    
    for (i in 1:length(base$census)){
      base$census[[i]] <- dplyr::select(dplyr::mutate(base$census[[i]], company = company), company, dplyr::everything())
    }
    
    for (i in 1:length(base$costing)){
      base$costing[[i]] <- dplyr::select(dplyr::mutate(base$costing[[i]], company = company), company, dplyr::everything())
    }
    
    competition[[company]] <- base
  }
  
  return(competition)
}
