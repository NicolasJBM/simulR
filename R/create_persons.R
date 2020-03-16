#' Prepare figures for small exercises about the high-low method of cost estimation.
#' @param headcount Numeric. Number of persons to generate.
#' @return A dataframe with gender, ethnicity, first_name, last_name, pronoun, possessive and title.
#' @importFrom randomNames randomNames
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @export


create_persons <- function(headcount = 2){
  
  persons <- randomNames::randomNames(n = headcount, return.complete.data = TRUE) %>%
    dplyr::mutate(
      pronoun = dplyr::case_when(gender == 0 ~ "he", TRUE ~ "she"),
      possessive = dplyr::case_when(gender == 0 ~ "his", TRUE ~ "her"),
      title = dplyr::case_when(gender == 0 ~ "Mr.", TRUE ~ "Mrs.")
    )
  
  return(persons)
}
