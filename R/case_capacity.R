#' Information about transfers of resources ensuring a capacity of production.
#'
#' @format Information about transfers of resources ensuring a capacity of production
#' \itemize{
#'   \item case: id of the case.
#'   \item nature: account number from which the resource comes.
#'   \item nature_label: label of the account from which the resource comes.
#'   \item destination: account number to which the resource goes.
#'   \item destination_label: label of the account to which the resource goes.
#'   \item contract: type of contract governing the transfer of resource: "sale", "purchase" , "employment", or "investment".
#'   \item resource: type of resource transferred.
#'   \item parameter: kind of information provided about the resource transfer.
#'   \item unit: unit of the parameter.
#'   \item initialization: value of the parameter in the initialization period.
#'   \item first_period: value of the parameter in the first period of operations.
#' }
#' @docType data
#' @keywords datasets
#' @name case_capacity
#' @usage data("case_capacity")
"case_capacity"
