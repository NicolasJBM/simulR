#' Information about transfers of resources ensuring a capacity of production.
#'
#' @format Information about transfers of resources ensuring a capacity of production
#' \itemize{
#'   \item case: id of the case.
#'   \item origin: account number from which the resource comes.
#'   \item origin_label: label of the account from which the resource comes.
#'   \item destination: account number to which the resource goes.
#'   \item destination_label: label of the account to which the resource goes.
#'   \item nature: type of transaction, i.e. "revenues", "costs", "materials, "services", "prepaid" , "employment", "commissions", "investment", "debt" or "equity".
#'   \item purpose: purpose of the transaction, i.e. "production", "advertising", "selling", "administration" or "financing".
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
